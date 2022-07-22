{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Reflex.Dom.GadtApi.WebSocket
  ( performWebSocketRequests
  , TaggedRequest (..)
  , TaggedResponse (..)
  , mkTaggedResponse
  , WebSocketEndpoint
  , tagRequests
  ) where

import Control.Monad.Fix (MonadFix)
import Data.Constraint.Extras
import Data.Aeson
import Data.Default
import qualified Data.Text as T
import Data.Typeable
import GHC.Generics
import qualified Data.Map as Map
import Data.Some
import Data.Text (Text)

import Reflex
import Reflex.Dom.Core (Prerender, prerender)
import Reflex.Dom.WebSocket

-- | A request tagged with an identifier
data TaggedRequest = TaggedRequest Int Value
  deriving (Typeable, Generic)

instance FromJSON TaggedRequest
instance ToJSON TaggedRequest

-- | A response tagged with an identifier matching the one in the 'TaggedRequest'. The identifier is the first argument.
data TaggedResponse = TaggedResponse Int Value
  deriving (Typeable, Generic)

instance FromJSON TaggedResponse
instance ToJSON TaggedResponse

type WebSocketEndpoint = Text

-- | Opens a websockets connection, takes the output of a 'RequesterT' widget
-- and issues that output as API requests over the socket. The result of this
-- function can be fed back into the requester as responses. For example:
--
-- @
-- rec (appResult, requests) <- runRequesterT myApplication responses
--     responses <- performWebSocketRequests myEndpoint requests
-- @
--
performWebSocketRequests
  :: forall req t m.
     ( Prerender t m, Applicative m
     , FromJSON (Some req)
     , forall a. ToJSON (req a)
     , Has FromJSON req
     )
  => WebSocketEndpoint
  -> Event t (RequesterData req)
  -> m (Event t (RequesterData (Either Text)))
performWebSocketRequests url req = fmap switchPromptlyDyn $ prerender (pure never) $ do
  rec w <- webSocket url $ def
        { _webSocketConfig_send = fmap encode <$> reqs
        }
      let rsp = fmapMaybe decodeStrict $ _webSocket_recv w
      (reqs, rsps) <- tagRequests req rsp
  pure rsps

-- | Constructs a response for a given request, and handles the
-- decoding/encoding and tagging steps internal to TaggedRequest and
-- TaggedResponse.
mkTaggedResponse
  :: (Monad m, FromJSON (Some f), Has ToJSON f)
  => TaggedRequest
  -> (forall a. f a -> m a)
  -> m (Either String TaggedResponse)
mkTaggedResponse (TaggedRequest reqId v) f = case fromJSON v of
  Success (Some a) -> do
    rsp <- f a
    pure $ Right $ TaggedResponse reqId (has @ToJSON a $ toJSON rsp)
  Error err -> pure $ Left err

-- | This function transforms a request 'Event' into an 'Event' of
-- 'TaggedRequest's (the indexed wire format used to transmit requests). It
-- expects to receive an 'Event' of 'TaggedResponse', the corresponding
-- response wire format, which it will transform into an "untagged" response.
--
-- @
--   requests  --> |-------------| --> tagged requests
--     ↗           |             |                 ↘
-- Client          | tagRequests |                Server
--     ↖           |             |                 ↙
--   responses <-- |-------------| <-- tagged responses
-- @
--
-- This function is provided so that you can use a single websocket for
-- multiple purposes without reimplementing the functionality of
-- 'performWebSocketRequests'. For instance, you might have a websocket split
-- into two "channels," one for these tagged API requests and another for data
-- being pushed from the server.
--
tagRequests
  :: forall req t m.
     ( Applicative m
     , FromJSON (Some req)
     , forall a. ToJSON (req a)
     , Has FromJSON req
     , Monad m
     , MonadFix m
     , Reflex t
     , MonadHold t m
     )
  => Event t (RequesterData req)
  -> Event t TaggedResponse
  -> m ( Event t [TaggedRequest]
       , Event t (RequesterData (Either Text))
       )
tagRequests req rsp = do
  rec (matchedReq, matchedRsp) <- matchResponsesWithRequests enc req $
        ffor rsp $ \(TaggedResponse t v) -> (t, v)
      let wireReq = fmap (Map.elems . Map.mapMaybeWithKey (\t v -> case fromJSON v of
            Success (r :: Value) -> Just $ TaggedRequest t r
            _ -> Nothing)) matchedReq
  pure (wireReq, matchedRsp)
  where
    enc :: forall a. req a -> (Value, Value -> Either Text a)
    enc r =
      ( toJSON r
      , \x -> case has @FromJSON r $ fromJSON x of
        Success s-> Right s
        Error e -> Left $ T.pack e
      )
