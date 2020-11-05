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
  , TaggedRequest
  , TaggedResponse
  , mkTaggedResponse
  , WebSocketEndpoint
  ) where

import Data.Constraint.Extras
import Data.Constraint.Forall
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

performWebSocketRequests
  :: forall req js t m.
     ( Prerender js t m, Applicative m
     , FromJSON (Some req)
     , forall a. ToJSON (req a)
     , ForallF ToJSON req
     , Has FromJSON req
     )
  => WebSocketEndpoint
  -> Event t (RequesterData req)
  -> m (Event t (RequesterData (Either Text)))
performWebSocketRequests url req = fmap switchPromptlyDyn $ prerender (pure never) $ do
  -- TODO: Pull the actual websocket out of here so that the GadtApi stuff can
  -- exist on one channel of a multi-channel connection
  rec (matchedReq, matchedRsp) <- matchResponsesWithRequests enc req $
        ffor rsp $ \(TaggedResponse t v) -> (t, v)
      let wireReq = fmap (Map.elems . Map.mapMaybeWithKey (\t v -> case fromJSON v of
            Success (r :: Value) -> Just $ TaggedRequest t r
            _ -> Nothing)) matchedReq
      w <- webSocket url $ def
        { _webSocketConfig_send = fmap encode <$> wireReq
        }
      let rsp = fmapMaybe decodeStrict $ _webSocket_recv w
  pure matchedRsp
  where
    enc :: forall a. req a -> (Value, Value -> Either Text a)
    enc r =  
      ( whichever @ToJSON @req @a $ toJSON r
      , \x -> case has @FromJSON r $ fromJSON x of
        Success s-> Right s
        Error e -> Left $ T.pack e
      )

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
