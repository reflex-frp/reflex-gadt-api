{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Reflex.Dom.GadtApi.WebSocket
  ( performWebSocketRequests
  , WebSocketClientConfig (..)
  , WebSocketClientStatus (..)
  , TaggedRequest (..)
  , TaggedResponse (..)
  , mkTaggedResponse
  , WebSocketEndpoint
  , tagRequests
  ) where

import Control.Monad.Fix (MonadFix)
import Data.Constraint.Extras
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Default
import qualified Data.Set as Set
import GHC.Generics
import qualified Data.Map as Map
import Data.Some
import Data.Text (Text)

import Reflex
import Reflex.Dom.Core (Prerender, prerender)
import Reflex.Dom.WebSocket

import Reflex.Dom.GadtApi.Codec (encodeApiRequest, serveApiRequest)

-- | A request tagged with an identifier
data TaggedRequest = TaggedRequest Int Value
  deriving Generic

instance FromJSON TaggedRequest
instance ToJSON TaggedRequest

-- | A response tagged with an identifier. The identifier matches the one in the 'TaggedRequest' and is the first argument.
data TaggedResponse = TaggedResponse Int Value
  deriving Generic

instance FromJSON TaggedResponse
instance ToJSON TaggedResponse

type WebSocketEndpoint = Text

-- | Configuration for the websocket that 'performWebSocketRequests' manages.
-- These fields match the native 'Reflex.Dom.WebSocket.WebSocketConfig'
-- fields. The XHR transport has no matching fields. The two transports have
-- different capabilities.
data WebSocketClientConfig t = WebSocketClientConfig
  { _webSocketClientConfig_url :: WebSocketEndpoint
    -- ^ URL of the websocket
  , _webSocketClientConfig_reconnect :: Bool
    -- ^ Whether to connect again automatically after the connection closes.
    -- Requests sent while no connection is open stay in a queue. The queue
    -- is sent when a connection opens. Outstanding requests survive
    -- disconnects and reconnects. See 'WebSocketClientStatus'.
  , _webSocketClientConfig_protocols :: [Text]
    -- ^ Subprotocols to request when opening the connection
  , _webSocketClientConfig_close :: Event t (Word, Text)
    -- ^ Close the connection with this code and reason when this event
    -- fires
  }

-- | The default configuration uses the empty URL. It requests no
-- subprotocols and never closes the connection itself. It connects again
-- automatically. Set '_webSocketClientConfig_url' before you use it.
instance Reflex t => Default (WebSocketClientConfig t) where
  def = WebSocketClientConfig
    { _webSocketClientConfig_url = ""
    , _webSocketClientConfig_reconnect = True
    , _webSocketClientConfig_protocols = []
    , _webSocketClientConfig_close = never
    }

-- | Connection status and protocol diagnostics for the socket that
-- 'performWebSocketRequests' manages. The lifecycle events come from
-- 'Reflex.Dom.WebSocket.WebSocket':
--
-- * '_webSocketClientStatus_open' fires each time a connection opens. This
--   includes connections after a reconnect.
-- * '_webSocketClientStatus_error' fires on a connection error. A close
--   event always follows it.
-- * '_webSocketClientStatus_close' fires each time a connection closes. It
--   carries the @wasClean@ flag, the status code, and the reason.
--
-- Outstanding requests are /not/ failed when the connection closes. They
-- stay pending until a matching response arrives. Requests sent while no
-- connection is open stay in a queue. The queue is sent when a connection
-- opens. A queued request that changes data can be sent again after a
-- reconnect. Set '_webSocketClientConfig_reconnect' to 'False' if your
-- application must not send the same change twice.
data WebSocketClientStatus t = WebSocketClientStatus
  { _webSocketClientStatus_open :: Event t ()
    -- ^ A connection opened
  , _webSocketClientStatus_error :: Event t ()
    -- ^ A connection error occurred. A close event always follows.
  , _webSocketClientStatus_close :: Event t (Bool, Word, Text)
    -- ^ The connection closed with @wasClean@, code, and reason
  , _webSocketClientStatus_unparseableMessage :: Event t ByteString
    -- ^ An incoming message that does not decode as a 'TaggedResponse'.
    -- The library cannot match such a message to a request. It reports the
    -- message here instead of a request error. If you share the socket with
    -- other traffic, use 'tagRequests'. Split the incoming messages
    -- yourself.
  , _webSocketClientStatus_unmatchedResponse :: Event t (Int, Value)
    -- ^ A well-formed 'TaggedResponse' with a tag that no outstanding
    -- request uses. The tag is unknown, or the response is a duplicate. The
    -- library cannot correlate such a response. It reports the response
    -- here.
  }

-- | Open one websocket connection. Send the output of a 'RequesterT' widget
-- as API requests on this socket. Integer tags multiplex the requests. Feed
-- the first result back into the requester as responses. The second result
-- reports connection and protocol problems. For example:
--
-- @
-- rec (appResult, requests) <- runRequesterT myApplication responses
--     (responses, status) <- performWebSocketRequests (def { _webSocketClientConfig_url = myEndpoint }) requests
-- @
--
-- See 'WebSocketClientStatus' for the lifecycle of outstanding requests
-- across disconnects and reconnects.
performWebSocketRequests
  :: forall req t m.
     ( Prerender t m, Monad m
     , FromJSON (Some req)
     , forall a. ToJSON (req a)
     , Has FromJSON req
     )
  => WebSocketClientConfig t
  -> Event t (RequesterData req)
  -> m (Event t (RequesterData (Either Text)), WebSocketClientStatus t)
performWebSocketRequests cfg req = do
  d <- prerender (pure (never, statusNever)) $ do
    rec w <- webSocket (_webSocketClientConfig_url cfg) $ WebSocketConfig
          { _webSocketConfig_send = fmap encode <$> reqs
          , _webSocketConfig_close = _webSocketClientConfig_close cfg
          , _webSocketConfig_reconnect = _webSocketClientConfig_reconnect cfg
          , _webSocketConfig_protocols = _webSocketClientConfig_protocols cfg
          }
        let decoded = ffor (_webSocket_recv w) $ \msg -> case decodeStrict msg of
              Just r -> Right r
              Nothing -> Left msg
            rsp = fmapMaybe (either (const Nothing) Just) decoded
            unparseable = fmapMaybe (either Just (const Nothing)) decoded
        (reqs, rsps, unmatched) <- tagRequests req rsp
    pure (rsps, WebSocketClientStatus
      { _webSocketClientStatus_open = _webSocket_open w
      , _webSocketClientStatus_error = _webSocket_error w
      , _webSocketClientStatus_close = _webSocket_close w
      , _webSocketClientStatus_unparseableMessage = unparseable
      , _webSocketClientStatus_unmatchedResponse = unmatched
      })
  pure
    ( switchPromptlyDyn (fst <$> d)
    , WebSocketClientStatus
        { _webSocketClientStatus_open = switchPromptlyDyn (_webSocketClientStatus_open . snd <$> d)
        , _webSocketClientStatus_error = switchPromptlyDyn (_webSocketClientStatus_error . snd <$> d)
        , _webSocketClientStatus_close = switchPromptlyDyn (_webSocketClientStatus_close . snd <$> d)
        , _webSocketClientStatus_unparseableMessage = switchPromptlyDyn (_webSocketClientStatus_unparseableMessage . snd <$> d)
        , _webSocketClientStatus_unmatchedResponse = switchPromptlyDyn (_webSocketClientStatus_unmatchedResponse . snd <$> d)
        }
    )
  where
    statusNever = WebSocketClientStatus never never never never never

-- | Construct a response for a request. This function does the decoding,
-- the encoding, and the tagging that 'TaggedRequest' and 'TaggedResponse'
-- need.
mkTaggedResponse
  :: (Monad m, FromJSON (Some f), Has ToJSON f)
  => TaggedRequest
  -> (forall a. f a -> m a)
  -> m (Either String TaggedResponse)
mkTaggedResponse (TaggedRequest reqId v) f =
  fmap (fmap (TaggedResponse reqId)) $ serveApiRequest f v

-- | Transform a request 'Event' into an 'Event' of 'TaggedRequest' values.
-- 'TaggedRequest' is the indexed wire format for requests. This function
-- expects an 'Event' of 'TaggedResponse' values. 'TaggedResponse' is the
-- response wire format. This function transforms them into untagged
-- responses.
--
-- @
--   requests  --> |-------------| --> tagged requests
--     ↗           |             |                 ↘
-- Client          | tagRequests |                Server
--     ↖           |             |                 ↙
--   responses <-- |-------------| <-- tagged responses
-- @
--
-- Use this function when one websocket serves multiple purposes. It gives
-- you the behavior of 'performWebSocketRequests' without a dedicated
-- socket. For example, split the socket into two channels. Use one channel
-- for these tagged requests. Use the other channel for data that the server
-- pushes.
--
-- The third result reports 'TaggedResponse' values with a tag that no
-- outstanding request uses. The tag is unknown or duplicate. Such responses
-- are dropped from the response output. Outstanding requests survive
-- disconnects. They are answered when a matching response arrives.
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
       , Event t (Int, Value)
       )
tagRequests req rsp = do
  rec (matchedReq, matchedRsp) <- matchResponsesWithRequests enc req $
        ffor rsp $ \(TaggedResponse t v) -> (t, v)
      let wireReq = fmap (Map.elems . Map.mapMaybeWithKey (\t v -> case fromJSON v of
            Success (r :: Value) -> Just $ TaggedRequest t r
            _ -> Nothing)) matchedReq
      outstanding <- foldDyn ($) Set.empty $ leftmost
        [ ffor matchedReq $ \m s -> Set.union s (Map.keysSet m)
        , ffor rsp $ \(TaggedResponse t _) s -> Set.delete t s
        ]
      let unmatched = flip push rsp $ \(TaggedResponse t v) -> do
            s <- sample $ current outstanding
            pure $ if Set.member t s then Nothing else Just (t, v)
  pure (wireReq, matchedRsp, unmatched)
  where
    enc :: forall a. req a -> (Value, Value -> Either Text a)
    enc = encodeApiRequest
