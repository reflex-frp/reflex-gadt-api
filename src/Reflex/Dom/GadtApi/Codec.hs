{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Pure request\/response codecs and dispatch for a GADT API.
--
-- "Reflex.Dom.GadtApi.WebSocket" and "Reflex.Dom.GadtApi.XHR" use these
-- operations internally. Use them on a server, or in a new transport, to
-- keep the same wire format.
--
-- Client and server constraints are separate. A client encodes requests and
-- decodes responses. A server decodes requests and encodes responses.
module Reflex.Dom.GadtApi.Codec
  ( encodeApiRequest
  , serveApiRequest
  ) where

import Data.Aeson
import Data.Constraint.Extras
import Data.Some (Some (..))
import Data.Text (Text)
import qualified Data.Text as T

-- | Encode an API request as JSON, together with a decoder for its response.
--
-- The decoder uses the request to find the 'FromJSON' instance of the
-- response type. Mixed requests hide this type. The decoder makes it known
-- again. A payload that does not decode as the response type gives 'Left'
-- with the parser error.
encodeApiRequest
  :: forall api a. (Has FromJSON api, forall x. ToJSON (api x))
  => api a
  -> (Value, Value -> Either Text a)
encodeApiRequest r =
  ( toJSON r
  , \v -> case has @FromJSON r $ fromJSON v of
      Success x -> Right x
      Error e -> Left $ T.pack e
  )

-- | Decode an incoming request. Run the handler on it. Encode the response.
--
-- A request 'Value' that does not match a constructor of the API gives
-- 'Left' with the parser error. This function does not catch an exception
-- from the handler. The interpreter or the host application must handle
-- such exceptions.
serveApiRequest
  :: (Monad m, FromJSON (Some api), Has ToJSON api)
  => (forall a. api a -> m a)
  -> Value
  -> m (Either String Value)
serveApiRequest f v = case fromJSON v of
  Success (Some a) -> do
    rsp <- f a
    pure $ Right $ has @ToJSON a $ toJSON rsp
  Error e -> pure $ Left e
