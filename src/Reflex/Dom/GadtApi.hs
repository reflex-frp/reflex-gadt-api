{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Reflex.Dom.GadtApi where

import Control.Concurrent (newEmptyMVar, putMVar, takeMVar)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Constraint.Extras (Has, has)
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Language.Javascript.JSaddle (MonadJSM)
import Reflex.Dom.Core

type ApiEndpoint = Text

-- | Takes the output of a 'RequesterT' widget and issues that
-- output as API requests. The result of this function can be
-- fed back into the requester as responses. For example:
--
-- @
-- rec (appResult, requests) <- runRequesterT myApplication $
--       switchPromptlyDyn responses
--     responses <- performXhrRequests myApiEndpoint requests
-- @
--
performXhrRequests
  :: forall t m api js.
     ( Has FromJSON api
     , forall a. ToJSON (api a)
     , Prerender js t m
     , Applicative m
     )
  => ApiEndpoint
  -> Event t (RequesterData api)
  -> m (Dynamic t (Event t (RequesterData (Either Text))))
performXhrRequests apiUrl req = prerender (pure never) $
  performEventAsync $ ffor req $ \r yield ->
    liftIO . yield =<< apiRequestXhr apiUrl r

-- | Encodes an API request as JSON and issues an 'XhrRequest',
-- and attempts to decode the response.
apiRequestXhr
  :: forall api m.
     ( MonadIO m
     , HasJSContext m
     , MonadJSM m
     , Has FromJSON api
     , forall a. ToJSON (api a)
     )
  => ApiEndpoint
  -> RequesterData api
  -> m (RequesterData (Either Text))
apiRequestXhr apiUrl = traverseRequesterData $ \x ->
  has @FromJSON @api x $ mkRequest x
  where
    mkRequest
      :: (HasJSContext m, MonadJSM m, FromJSON b)
      => api b
      -> m (Either Text b)
    mkRequest req = do
      response <- liftIO newEmptyMVar
      _ <- newXMLHttpRequest (postJson apiUrl req) $
        liftIO . putMVar response
      xhrResp <- liftIO $ takeMVar response
      case decodeXhrResponse xhrResp of
        Nothing -> pure $ Left $
          "Response could not be decoded for request: " <>
            T.decodeUtf8 (LBS.toStrict $ encode req)
        Just r -> pure $ Right r
