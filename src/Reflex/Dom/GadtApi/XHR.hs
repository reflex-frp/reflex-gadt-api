{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Reflex.Dom.GadtApi.XHR where

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Constraint.Extras (Has, has)
import Data.Functor (void)
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import GHC.Generics
import Language.Javascript.JSaddle (MonadJSM)
import Language.Javascript.JSaddle.Monad (runJSM, askJSM)
import Reflex.Dom.Core

type ApiEndpoint = Text

-- | Takes the output of a 'RequesterT' widget and issues that
-- output as API requests. The result of this function can be
-- fed back into the requester as responses. For example:
--
-- @
-- rec (appResult, requests) <- runRequesterT myApplication responses
--     responses <- performXhrRequests myApiEndpoint requests
-- @
--
performXhrRequests
  :: forall t m api.
     ( Has FromJSON api
     , forall a. ToJSON (api a)
     , Prerender t m
     , Applicative m
     )
  => ApiEndpoint
  -> Event t (RequesterData api)
  -> m (Event t (RequesterData (Either XhrError)))
performXhrRequests apiUrl req = fmap switchPromptlyDyn $ prerender (pure never) $ do
  performEventAsync $ ffor req $ \r yield -> do
    ctx <- askJSM
    void $ liftIO $ forkIO $ flip runJSM ctx $
      liftIO . yield =<< apiRequestXhr apiUrl r

data XhrError = XhrError
  { _xhrError_request :: XhrRequest Text
  , _xhrError_response :: XhrResponse
  }
  deriving (Generic)

-- | Encodes an API request as JSON and issues an 'XhrRequest',
-- and attempts to decode the response.
apiRequestXhr
  :: forall api m.
     ( MonadIO m
     , MonadJSM m
     , Has FromJSON api
     , forall a. ToJSON (api a)
     )
  => ApiEndpoint
  -> RequesterData api
  -> m (RequesterData (Either XhrError))
apiRequestXhr apiUrl = traverseRequesterData $ \x ->
  has @FromJSON @api x $ mkRequest x
  where
    mkRequest
      :: (MonadJSM m, FromJSON b)
      => api b
      -> m (Either XhrError b)
    mkRequest req = do
      response <- liftIO newEmptyMVar
      let request = postJson apiUrl req
      _ <- newXMLHttpRequest request $ liftIO . putMVar response
      xhrResp <- liftIO $ takeMVar response
      case decodeXhrResponse xhrResp of
        Nothing -> pure $ Left $ XhrError request xhrResp
        Just r -> pure $ Right r
