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
import Data.Constraint.Extras (Has, has)
import Data.Default (Default (..))
import Data.Functor (void)
import Data.Text (Text)
import Language.Javascript.JSaddle (MonadJSM)
import Language.Javascript.JSaddle.Monad (runJSM, askJSM)
import Reflex.Dom.Core

type ApiEndpoint = Text

-- | Configuration for 'performXhrRequests' and 'apiRequestXhr'.
--
-- 'Reflex.Dom.Xhr.postJson' builds each request as a JSON @POST@. Use
-- '_xhrClientConfig_configureRequest' to adjust the
-- 'Reflex.Dom.Xhr.XhrRequest' before the library sends it. For example, add
-- headers or credentials.
data XhrClientConfig = XhrClientConfig
  { _xhrClientConfig_url :: ApiEndpoint
    -- ^ URL that the requests are sent to
  , _xhrClientConfig_configureRequest :: XhrRequest Text -> XhrRequest Text
    -- ^ Adjust each outgoing 'XhrRequest' before it is sent
  }

-- | The default configuration uses the empty URL and does not change the
-- requests. Set '_xhrClientConfig_url' before you use it.
instance Default XhrClientConfig where
  def = XhrClientConfig
    { _xhrClientConfig_url = ""
    , _xhrClientConfig_configureRequest = id
    }

-- | Send the output of a 'RequesterT' widget as API requests. Feed the
-- result back into the requester as responses. For example:
--
-- @
-- rec (appResult, requests) <- runRequesterT myApplication responses
--     responses <- performXhrRequests (def { _xhrClientConfig_url = myApiEndpoint }) requests
-- @

performXhrRequests
  :: forall t m api.
     ( Has FromJSON api
     , forall a. ToJSON (api a)
     , Prerender t m
     , Applicative m
     )
  => XhrClientConfig
  -> Event t (RequesterData api)
  -> m (Event t (RequesterData (Either XhrError)))
performXhrRequests cfg req = fmap switchPromptlyDyn $ prerender (pure never) $ do
  performEventAsync $ ffor req $ \r yield -> do
    ctx <- askJSM
    void $ liftIO $ forkIO $ flip runJSM ctx $
      liftIO . yield =<< apiRequestXhr cfg r

-- | A failed request. The record holds the request as it was sent. It also
-- holds the response, or the 'Reflex.Dom.Xhr.XhrException' that stopped the
-- request. 'XhrException_Aborted' shows an aborted request.
-- 'XhrException_Error' shows a request that could not be sent.
data XhrError = XhrError
  { _xhrError_request :: XhrRequest Text
  , _xhrError_response :: Either XhrException XhrResponse
  }

xhrErrorToText :: XhrError -> Text
xhrErrorToText e =
  let url = _xhrRequest_url $ _xhrError_request e
  in case _xhrError_response e of
    Left XhrException_Aborted -> "Aborted: " <> url
    Left XhrException_Error -> "Request error: " <> url
    Right rsp ->
      let
        status = _xhrResponse_statusText rsp
        body = _xhrResponse_responseText rsp
      in status <> case body of
        Nothing -> ""
        Just b -> ": " <> b

-- | Encode an API request as JSON. Send it with one 'XhrRequest'. Decode
-- the response.
--
-- Only a response that does not decode gives an 'XhrError'. This function
-- does not check the HTTP status of the response. A non-2xx response with a
-- body that decodes is a success. Read '_xhrError_response' to get the
-- status of a failed request.
apiRequestXhr
  :: forall api m.
     ( MonadIO m
     , MonadJSM m
     , Has FromJSON api
     , forall a. ToJSON (api a)
     )
  => XhrClientConfig
  -> RequesterData api
  -> m (RequesterData (Either XhrError))
apiRequestXhr cfg = traverseRequesterData $ \x ->
  has @FromJSON @api x $ mkRequest x
  where
    mkRequest
      :: (MonadJSM m, FromJSON b)
      => api b
      -> m (Either XhrError b)
    mkRequest req = do
      response <- liftIO newEmptyMVar
      let request = _xhrClientConfig_configureRequest cfg $
            postJson (_xhrClientConfig_url cfg) req
      _ <- newXMLHttpRequestWithError request $ liftIO . putMVar response
      result <- liftIO $ takeMVar response
      pure $ case result of
        Left e -> Left $ XhrError request (Left e)
        Right xhrResp -> case decodeXhrResponse xhrResp of
          Nothing -> Left $ XhrError request (Right xhrResp)
          Just r -> Right r
