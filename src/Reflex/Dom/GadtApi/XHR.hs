{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS -fno-warn-type-defaults #-}

module Reflex.Dom.GadtApi.XHR where

import Control.Lens hiding (has)
import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Constraint.Extras (Has, has)
import Data.Functor (void)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Language.Javascript.JSaddle.Object ( fun )
import Language.Javascript.JSaddle ( MonadJSM
                                   , JSM
                                   , obj
                                   , js0
                                   , js1
                                   , jsgf
                                   , (<#)
                                   , toJSVal
                                   , fromJSValUnchecked
                                   , liftJSM
                                   )
import Language.Javascript.JSaddle.Monad (runJSM, askJSM)
import Reflex.Dom.Core

default (Text)

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
  -> m (Event t (RequesterData (Either Text)))
performXhrRequests apiUrl req = fmap switchPromptlyDyn $ prerender (pure never) $ do
  performXhrRequestsJs apiUrl req

-- | Same as performXhrRequests but without running in a prerender
performXhrRequestsJs
  :: forall t m api.
     ( Has FromJSON api
     , forall a. ToJSON (api a)
     , TriggerEvent t m
     , PerformEvent t m
     , MonadJSM (Performable m)
     )
  => ApiEndpoint
  -> Event t (RequesterData api)
  -> m (Event t (RequesterData (Either Text)))
performXhrRequestsJs apiUrl req = do
  performEventAsync $ ffor req $ \r yield -> do
    ctx <- askJSM
    void $ liftIO $ forkIO $ flip runJSM ctx $
      liftIO . yield =<< apiRequestXhr apiUrl r

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
  -> m (RequesterData (Either Text))
apiRequestXhr apiUrl = traverseRequesterData $ \x ->
  has @FromJSON @api x $ mkRequest x
  where
    mkRequest
      :: (MonadJSM m, FromJSON b)
      => api b
      -> m (Either Text b)
    mkRequest req =
      liftJSM $ fetchPost apiUrl req

-- | Make a POST reqeust via the fetch api
fetchPost :: (ToJSON a, FromJSON b) => Text -> a -> JSM (Either Text b)
fetchPost url a = do
  headers <- obj
  (headers <# "Content-Type") "application/json"

  config <- obj
  (config <# "method") "POST"
  (config <# "cors") "no-cors"
  (config <# "cache") "no-cache"
  (config <# "body") (T.decodeUtf8 $ LBS.toStrict $ encode a)

  mailbox <- liftIO newEmptyMVar
  urlVal <- toJSVal url
  configVal <- toJSVal config
  promise <- jsgf "fetch" [urlVal, configVal]
  let
    textThenHandler = fun $ \_ _ [bodyVal] -> do
      body <- fromJSValUnchecked bodyVal
      let
        ret = case eitherDecode $ LBS.fromStrict $ T.encodeUtf8 body of
          Left err -> Left $ T.pack err
          Right result -> Right result
      liftIO $ putMVar mailbox ret

    thenHandler = fun $ \_ _ [resp] -> do
      bodyPromise <- resp ^. js0 "text"
      _ <- bodyPromise ^. js1 "then" textThenHandler
      pure ()

    catchHandler = fun $ \_ _ [errVal] -> do
      err :: Text <- fromJSValUnchecked errVal
      liftIO $ putMVar mailbox $ Left err

  _ <- promise ^. js1 "then" thenHandler
  _ <- promise ^. js1 "catch" catchHandler

  liftIO $ takeMVar mailbox
