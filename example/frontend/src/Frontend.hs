{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Frontend where

import Data.Functor.Identity
import Data.Text as T
import Obelisk.Frontend
import Obelisk.Route.Frontend
import Obelisk.Configs
import Reflex.Dom.Core

import Common.Route
import Readme

type ValidEnc = Encoder Identity Identity (R (FullRoute BackendRoute FrontendRoute)) PageName

-- This runs in a monad that can be run on the client or the server.
-- To run code in a pure client or pure server context, use one of the
-- `prerender` functions.
frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "CatNet"
  , _frontend_body = do
      let enc :: Either Text ValidEnc = checkEncoder fullRouteEncoder
      r <- getTextConfig "common/route"
      case (enc, r) of
        (Left _, _) -> error "Routes are invalid!"
        (_, Nothing) -> error "Couldn't load common/route config file"
        (Right validEnc, Just host) -> do
          subRoute_ $ \case
            FrontendRoute_Main -> do
              el "h1" $ text "Choose preferred technology"
              routeLink (FrontendRoute_UseXhr :/ ()) $ el "button" $ text "XHR"
              routeLink (FrontendRoute_UseWS :/ ()) $ el "button" $ text "WebSocket"
            FrontendRoute_UseWS -> startCatnet $ Right $
              T.replace "http" "ws" host <>
                renderBackendRoute validEnc (BackendRoute_WebSocket :/ ())
            FrontendRoute_UseXhr -> startCatnet $ Left $
              renderBackendRoute validEnc $ BackendRoute_Api :/ ()
  }
