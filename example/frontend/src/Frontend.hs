{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Frontend where

import Data.Text as T
import Obelisk.Configs
import Obelisk.Frontend
import Obelisk.Generated.Static
import Obelisk.Route.Frontend
import Reflex.Dom.Core

import Common.Route
import Readme

-- This runs in a monad that can be run on the client or the server.
-- To run code in a pure client or pure server context, use one of the
-- `prerender` functions.
frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "CatNet"
      elAttr "link" ("rel" =: "stylesheet" <> "href" =: static @"pico.classless.min.css") blank
  , _frontend_body = do
      r <- getTextConfig "common/route"
      case r of
        Nothing -> error "Couldn't load common/route config file"
        Just host -> elClass "main" "container" $ do
          subRoute_ $ \case
            FrontendRoute_Main -> do
              el "h1" $ text "Choose preferred technology"
              routeLink (FrontendRoute_UseXhr :/ ()) $ el "button" $ text "XHR"
              routeLink (FrontendRoute_UseWS :/ ()) $ el "button" $ text "WebSocket"
            FrontendRoute_UseWS -> startCatnet $ Right $
              T.replace "http" "ws" host <>
                renderBackendRoute checkedFullRouteEncoder (BackendRoute_WebSocket :/ ())
            FrontendRoute_UseXhr -> startCatnet $ Left $
              renderBackendRoute checkedFullRouteEncoder $ BackendRoute_Api :/ ()
  }
