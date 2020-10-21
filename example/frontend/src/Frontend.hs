{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Frontend where

import Obelisk.Frontend
import Obelisk.Route

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
  , _frontend_body = do
      let enc = checkEncoder fullRouteEncoder
      case enc of
        Left _ -> error "Routes are invalid!"
        Right validEnc -> do
          startCatnet $ renderBackendRoute validEnc $ BackendRoute_Api :/ ()
  }
