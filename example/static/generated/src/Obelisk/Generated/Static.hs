{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Obelisk.Generated.Static where

import qualified GHC.Types
import Data.Text (Text)
import qualified Data.Text.Internal
import Data.Monoid ((<>))

static :: forall a. StaticFile a => Text
static = "static/" <> staticPath @a

class StaticFile (s :: GHC.Types.Symbol)
    where {staticPath :: Data.Text.Internal.Text}
instance StaticFile "pico.classless.min.css"
    where {staticPath = "0ylmrckdgzdwqshrwgqhk2w4xdyrbxz84m8q2mjy449n1qjj60k4-pico.classless.min.css"}
