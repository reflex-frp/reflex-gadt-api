reflex-gadt-api
===============
[![travis-ci](https://api.travis-ci.org/reflex-frp/reflex-gadt-api.svg?branch=develop)](https://travis-ci.org/reflex-frp/reflex-gadt-api)


This package is designed to be used in full-stack Haskell applications where the API is defined as a GADT, the wire format is JSON, and the frontend is using reflex-dom(-core). reflex-gadt-api provides the basic FRP and encoding/decoding infrastructure to support this architecture.

Example Usage:
--------------

Let's start with some imports and language pragmas.

```haskell

> {-# LANGUAGE DeriveGeneric #-}
> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE KindSignatures #-}
> {-# LANGUAGE LambdaCase #-}
> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE OverloadedStrings #-}
> {-# LANGUAGE PolyKinds #-}
> {-# LANGUAGE QuantifiedConstraints #-}
> {-# LANGUAGE RankNTypes #-}
> {-# LANGUAGE RecursiveDo #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE TemplateHaskell #-}
> {-# LANGUAGE TypeApplications #-}
> {-# LANGUAGE TypeFamilies #-}
> {-# LANGUAGE UndecidableInstances #-}
>
> module Readme where
>
> import Control.Monad.IO.Class
> import Control.Monad.Fix
> import Data.Aeson
> import Data.Aeson.GADT.TH
> import Data.Constraint.Extras
> import Data.Constraint.Extras.TH
> import Data.Text as T
> import Data.Time
> import GHC.Generics
> import Reflex.Dom.Core
> import Reflex.Dom.GadtApi

```

The code that follows would typically go in a common module, since it would be used on the frontend and on the backend. It sets up the basic data type definitions and a couple of GADTs that describe the API.

```haskell
> data Dog = Dog
>   { _dog_name :: Text
>   , _dog_sighted :: UTCTime
>   , _dog_suspicious :: Bool
>   , _dog_imageUri :: Maybe Text
>   }
>   deriving (Generic)
>
> instance ToJSON Dog
> instance FromJSON Dog
>
```

Here we have an API for retrieving and interacting with the `Dog` data:

```haskell
> data DogApi :: * -> * where
>   DogApi_GetByDay :: Day -> DogApi [Dog]
>   DogApi_GetByName :: Text -> DogApi [Dog]
>   DogApi_GetLastSeen :: DogApi (Maybe Dog)
>   DogApi_ReportSighting :: Text -> Bool -> Maybe Text -> DogApi (Either Text ())
>   DogApi_GetSuspiciousSightings :: DogApi [Dog]
>
> newtype Token = Token { unToken :: Text }
>   deriving (Generic)
>
> instance ToJSON Token
> instance FromJSON Token
>
```

We can take the `DogApi` and embed it in another GADT API. This outer API will take handle authentication. (Note that we're not actually implementing a secure authentication scheme or anything here. This is just a toy example.)

```haskell
> data CatApi a where
>   CatApi_Identify :: Text -> CatApi (Either Text Token)
>   CatApi_DogApi :: Token -> DogApi a -> CatApi a
>
> deriveJSONGADT ''DogApi
> deriveJSONGADT ''CatApi
> deriveArgDict ''DogApi
> deriveArgDict ''CatApi
>
```

On the frontend, we'll run a 'RequesterT' widget that allows us to emit an event of requests, and we'll transform those requests into XHR calls to the API endpoint.

```haskell
>
> startCatnet
>   :: forall js t m.
>      ( MonadFix m, Reflex t, Prerender js t m, MonadHold t m
>      , MonadIO (Performable m)
>      , Adjustable t m, DomBuilder t m, PostBuild t m
>      , TriggerEvent t m, PerformEvent t m
>      , Has FromJSON CatApi
>      , forall a. ToJSON (CatApi a)
>      )
>   => ApiEndpoint
>   -> m ()
> startCatnet apiUrl = do
>   rec (_, requests) <- runRequesterT start $ switchPromptlyDyn responses
>       responses <- performXhrRequests apiUrl (requests :: Event t (RequesterData CatApi))
>   pure ()
>   where
>     start = workflowView login
>     login :: Workflow t (RequesterT t CatApi (Either Text) m) ()
>     login = Workflow $ do
>       el "h1" $ text "Identify Yourself"
>       cat <- inputElement def
>       click <- button "submit"
>       rsp <- requestingJs $
>         tag (current $ CatApi_Identify <$> value cat) click
>       let token = fforMaybe rsp $ \case
>             Right (Right catToken) -> Just catToken
>             _ -> Nothing
>       pure ((), catnet <$> token)
>     catnet token = Workflow $ do
>       el "h1" $ text "Welcome, Fellow Cat."
>       addDog <- button "Report a Sighting"
>       getLastSeen <- button "Get Last Seen"
>       lastSeen <- requestingJs $
>         CatApi_DogApi token DogApi_GetLastSeen <$ getLastSeen
>       widgetHold_ blank $ ffor lastSeen $ \case
>         Left err -> text err
>         Right Nothing -> text "No dogs reported. Rest easy, catizen."
>         Right (Just dog) -> showDog dog
>       showSuspicious <- button "Suspicious Dogs"
>       suspicious <- requestingJs $
>         CatApi_DogApi token DogApi_GetSuspiciousSightings <$ showSuspicious
>       widgetHold_ blank $ ffor suspicious $ \case
>         Left err -> text err
>         Right dogs -> el "ul" $ mapM_ (el "li" . showDog) dogs
>       return ((), dogSighting token <$ addDog)
>     dogSighting token = Workflow $ do
>       name <- el "label" $ do
>         text "Name"
>         _inputElement_value <$> inputElement def
>       suspect <- el "label" $ do
>         text "Suspicious?"
>         value <$> checkbox False def
>       img <- el "label" $ do
>         text "Image URI (if available)"
>         v <- _inputElement_value <$> inputElement def
>         return $ ffor v $ \v' -> if T.null v' then Nothing else Just v'
>       send <- button "submit"
>       let dogApi = DogApi_ReportSighting <$> name <*> suspect <*> img
>       rsp <- requestingJs $ tag (current $ CatApi_DogApi token <$> dogApi) send
>       widgetHold_ blank $ ffor rsp $ \case
>         Right (Right ()) -> text "Dog reported! Returning to catnet..."
>         _ -> text "Couldn't submit."
>       leave <- delay 3 rsp
>       return ((), catnet token <$ leave)
>     requestingJs r = fmap (switch . current) $ prerender (pure never) $ requesting r
>     showDog dog = divClass "dog" $ el "dl" $ do
>       el "dt" $ text "Name"
>       el "dd" $ text $ _dog_name dog
>       el "dt" $ text "Last Seen: "
>       el "dd" $ text $ pack $ show $ _dog_sighted dog
>       el "dt" $ text "Suspicious?"
>       el "dd" $ text $ case _dog_suspicious dog of
>         True -> "very"
>         False -> "not sure"
>       el "dt" $ text "Mugshot"
>       el "dd" $ case _dog_imageUri dog of
>         Just img -> elAttr "img" ("src" =: img <> "alt" =: "dog mugshot") blank
>         Nothing -> text "None"
>
> main :: IO ()
> main = return ()
```

Go to the [example](example) [directory](directory) to run this example (including the backend).
