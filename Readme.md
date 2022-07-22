reflex-gadt-api
===============
[![Haskell](https://img.shields.io/badge/language-Haskell-orange.svg)](https://haskell.org) [![Hackage](https://img.shields.io/hackage/v/reflex-gadt-api.svg)](https://hackage.haskell.org/package/reflex-gadt-api) [![Hackage CI](https://matrix.hackage.haskell.org/api/v2/packages/reflex-gadt-api/badge)](https://matrix.hackage.haskell.org/#/package/reflex-gadt-api) [![Travis CI](https://api.travis-ci.org/reflex-frp/reflex-gadt-api.svg?branch=develop)](https://travis-ci.org/reflex-frp/reflex-gadt-api) [![Github CI](https://github.com/reflex-frp/reflex-gadt-api/workflows/Haskell%20CI/badge.svg)](https://github.com/reflex-frp/reflex-gadt-api/actions) [![BSD3 License](https://img.shields.io/badge/license-BSD3-blue.svg)](https://github.com/reflex-frp/reflex-gadt-api/blob/master/LICENSE)



This package is designed to be used in full-stack Haskell applications where the API is defined as a GADT, the wire format is JSON, and the frontend is using reflex-dom(-core). reflex-gadt-api provides the basic FRP and encoding/decoding infrastructure to support this architecture.

To serialize the GADT API definition, we use [aeson-gadt-th](https://github.com/obsidiansystems/aeson-gadt-th) with a little help from [constraints-extras](https://github.com/obsidiansystems/constraints-extras).

Example Usage:
--------------

Let's start with some imports and language pragmas.

```haskell

> {-# LANGUAGE DeriveGeneric #-}
> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE LambdaCase #-}
> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE OverloadedStrings #-}
> {-# LANGUAGE QuantifiedConstraints #-}
> {-# LANGUAGE RecursiveDo #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE TemplateHaskell #-}
> {-# LANGUAGE TypeFamilies #-}
> {-# LANGUAGE UndecidableInstances #-}
>
> module Readme where
>
> import Control.Monad (void)
> import Control.Monad.IO.Class (MonadIO)
> import Control.Monad.Fix (MonadFix)
> import Data.Aeson (ToJSON(..), FromJSON(..))
> import Data.Aeson.GADT.TH (deriveJSONGADT)
> import Data.Constraint.Extras (Has)
> import Data.Constraint.Extras.TH (deriveArgDict)
> import Data.Text as T (Text, null, pack)
> import Data.Time (UTCTime, Day)
> import GHC.Generics (Generic)
> import Reflex.Dom.Core
> import Reflex.Dom.GadtApi
>

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

We can take the `DogApi` and embed it in another GADT API. This outer API will handle authentication. (Note that we're not actually implementing a secure authentication scheme or anything here. This is just a toy example.)

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

On the frontend, we'll run a `RequesterT` widget that allows us to emit an event of requests, and we'll transform those requests into XHR calls to the API endpoint.

```haskell

> type Catnet t m = (RequesterT t CatApi (Either Text) m)
>

```

This synonym is just here for convenience. The right-hand-side describes a `RequesterT` widget that issues `CatApi` requests and receives responses.  In other words, when we're inside this `RequesterT` we can call the `requesting` function to send API requests and receive responses.

The `Either` here represents the possibility that we'll receive an error instead of the response we expected.

Now, we'll actually start up the `RequesterT`:

```haskell

> startCatnet
>   :: forall t m.
>      ( Prerender t m, MonadHold t m
>      , MonadIO (Performable m), MonadFix m
>      , DomBuilder t m, PostBuild t m
>      , TriggerEvent t m, PerformEvent t m
>      , Has FromJSON CatApi
>      , forall a. ToJSON (CatApi a)
>      )
>   => Either ApiEndpoint WebSocketEndpoint
>   -> m ()
> startCatnet endpoint = do

```
`runRequesterT` expects us to provide some reflex widget as its first argument (here `start`). The reflex widget is able to issue API requests, and one of the results of `runRequesterT` is an `Event` of those requests.

The second argument to `runRequesterT` is an `Event` of responses. Because we need to get this `Event` of requests and feed it into a function that can produce the responses, and then feed those responses back into `runRequesterT`, we have to use `RecursiveDo`. If we didn't we wouldn't have access to the responses inside of the reflex widget that issued the requests. This is the main loop of a `RequesterT` widget.


```haskell

>   rec (_, requests) <- runRequesterT start responses

```

The `Event` of responses comes, in this case, from a function that will take the requests emitted on the previous line and fetch responses to those requests. It produces an `Event` of responses. We can use either `Reflex.Dom.GadtApi.XHR.performXhrRequests` if we want to send requests using XHR or `Reflex.Dom.GadtApi.WebSocket.performWebSocketRequests` to use WebSockets.
```haskell

>       responses <- case endpoint of
>         Left xhr -> performXhrRequests xhr (requests :: Event t (RequesterData CatApi))
>         Right ws -> performWebSocketRequests ws (requests :: Event t (RequesterData CatApi))
>   pure ()
>   where

```


Our `start` widget has the type `Catnet t m ()`, so it (and its child widgets) can potentially issue `CatApi` requests.

The actual widget code here isn't important to the way that `RequesterT` works, nor is this meant to be production-grade application. Nevertheless, there are a few interesting bits that we'll point out.

We're using reflex `Workflow`s to switch between pages, but we could accomplish the same result in other ways. Each `Workflow` can run a widget and send the user to another page based on the firing of some `Event` (either produced by the child widget or in scope from somewhere else).

```haskell

>     start :: Catnet t m ()
>     start = void $ workflowView loginW
>     loginW :: Workflow t (Catnet t m) ()
>     loginW = Workflow $ do
>       token <- login
>       pure ((), catnetW <$> token) -- Go to catnet after "logging in"
>     catnetW token = Workflow $ do
>       addDog <- catnet token
>       pure ((), dogSightingW token <$ addDog) -- Go to sighting submission form
>     dogSightingW token = Workflow $ do
>       rsp <- dogSighting token
>       leave <- delay 3 rsp
>       pure ((), catnetW token <$ leave) -- Go back to catnet
>

```

If you're building your frontend in a context where the user interface needs to be susceptible to server-side rendering (for example, if you're using [obelisk](https://github.com/obsidiansystems/obelisk)'s "prerendering" functionality to serve static pages that are "hydrated" once the JS loads), you'll need to wrap any code relying on Javascript (e.g., your XHR requests) in a `prerender`. The function below does this for us.

```haskell

> requestingJs
>   :: (Reflex t, MonadFix m, Prerender t m)
>   => Event t (Request (Client (Catnet t m)) a)
>   -> Catnet t m (Event t (Response (Client (Catnet t m)) a))
> requestingJs r = fmap (switch . current) $ prerender (pure never) $ requesting r
>

```

On the login page, we construct a `Behavior` of a `CatApi_Identify` request and send it when the user clicks the submit button.

The response from the server is an `Event` that can be used to update the user interface or perform other actions.

```haskell

> login
>   :: (DomBuilder t m, MonadHold t m, MonadFix m, Prerender t m)
>   => Catnet t m (Event t Token)
> login = do
>   el "h1" $ text "Identify Yourself"
>   cat <- inputElement def
>   click <- button "submit"
>   rsp <- requestingJs $
>     tag (current $ CatApi_Identify <$> value cat) click
>   let token = fforMaybe rsp $ \case
>         Right (Right catToken) -> Just catToken
>         _ -> Nothing
>   pure token
>

```

This function builds a UI with a few buttons. Depending on which button is clicked, we'll issue an API request and display the result, with one exception: the "Report a Sighting" button click event is returned (and used by the `Workflow` above to navigate to another page).

```haskell

> catnet
>   :: (DomBuilder t m, MonadHold t m, MonadFix m, Prerender t m)
>   => Token
>   -> Catnet t m (Event t ())
> catnet token = do
>   el "h1" $ text "Welcome, Fellow Cat."
>   addDog <- button "Report a Sighting"
>   -- Issue a request for the last seen dog and display the result or error.
>   getLastSeen <- button "Get Last Seen"
>   lastSeen <- requestingJs $
>     CatApi_DogApi token DogApi_GetLastSeen <$ getLastSeen
>   widgetHold_ blank $ ffor lastSeen $ \case
>     Left err -> text err
>     Right Nothing -> text "No dogs reported. Rest easy, catizen."
>     Right (Just dog) -> showDog dog
>   -- Issue a request for suspicious dogs and display the result or error.
>   showSuspicious <- button "Suspicious Dogs"
>   suspicious <- requestingJs $
>     CatApi_DogApi token DogApi_GetSuspiciousSightings <$ showSuspicious
>   widgetHold_ blank $ ffor suspicious $ \case
>     Left err -> text err
>     Right dogs -> el "ul" $ mapM_ (el "li" . showDog) dogs
>   -- Return the "Report a sighting" click event
>   pure addDog
>

```

The `showDog` widget below does not have `Catnet` or `RequesterT` in its type signature, so we know that it doesn't issue requests. It just builds a display widget for a particular `Dog`.

```haskell

> showDog :: DomBuilder t m => Dog -> m ()
> showDog dog = divClass "dog" $ el "dl" $ do
>   el "dt" $ text "Name"
>   el "dd" $ text $ _dog_name dog
>   el "dt" $ text "Last Seen: "
>   el "dd" $ text $ pack $ show $ _dog_sighted dog
>   el "dt" $ text "Suspicious?"
>   el "dd" $ text $ case _dog_suspicious dog of
>     True -> "very"
>     False -> "not sure"
>   el "dt" $ text "Mugshot"
>   el "dd" $ case _dog_imageUri dog of
>     Just img -> elAttr "img" ("src" =: img <> "alt" =: "dog mugshot") blank
>     Nothing -> text "None"
>

```

This rudimentary form allows us to assemble a `Dynamic` `DogApi` request, embed it in a `CatApi` request, and send it.

```haskell

> dogSighting
>   :: (DomBuilder t m, MonadHold t m, PostBuild t m, Prerender t m, MonadFix m)
>   => Token
>   -> Catnet t m (Event t (Either Text ()))
> dogSighting token = do

```
The input elements below produce `Dynamic` text and bool values that we will use to construct our `DogApi_ReportSighting` request. Recall that `DogApi_ReportSighting` has the type:

    DogApi_ReportSighting :: Text -> Bool -> Maybe Text -> DogApi (Either Text ())

We need to get some text for the name, a bool indicating the dog's suspiciousness, and, optionally, some text for the url of the dog's picture.

```haskell

>   name <- el "label" $ do
>     text "Name"
>     _inputElement_value <$> inputElement def
>   suspect <- el "label" $ do
>     text "Suspicious?"
>     value <$> checkbox False def
>   img <- el "label" $ do
>     text "Image URI (if available)"
>     v <- _inputElement_value <$> inputElement def
>     pure $ ffor v $ \v' -> if T.null v' then Nothing else Just v'
>   send <- button "submit"

```

Once we've got those three values, we can apply them to the `DogApi_ReportSighting` constructor. We're using some infix functions from [Control.Applicative](https://hackage.haskell.org/package/base/docs/Control-Applicative.html) to apply the constructor to `Dynamic` values, and, as a result, we get a `Dynamic` `DogApi` request. When the submit `Event` fires, we take the `current` value of that `Dynamic` and send it.

```haskell

>   let dogApi = DogApi_ReportSighting <$> name <*> suspect <*> img
>   rsp <- requestingJs $ tag (current $ CatApi_DogApi token <$> dogApi) send
>   widgetHold_ blank $ ffor rsp $ \case
>     Right (Right ()) -> text "Dog reported! Returning to catnet..."
>     _ -> text "Couldn't submit."
>   pure $ ffor rsp $ \case
>     Right (Left err) -> Left err
>     Left err -> Left err
>     Right (Right result) -> Right result
>
> main :: IO ()
> main = return ()

```

Go to the [example](example) directory to run this example (including the [backend](example/backend/src/Backend.md)).
