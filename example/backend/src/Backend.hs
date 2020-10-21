{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Backend where

import Control.Monad.IO.Class (liftIO)
import Common.Route
import Obelisk.Backend
import Obelisk.Route
import Snap.Core
import Data.Aeson as Aeson
import Readme
import Data.Some
import Data.List
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.IORef

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
    dogs <- newIORef dogs0
    let withDogs :: ([Dog] -> Snap a) -> Snap a
        withDogs f = do
          d <- liftIO $ readIORef dogs
          f d
    serve $ \case
      BackendRoute_Api :/ () -> (Aeson.decode <$> readRequestBody 16384) >>= \case
        Nothing -> do
          modifyResponse $ setResponseStatus 400 "Bad Request"
          writeText "Are you sure that's what you meant?"
        Just (Some (CatApi_Identify cat)) ->
          writeLBS $ Aeson.encode $
            (Right $ Token $ T.reverse cat :: Either Text Token)
        Just (Some (CatApi_DogApi token dogApi)) -> do
          liftIO $ putStrLn $ T.unpack $ T.intercalate " "
            [ "Received a request from cat"
            , T.reverse (unToken token)
            , "(Token: " <> unToken token <> ")"
            ]
          writeLBS =<< case dogApi of
            DogApi_GetLastSeen -> withDogs $ \d -> pure $
              case sortOn _dog_sighted d of
                (lastSeen:_) -> Aeson.encode $ Just lastSeen
                _ -> Aeson.encode (Nothing :: Maybe Dog)
            DogApi_GetSuspiciousSightings -> withDogs $ \d -> pure $
              Aeson.encode $ filter _dog_suspicious d
            DogApi_GetByDay day -> withDogs $ \d -> pure $
              Aeson.encode $ filter (\x -> day == utctDay (_dog_sighted x)) d
            DogApi_GetByName name -> withDogs $ \d -> pure $
              Aeson.encode $ filter (\x -> name == _dog_name x) d
            DogApi_ReportSighting name susp img -> do
              liftIO $ do
                t <- getCurrentTime
                let dog = Dog name t susp img
                atomicModifyIORef' dogs $ \ds -> (dog : ds, ())
              pure $ Aeson.encode $ (Right () :: Either Text ())
      BackendRoute_Missing :/ _ -> do
        modifyResponse $ setResponseStatus 404 "Not Found"
        writeText "404 Nothing to see here."
  , _backend_routeEncoder = fullRouteEncoder
  }

dogs0 :: [Dog]
dogs0 =
  [ Dog "Ace" (read "2020-10-21 19:41:17 UTC") True (Just "https://images.dog.ceo/breeds/greyhound-italian/n02091032_11088.jpg")
  , Dog "Bandit" (read "2020-10-19 12:33:01 UTC") True (Just "https://images.dog.ceo/breeds/entlebucher/n02108000_2185.jpg")
  , Dog "Chance" (read "2020-09-19 02:30:44 UTC") True (Just "https://images.dog.ceo/breeds/pyrenees/n02111500_1787.jpg")
  , Dog "Duke" (read "2020-10-21 00:14:00 UTC") True Nothing
  , Dog "Elvis" (read "2020-10-19 12:33:04 UTC") False (Just "https://images.dog.ceo/breeds/spaniel-japanese/n02085782_3727.jpg")
  ]

