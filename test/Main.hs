{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
module Main (main) where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), decodeStrict, encode, object, toJSON, (.=))
import Data.ByteString.Lazy (toStrict)
import Data.Aeson.GADT.TH (deriveJSONGADT)
import Data.Constraint.Extras.TH (deriveArgDict)
import Data.Either (isLeft)
import Data.Kind (Type)
import Data.List (isInfixOf)
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Test.Hspec (describe, hspec, it, shouldBe, shouldSatisfy)

import Reflex
import Reflex.Pure
import Reflex.Dom.GadtApi.Codec (encodeApiRequest, serveApiRequest)
import Reflex.Dom.GadtApi.WebSocket

data DogApi :: Type -> Type where
  DogApi_GetName :: DogApi Text
  DogApi_Count :: DogApi Int
  DogApi_GetLast :: DogApi (Maybe Text)
  DogApi_Report :: Text -> DogApi (Either Text ())

deriveJSONGADT ''DogApi
deriveArgDict ''DogApi

newtype Token = Token { unToken :: Text }
  deriving (Generic)

instance ToJSON Token
instance FromJSON Token

data CatApi a where
  CatApi_Identify :: Text -> CatApi (Either Text Token)
  CatApi_DogApi :: Token -> DogApi a -> CatApi a

deriveJSONGADT ''CatApi
deriveArgDict ''CatApi

-- The wire format of a request is @("<Tag>", payload)@ (see aeson-gadt-th);
-- constructors with no arguments encode @()@ as the empty array.
tagged :: Text -> Value -> Value
tagged reqTag payload = toJSON (reqTag, payload)

handleDogApi :: DogApi a -> IO a
handleDogApi = \case
  DogApi_GetName -> pure "Rex"
  DogApi_Count -> pure 2
  DogApi_GetLast -> pure (Just "Fido")
  DogApi_Report _ -> pure (Right ())

handleCatApi :: CatApi a -> IO a
handleCatApi = \case
  CatApi_Identify name -> pure (Right (Token ("token-" <> name)))
  CatApi_DogApi _ inner -> handleDogApi inner

-- | A pure timeline: moments are 'Int's, and an 'Event' is just a function
-- of the moment. This lets us drive 'RequesterT' and 'tagRequests' end to
-- end, with real request tagging and correlation, without a host framework.
type T = Pure Int

-- | An event that fires with the given values at the given moments
occ :: [(Int, a)] -> Event T a
occ xs = Event (`lookup` xs)

-- | One client network: a 'RequesterT' widget with one 'requesting' call per
-- response type, wired through 'tagRequests'. Returns each requester's typed
-- response event, the outgoing wire requests, and unmatched responses.
buildClient
  :: Event T (DogApi Text)
  -> Event T (DogApi Int)
  -> Event T (DogApi (Maybe Text))
  -> Event T TaggedResponse
  -> Int
  -> ( Event T (Either Text Text)
     , Event T (Either Text Int)
     , Event T (Either Text (Maybe Text))
     , Event T [TaggedRequest]
     , Event T (Int, Value)
     )
buildClient eName eCount eLast rsp t0 =
  let widget = do
        wName <- requesting eName
        wCount <- requesting eCount
        wLast <- requesting eLast
        pure (wName, wCount, wLast)
      ((rName, rCount, rLast), reqs) = runRequesterT widget responses t0
      (wire, responses, unmatched) = tagRequests reqs rsp t0
  in (rName, rCount, rLast, wire, unmatched)

-- | Observation of an event at every listed moment
atEach :: [Int] -> Event T a -> [(Int, Maybe a)]
atEach ts e = fmap (\t -> (t, unEvent e t)) ts

main :: IO ()
main = hspec $ do
  describe "encodeApiRequest" $ do
    it "encodes the request and decodes a matching response" $ do
      let (v, dec) = encodeApiRequest DogApi_GetName
      v `shouldBe` tagged "DogApi_GetName" (toJSON ())
      dec (String "Rex") `shouldBe` Right "Rex"
    it "uses each request's own response dictionary" $ do
      let (_, decText) = encodeApiRequest DogApi_GetName
          (_, decInt) = encodeApiRequest DogApi_Count
          (_, decMaybe) = encodeApiRequest DogApi_GetLast
          (_, decEither) = encodeApiRequest (DogApi_Report "Fido")
      decText (String "Rex") `shouldBe` Right "Rex"
      decInt (Number 3) `shouldBe` Right 3
      decMaybe Null `shouldBe` Right Nothing
      decEither (object ["Left" .= ("no such dog" :: Text)]) `shouldBe` Right (Left "no such dog")
    it "reports responses that do not match the response type" $ do
      let (_, decInt) = encodeApiRequest DogApi_Count
      decInt (String "seven") `shouldSatisfy` isLeft
    it "supports nested APIs" $ do
      let (v, dec) = encodeApiRequest (CatApi_DogApi (Token "t") DogApi_Count)
      v `shouldBe` tagged "CatApi_DogApi" (toJSON (Token "t", tagged "DogApi_Count" (toJSON ())))
      dec (Number 3) `shouldBe` Right 3

  describe "serveApiRequest" $ do
    it "decodes, interprets, and encodes a request" $ do
      r <- serveApiRequest handleCatApi (tagged "CatApi_Identify" (toJSON ("felix" :: Text)))
      r `shouldBe` Right (object ["Right" .= object ["unToken" .= ("token-felix" :: Text)]])
    it "decodes nested requests with the inner response type" $ do
      r <- serveApiRequest handleCatApi
        (tagged "CatApi_DogApi" (toJSON (Token "t", tagged "DogApi_GetLast" (toJSON ()))))
      r `shouldBe` Right (String "Fido")
    it "rejects unknown request tags" $ do
      r <- serveApiRequest handleCatApi (tagged "CatApi_Nonsense" (toJSON ()))
      r `shouldSatisfy` \case
        Left e -> "CatApi_Nonsense" `isInfixOf` e
        Right _ -> False
    it "rejects malformed request payloads" $ do
      r <- serveApiRequest handleCatApi (tagged "CatApi_Identify" (toJSON (5 :: Int)))
      r `shouldSatisfy` isLeft

  describe "mkTaggedResponse" $ do
    it "interprets and re-tags a request" $ do
      r <- mkTaggedResponse
        (TaggedRequest 7 (tagged "DogApi_GetName" (toJSON ())))
        handleDogApi
      fmap encode r `shouldBe` Right (encode (TaggedResponse 7 (String "Rex")))
    it "rejects malformed request values" $ do
      r <- mkTaggedResponse (TaggedRequest 7 (Number 1)) handleDogApi
      fmap encode r `shouldSatisfy` isLeft

  describe "TaggedResponse wire format" $ do
    it "round-trips through JSON" $ do
      let rsp = TaggedResponse 1 (String "Rex")
      fmap encode (decodeStrict (toStrict (encode rsp)) :: Maybe TaggedResponse) `shouldBe` Just (encode rsp)
    it "does not decode malformed frames" $ do
      isNothing (decodeStrict "{" :: Maybe TaggedResponse) `shouldBe` True
      isNothing (decodeStrict "not json" :: Maybe TaggedResponse) `shouldBe` True

  describe "tagRequests" $ do
    it "issues sequentially tagged requests and correlates responses" $ do
      let (rName, _, _, wire, unmatched) =
            buildClient (occ [(1, DogApi_GetName)]) never never
              (occ [(2, TaggedResponse 1 (String "Rex"))]) 0
      fmap toJSON <$> unEvent wire 1 `shouldBe` Just [toJSON (1 :: Int, tagged "DogApi_GetName" (toJSON ()))]
      atEach [1 .. 3] rName `shouldBe` [(1, Nothing), (2, Just (Right "Rex")), (3, Nothing)]
      atEach [1 .. 3] unmatched `shouldBe` [(t, Nothing) | t <- [1 .. 3]]

    it "correlates out-of-order responses" $ do
      let (rName, rCount, _, _, unmatched) =
            buildClient (occ [(1, DogApi_GetName)]) (occ [(2, DogApi_Count)]) never
              (occ [ (3, TaggedResponse 2 (Number 7))
                   , (4, TaggedResponse 1 (String "Rex"))
                   ]) 0
      atEach [1 .. 4] rCount `shouldBe`
        [(1, Nothing), (2, Nothing), (3, Just (Right 7)), (4, Nothing)]
      atEach [1 .. 4] rName `shouldBe`
        [(1, Nothing), (2, Nothing), (3, Nothing), (4, Just (Right "Rex"))]
      atEach [1 .. 4] unmatched `shouldBe` [(t, Nothing) | t <- [1 .. 4]]

    it "tags multiple requests issued in the same frame" $ do
      let (rName, rCount, _, wire, _) =
            buildClient (occ [(1, DogApi_GetName)]) (occ [(1, DogApi_Count)]) never
              (occ [ (2, TaggedResponse 1 (Number 7))
                   , (3, TaggedResponse 2 (String "Rex"))
                   ]) 0
      fmap toJSON <$> unEvent wire 1 `shouldBe`
        Just [ toJSON (1 :: Int, tagged "DogApi_Count" (toJSON ()))
             , toJSON (2 :: Int, tagged "DogApi_GetName" (toJSON ())) ]
      atEach [1 .. 3] rCount `shouldBe` [(1, Nothing), (2, Just (Right 7)), (3, Nothing)]
      atEach [1 .. 3] rName `shouldBe` [(1, Nothing), (2, Nothing), (3, Just (Right "Rex"))]

    it "reports responses with unknown tags instead of dropping them" $ do
      let (rName, _, _, _, unmatched) =
            buildClient (occ [(1, DogApi_GetName)]) never never
              (occ [(2, TaggedResponse 99 (String "Rex"))]) 0
      atEach [1 .. 2] rName `shouldBe` [(1, Nothing), (2, Nothing)]
      atEach [1 .. 2] unmatched `shouldBe` [(1, Nothing), (2, Just (99, String "Rex"))]

    it "reports duplicate responses instead of dropping them" $ do
      let (rName, _, _, _, unmatched) =
            buildClient (occ [(1, DogApi_GetName)]) never never
              (occ [ (2, TaggedResponse 1 (String "Rex"))
                   , (3, TaggedResponse 1 (String "Rex again"))
                   ]) 0
      atEach [1 .. 3] rName `shouldBe`
        [(1, Nothing), (2, Just (Right "Rex")), (3, Nothing)]
      atEach [1 .. 3] unmatched `shouldBe`
        [(1, Nothing), (2, Nothing), (3, Just (1, String "Rex again"))]

    it "surfaces malformed payloads as request errors and retires the tag" $ do
      let (rName, _, _, _, unmatched) =
            buildClient (occ [(1, DogApi_GetName)]) never never
              (occ [ (2, TaggedResponse 1 (Number 7)) -- not Text
                   , (3, TaggedResponse 1 (String "late retry"))
                   ]) 0
      unEvent rName 1 `shouldBe` Nothing
      unEvent rName 2 `shouldSatisfy` \case
        Just (Left e) -> "Text" `isInfixOf` Text.unpack e
        _ -> False
      unEvent rName 3 `shouldBe` Nothing
      atEach [1 .. 3] unmatched `shouldBe`
        [(1, Nothing), (2, Nothing), (3, Just (1, String "late retry"))]
