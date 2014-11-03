
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Json.WithIdSpec
(
  spec
)
where

import Data.Aeson        ( ToJSON, FromJSON, encode, decode, toJSON )
import Data.Maybe        ( fromJust )
import Data.String       ( IsString(..) )
import Database.Persist  ( Key, KeyBackend(Key), toPersistValue )
import GHC.Generics      ( Generic )
import Test.Hspec        ( Spec, describe, it, shouldBe )
import qualified Data.Text as T
                         ( Text )

import Json.WithId       ( WithId, addId )


instance (ToJSON j, FromJSON j) => Show (WithId j) where
    show = show . toJSON

instance (Eq j, ToJSON j, FromJSON j) => Eq (WithId j) where
    x == y = (show $ toJSON x) == (show $ toJSON y)


data SampleType = SampleType
    {
      name   :: T.Text
    , number :: Int
    }
  deriving (Generic, Show, Eq)

instance ToJSON SampleType
instance FromJSON SampleType

instance IsString (KeyBackend d SampleType) where
    fromString = Key . toPersistValue


spec :: Spec
spec =

    describe "WithId" $ do

        it "should serialize data and include id" $ do
            let withoutId = SampleType { name = "Sample Name", number = 123 }
            encode withoutId `shouldBe` "{\"name\":\"Sample Name\",\"number\":123}"
            let withId = addId ("XYZ" :: Key SampleType) withoutId
            encode withId `shouldBe` "{\"name\":\"Sample Name\",\"id\":\"XYZ\",\"number\":123}"

        it "should deserialize data that includes id" $ do
            let decoded = decode "{\"name\":\"Sample Name\",\"id\":\"XYZ\",\"number\":123}" :: Maybe (WithId SampleType)
            (encode . fromJust $ decoded) `shouldBe` "{\"name\":\"Sample Name\",\"id\":\"XYZ\",\"number\":123}"

        it "should not deserialize data that does not includes id" $ do
            let decoded = decode "{\"name\":\"Sample Name\",\"number\":123}" :: Maybe (WithId SampleType)
            decoded `shouldBe` Nothing
