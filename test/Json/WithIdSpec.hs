
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Json.WithIdSpec
(
    spec
)
where

import Data.Aeson    ( ToJSON, FromJSON, encode, decode )
import Data.Maybe    ( fromJust )
import GHC.Generics  ( Generic )
import Test.Hspec    ( Spec, describe, it, shouldBe )
import qualified Data.Text as T
                     ( Text )

import Json.WithId   ( WithId(..) )

deriving instance Show a => Show (WithId a)
deriving instance Eq a => Eq (WithId a)

spec :: Spec
spec =

    describe "WithId" $ do

        it "should serialize data and include id" $ do
            let withoutId = SampleType { name = "Sample Name", number = 123 }
            encode withoutId `shouldBe` "{\"name\":\"Sample Name\",\"number\":123}"
            let withId = WithId "XYZ" withoutId
            encode withId `shouldBe` "{\"name\":\"Sample Name\",\"id\":\"XYZ\",\"number\":123}"

        it "should deserialize data that includes id" $ do
            let decoded = decode "{\"name\":\"Sample Name\",\"id\":\"XYZ\",\"number\":123}" :: Maybe (WithId SampleType)
            (encode . fromJust $ decoded) `shouldBe` "{\"name\":\"Sample Name\",\"id\":\"XYZ\",\"number\":123}"

        it "should not deserialize data that does not includes id" $ do
            let decoded = decode "{\"name\":\"Sample Name\",\"number\":123}" :: Maybe (WithId SampleType)
            decoded `shouldBe` Nothing

data SampleType = SampleType
    {
      name   :: T.Text
    , number :: Int
    }
  deriving (Generic, Show, Eq)

instance ToJSON SampleType
instance FromJSON SampleType
