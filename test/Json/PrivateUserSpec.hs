
{-# LANGUAGE OverloadedStrings #-}

module Json.PrivateUserSpec
(
  spec
)
where

import Data.Aeson  ( encode, decode )
import Test.Hspec  ( Spec, describe, it, shouldBe )

import qualified Json.PrivateUser as PU
                   ( PrivateUser(..) )


spec :: Spec
spec =

    describe "json" $ do

        it "should serialize correctly" $ do
            let v = PU.PrivateUser
                        { PU.id          = "1234"
                        , PU.displayName = "DN"
                        , PU.email       = "EM"
                        }
            encode v `shouldBe` "{\"email\":\"EM\",\"displayName\":\"DN\",\"id\":\"1234\"}"

        it "should deserialize correctly" $ do
            let v = PU.PrivateUser
                        { PU.id          = "1234"
                        , PU.displayName = "DN"
                        , PU.email       = "EM"
                        }
            decode "{\"email\":\"EM\",\"displayName\":\"DN\",\"id\":\"1234\"}" `shouldBe` Just v
