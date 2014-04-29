
{-# LANGUAGE OverloadedStrings #-}

module Json.PrivateUserPreSpec
(
  spec
)
where

import Data.Aeson           ( encode, decode )
import Test.Hspec           ( Spec, describe, it, shouldBe )

import Json.PrivateUserPre  ( PrivateUserPre(..) )


spec :: Spec
spec =

    describe "json" $ do

        it "should serialize correctly" $ do
            let v = PrivateUserPre
                        { displayName = "DN"
                        , email = "EM"
                        }
            encode v `shouldBe` "{\"email\":\"EM\",\"displayName\":\"DN\"}"

        it "should deserialize correctly" $ do
            let v = PrivateUserPre
                        { displayName = "DN"
                        , email = "EM"
                        }
            decode "{\"email\":\"EM\",\"displayName\":\"DN\"}" `shouldBe` Just v
