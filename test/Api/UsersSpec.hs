
{-# LANGUAGE OverloadedStrings #-}

module Api.UsersSpec
(
  spec
)
where

import Control.Applicative        ( (<$>) )
import Data.Aeson                 ( decode )
import Data.Maybe                 ( fromJust )
import Network.HTTP.Types.Status  ( ok200, badRequest400, notFound404 )
import Network.Wai.Test           ( simpleStatus, simpleBody )
import Test.Hspec                 ( Spec, describe, it, shouldBe, shouldSatisfy )

import Json.PublicUserSummary     ( PublicUserSummary )
import Utility                    ( runTest, get )


spec :: Spec
spec = do

    describe "get all users" $ do

        it "should give 200" $
            runTest $ \app -> do
                status <- simpleStatus <$> app `get` "/api/users"
                status `shouldBe` ok200

        it "should give empty list" $
            runTest $ \app -> do
                body <- simpleBody <$> app `get` "/api/users"
                let users = decode body :: Maybe [PublicUserSummary]
                fromJust users `shouldSatisfy` null

    describe "get single user" $ do

        it "should give 400 for bad key" $
            runTest $ \app -> do
                status <- simpleStatus <$> app `get` "/api/users/bad-key"
                status `shouldBe` badRequest400

        it "should give 404 for missing key" $
            runTest $ \app -> do
                status <- simpleStatus <$> app `get` "/api/users/123"
                status `shouldBe` notFound404
