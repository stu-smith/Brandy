
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

import Json.PrivateUserPre        ( PrivateUserPre(..) )
import Json.PublicUserSummary     ( PublicUserSummary )
import TestUtility                ( runTest, get, post )


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

    describe "add single user" $ do

        it "should give 400 for missing displayName" $
            runTest $ \app -> do
                let payload = PrivateUserPre "" "email@example.com"
                status <- simpleStatus <$> (app `post` "/api/users") payload
                status `shouldBe` badRequest400

        it "should give 400 for missing email" $
            runTest $ \app -> do
                let payload = PrivateUserPre "Display Name" ""
                status <- simpleStatus <$> (app `post` "/api/users") payload
                status `shouldBe` badRequest400

        it "should give 200 for add user" $
            runTest $ \app -> do
                let payload = PrivateUserPre "Display Name" "email@example.com"
                status <- simpleStatus <$> (app `post` "/api/users") payload
                status `shouldBe` ok200

        it "should give 400 for duplicate email" $
            runTest $ \app -> do
                let payload1 = PrivateUserPre "Display Name 1" "email@example.com"
                status1 <- simpleStatus <$> (app `post` "/api/users") payload1
                status1 `shouldBe` ok200
                let payload2 = PrivateUserPre "Display Name 2" "email@example.com"
                status2 <- simpleStatus <$> (app `post` "/api/users") payload2
                status2 `shouldBe` badRequest400

        it "should give 400 for duplicate displayName" $
            runTest $ \app -> do
                let payload1 = PrivateUserPre "Display Name" "email1@example.com"
                status1 <- simpleStatus <$> (app `post` "/api/users") payload1
                status1 `shouldBe` ok200
                let payload2 = PrivateUserPre "Display Name" "email2@example.com"
                status2 <- simpleStatus <$> (app `post` "/api/users") payload2
                status2 `shouldBe` badRequest400
