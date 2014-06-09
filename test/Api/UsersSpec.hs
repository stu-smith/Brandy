
{-# LANGUAGE OverloadedStrings #-}

module Api.UsersSpec
(
  spec
)
where

import Control.Applicative         ( (<$>) )
import Network.HTTP.Types.Status   ( ok200, badRequest400, notFound404, conflict409 )
import Network.Wai.Test            ( simpleStatus )
import Test.Hspec                  ( Spec, describe, it, shouldBe, shouldSatisfy )

import qualified Json.PrivateUser  ( PrivateUser(..) )
import Json.PrivateUserPre         ( PrivateUserPre(..) )
import Json.PublicUserSummary      ( PublicUserSummary )
import Uri                         ( (+/+) )
import TestUtility                 ( runTest, get, post, put, jsonBody )


spec :: Spec
spec = do

    describe "get all users" $ do

        it "should give 200" $
            runTest $ \app -> do
                status <- simpleStatus <$> app `get` "/api/users"
                status `shouldBe` ok200

        it "should give empty list" $
            runTest $ \app -> do
                users <- (jsonBody <$> app `get` "/api/users") :: IO [PublicUserSummary]
                users `shouldSatisfy` null

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
                let insertBody = PrivateUserPre "" "email@example.com"
                status <- simpleStatus <$> (app `post` "/api/users") insertBody
                status `shouldBe` badRequest400

        it "should give 400 for missing email" $
            runTest $ \app -> do
                let insertBody = PrivateUserPre "Display Name" ""
                status <- simpleStatus <$> (app `post` "/api/users") insertBody
                status `shouldBe` badRequest400

        it "should give 200 for add user" $
            runTest $ \app -> do
                let insertBody = PrivateUserPre "Display Name" "email@example.com"
                status <- simpleStatus <$> (app `post` "/api/users") insertBody
                status `shouldBe` ok200

        it "should give 409 for duplicate email" $
            runTest $ \app -> do
                let insertBody1 = PrivateUserPre "Display Name 1" "email@example.com"
                status1 <- simpleStatus <$> (app `post` "/api/users") insertBody1
                status1 `shouldBe` ok200
                let insertBody2 = PrivateUserPre "Display Name 2" "email@example.com"
                status2 <- simpleStatus <$> (app `post` "/api/users") insertBody2
                status2 `shouldBe` conflict409

        it "should give 409 for duplicate displayName" $
            runTest $ \app -> do
                let insertBody1 = PrivateUserPre "Display Name" "email1@example.com"
                status1 <- simpleStatus <$> (app `post` "/api/users") insertBody1
                status1 `shouldBe` ok200
                let insertBody2 = PrivateUserPre "Display Name" "email2@example.com"
                status2 <- simpleStatus <$> (app `post` "/api/users") insertBody2
                status2 `shouldBe` conflict409

    describe "update single user" $ do

        it "should give 400 for missing displayName" $
            runTest $ \app -> do
                let insertBody = PrivateUserPre "Display Name" "email@example.com"
                inserted <- jsonBody <$> (app `post` "/api/users") insertBody
                let uid = Json.PrivateUser.id inserted
                let updateBody = PrivateUserPre "" "newemail@example.com"
                status <- simpleStatus <$> (app `put` ("/api/users" +/+ uid)) updateBody
                status `shouldBe` badRequest400

        it "should give 400 for missing email" $
            runTest $ \app -> do
                let insertBody = PrivateUserPre "Display Name" "email@example.com"
                inserted <- jsonBody <$> (app `post` "/api/users") insertBody
                let uid = Json.PrivateUser.id inserted
                let updateBody = PrivateUserPre "New Display Name" ""
                status <- simpleStatus <$> (app `put` ("/api/users" +/+ uid)) updateBody
                status `shouldBe` badRequest400

        it "should give 200 for update user" $
            runTest $ \app -> do
                let insertBody = PrivateUserPre "Display Name" "email@example.com"
                inserted <- jsonBody <$> (app `post` "/api/users") insertBody
                let uid = Json.PrivateUser.id inserted
                let updateBody = PrivateUserPre "New Display Name" "newemail@example.com"
                status <- simpleStatus <$> (app `put` ("/api/users" +/+ uid)) updateBody
                status `shouldBe` ok200
