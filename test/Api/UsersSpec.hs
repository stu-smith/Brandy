
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Api.UsersSpec
(
  spec
)
where

import Control.Applicative        ( (<$>) )
import Data.Aeson                 ( FromJSON, ToJSON, toJSON )
import Data.Monoid                ( (<>) )
import Network.HTTP.Types.Status  ( ok200, created201, noContent204
                                  , badRequest400, notFound404, conflict409 )
import Network.Wai.Test           ( simpleStatus )
import Test.Hspec                 ( Spec, describe, it, shouldBe, shouldSatisfy )

import Json.PrivateUser           ( PrivateUser(..) )
import Json.PublicUserSummary     ( PublicUserSummary(..) )
import Json.WithId                ( WithId, getId )
import TestUtility                ( runTest, get, post, put, delete, jsonBody, uri )


deriving instance Show PublicUserSummary

instance (ToJSON j, FromJSON j) => Show (WithId j) where
    show = show . toJSON

instance (Eq j, ToJSON j, FromJSON j) => Eq (WithId j) where
    x == y = (show $ toJSON x) == (show $ toJSON y)


spec :: Spec
spec = do

    let usersBase = uri ["api", "users"]

    describe "get all users" $ do

        it "should give 200" $
            runTest $ \app -> do
                status <- simpleStatus <$> app `get` usersBase
                status `shouldBe` ok200

        it "should give empty list" $
            runTest $ \app -> do
                users <- (jsonBody <$> app `get` usersBase) :: IO [WithId PublicUserSummary]
                users `shouldSatisfy` null

    describe "get single user" $ do

        it "should give 404 for bad key" $
            runTest $ \app -> do
                status <- simpleStatus <$> app `get` (usersBase <> uri ["bad-key"])
                status `shouldBe` notFound404

        it "should give 404 for missing key" $
            runTest $ \app -> do
                status <- simpleStatus <$> app `get` (usersBase <> uri ["123"])
                status `shouldBe` notFound404

        it "should give 200 for get user" $
            runTest $ \app -> do
                let insertBody = PrivateUser "Display Name" "email@example.com"
                inserted <- jsonBody <$> (app `post` usersBase) insertBody :: IO (WithId PrivateUser)
                let uid = getId inserted
                status <- simpleStatus <$> app `get` (usersBase <> uri [uid])
                status `shouldBe` ok200

    describe "add single user" $ do

        it "should give 400 for missing displayName" $
            runTest $ \app -> do
                let insertBody = PrivateUser "" "email@example.com"
                status <- simpleStatus <$> (app `post` usersBase) insertBody
                status `shouldBe` badRequest400

        it "should give 400 for missing email" $
            runTest $ \app -> do
                let insertBody = PrivateUser "Display Name" ""
                status <- simpleStatus <$> (app `post` usersBase) insertBody
                status `shouldBe` badRequest400

        it "should give 201 for add user" $
            runTest $ \app -> do
                let insertBody = PrivateUser "Display Name" "email@example.com"
                status <- simpleStatus <$> (app `post` usersBase) insertBody
                status `shouldBe` created201

        it "should give 409 for duplicate email" $
            runTest $ \app -> do
                let insertBody1 = PrivateUser "Display Name 1" "email@example.com"
                status1 <- simpleStatus <$> (app `post` usersBase) insertBody1
                status1 `shouldBe` created201
                let insertBody2 = PrivateUser "Display Name 2" "email@example.com"
                status2 <- simpleStatus <$> (app `post` usersBase) insertBody2
                status2 `shouldBe` conflict409

        it "should give 409 for duplicate displayName" $
            runTest $ \app -> do
                let insertBody1 = PrivateUser "Display Name" "email1@example.com"
                status1 <- simpleStatus <$> (app `post` usersBase) insertBody1
                status1 `shouldBe` created201
                let insertBody2 = PrivateUser "Display Name" "email2@example.com"
                status2 <- simpleStatus <$> (app `post` usersBase) insertBody2
                status2 `shouldBe` conflict409

    describe "update single user" $ do

        it "should give 400 for missing displayName" $
            runTest $ \app -> do
                let insertBody = PrivateUser "Display Name" "email@example.com"
                inserted <- jsonBody <$> (app `post` usersBase) insertBody :: IO (WithId PrivateUser)
                let uid = getId inserted
                let updateBody = PrivateUser "" "newemail@example.com"
                status <- simpleStatus <$> (app `put` (usersBase <> uri [uid])) updateBody
                status `shouldBe` badRequest400

        it "should give 400 for missing email" $
            runTest $ \app -> do
                let insertBody = PrivateUser "Display Name" "email@example.com"
                inserted <- jsonBody <$> (app `post` usersBase) insertBody :: IO (WithId PrivateUser)
                let uid = getId inserted
                let updateBody = PrivateUser "New Display Name" ""
                status <- simpleStatus <$> (app `put` (usersBase <> uri [uid])) updateBody
                status `shouldBe` badRequest400

        it "should give 200 for update user" $
            runTest $ \app -> do
                let insertBody = PrivateUser "Display Name" "email@example.com"
                inserted <- jsonBody <$> (app `post` usersBase) insertBody :: IO (WithId PrivateUser)
                let uid = getId inserted
                let updateBody = PrivateUser "New Display Name" "newemail@example.com"
                status <- simpleStatus <$> (app `put` (usersBase <> uri [uid])) updateBody
                status `shouldBe` ok200

    describe "delete single user" $ do

        it "should give 204 for bad key" $
            runTest $ \app -> do
                status <- simpleStatus <$> app `delete` (usersBase <> uri ["bad-key"])
                status `shouldBe` noContent204

        it "should give 204 for missing key" $
            runTest $ \app -> do
                status <- simpleStatus <$> app `delete` (usersBase <> uri ["123"])
                status `shouldBe` noContent204

        it "should give 204 for successful delete user" $
            runTest $ \app -> do
                let insertBody = PrivateUser "Display Name" "email@example.com"
                inserted <- jsonBody <$> (app `post` usersBase) insertBody :: IO (WithId PrivateUser)
                let uid = getId inserted
                status <- simpleStatus <$> app `delete` (usersBase <> uri [uid])
                status `shouldBe` noContent204

        it "should actually delete the item" $
            runTest $ \app -> do
                let insertBody = PrivateUser "Display Name" "email@example.com"
                inserted <- jsonBody <$> (app `post` usersBase) insertBody :: IO (WithId PrivateUser)
                let uid = getId inserted
                get1status <- simpleStatus <$> app `get` (usersBase <> uri [uid])
                get1status `shouldBe` ok200
                status <- simpleStatus <$> app `delete` (usersBase <> uri [uid])
                status `shouldBe` noContent204
                get2status <- simpleStatus <$> app `get` (usersBase <> uri [uid])
                get2status `shouldBe` notFound404
