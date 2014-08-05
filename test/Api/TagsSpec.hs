
{-# LANGUAGE OverloadedStrings #-}

module Api.TagsSpec
(
  spec
)
where

import Control.Applicative        ( (<$>) )
import Network.HTTP.Types.Status  ( ok200, created201, noContent204
                                  , badRequest400, notFound404, conflict409 )
import Network.Wai.Test           ( simpleStatus )
import Test.Hspec                 ( Spec, describe, it, shouldBe, shouldSatisfy )

import TestUtility                ( runTest, get, put, post, delete, jsonBody )
import Json.Tag                   ( Tag(..) )
import Json.WithId                ( WithId(..), getId )
import Uri                        ( (+/+) )


spec :: Spec
spec = do

    let tagsBase = "/api/tags"

    describe "get all tags" $ do

        it "should give 200" $
            runTest $ \app -> do
                status <- simpleStatus <$> app `get` tagsBase
                status `shouldBe` ok200

        it "should give empty list" $
            runTest $ \app -> do
                tags <- (jsonBody <$> app `get` tagsBase) :: IO [WithId Tag]
                tags `shouldSatisfy` null

    describe "get single tag" $ do

        it "should give 404 for bad key" $
            runTest $ \app -> do
                status <- simpleStatus <$> app `get` (tagsBase +/+ "bad-key")
                status `shouldBe` notFound404

        it "should give 404 for missing key" $
            runTest $ \app -> do
                status <- simpleStatus <$> app `get` (tagsBase +/+ "123")
                status `shouldBe` notFound404

        it "should give 200 for get tag" $
            runTest $ \app -> do
                let insertBody = Tag "tag-name"
                inserted <- jsonBody <$> (app `post` tagsBase) insertBody :: IO (WithId Tag)
                let tid = getId inserted
                status <- simpleStatus <$> app `get` (tagsBase +/+ tid)
                status `shouldBe` ok200

    describe "add single tag" $ do

        it "should give 400 for missing name" $
            runTest $ \app -> do
                let insertBody = Tag ""
                status <- simpleStatus <$> (app `post` tagsBase) insertBody
                status `shouldBe` badRequest400

        it "should give 201 for add tag" $
            runTest $ \app -> do
                let insertBody = Tag "tag-name"
                status <- simpleStatus <$> (app `post` tagsBase) insertBody
                status `shouldBe` created201

        it "should give 409 for duplicate name" $
            runTest $ \app -> do
                let insertBody1 = Tag "tag-name"
                status1 <- simpleStatus <$> (app `post` tagsBase) insertBody1
                status1 `shouldBe` created201
                let insertBody2 = Tag "tag-name"
                status2 <- simpleStatus <$> (app `post` tagsBase) insertBody2
                status2 `shouldBe` conflict409

    describe "update single tag" $ do

        it "should give 400 for missing name" $
            runTest $ \app -> do
                let insertBody = Tag "tag-name"
                inserted <- jsonBody <$> (app `post` tagsBase) insertBody :: IO (WithId Tag)
                let uid = getId inserted
                let updateBody = Tag ""
                status <- simpleStatus <$> (app `put` (tagsBase +/+ uid)) updateBody
                status `shouldBe` badRequest400

        it "should give 200 for update tag" $
            runTest $ \app -> do
                let insertBody = Tag "tag-name"
                inserted <- jsonBody <$> (app `post` tagsBase) insertBody :: IO (WithId Tag)
                let uid = getId inserted
                let updateBody = Tag "new-tag-name"
                status <- simpleStatus <$> (app `put` (tagsBase +/+ uid)) updateBody
                status `shouldBe` ok200

    describe "delete single tag" $ do

        it "should give 204 for bad key" $
            runTest $ \app -> do
                status <- simpleStatus <$> app `delete` (tagsBase +/+ "bad-key")
                status `shouldBe` noContent204

        it "should give 204 for missing key" $
            runTest $ \app -> do
                status <- simpleStatus <$> app `delete` (tagsBase +/+ "123")
                status `shouldBe` noContent204

        it "should give 204 for successful delete tag" $
            runTest $ \app -> do
                let insertBody = Tag "tag-name"
                inserted <- jsonBody <$> (app `post` tagsBase) insertBody :: IO (WithId Tag)
                let uid = getId inserted
                status <- simpleStatus <$> app `delete` (tagsBase +/+ uid)
                status `shouldBe` noContent204

        it "should actually delete the item" $
            runTest $ \app -> do
                let insertBody = Tag "tag-name"
                inserted <- jsonBody <$> (app `post` tagsBase) insertBody :: IO (WithId Tag)
                let uid = getId inserted
                get1status <- simpleStatus <$> app `get` (tagsBase +/+ uid)
                get1status `shouldBe` ok200
                status <- simpleStatus <$> app `delete` (tagsBase +/+ uid)
                status `shouldBe` noContent204
                get2status <- simpleStatus <$> app `get` (tagsBase +/+ uid)
                get2status `shouldBe` notFound404
