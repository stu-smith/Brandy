
{-# LANGUAGE OverloadedStrings #-}

module Api.TagsSpec
(
  spec
)
where

import Control.Applicative        ( (<$>) )
import Network.HTTP.Types.Status  ( ok200, badRequest400, notFound404, conflict409 )
import Network.Wai.Test           ( simpleStatus )
import Test.Hspec                 ( Spec, describe, it, shouldBe, shouldSatisfy )

import TestUtility                ( runTest, get, post, jsonBody )
import Json.Tag                   ( Tag(..) )
import Json.WithId                ( WithId(..), getId )
import Uri                        ( (+/+) )


spec :: Spec
spec = do

    let apiBase = "/api/tags"

    describe "get all tags" $ do

        it "should give 200" $
            runTest $ \app -> do
                status <- simpleStatus <$> app `get` apiBase
                status `shouldBe` ok200

        it "should give empty list" $
            runTest $ \app -> do
                tags <- (jsonBody <$> app `get` apiBase) :: IO [WithId Tag]
                tags `shouldSatisfy` null

    describe "get single tag" $ do

        it "should give 404 for bad key" $
            runTest $ \app -> do
                status <- simpleStatus <$> app `get` (apiBase +/+ "bad-key")
                status `shouldBe` notFound404

        it "should give 404 for missing key" $
            runTest $ \app -> do
                status <- simpleStatus <$> app `get` (apiBase +/+ "123")
                status `shouldBe` notFound404

        it "should give 200 for get tag" $
            runTest $ \app -> do
                let insertBody = Tag "tag-name"
                inserted <- jsonBody <$> (app `post` apiBase) insertBody :: IO (WithId Tag)
                let tid = getId inserted
                status <- simpleStatus <$> app `get` (apiBase +/+ tid)
                status `shouldBe` ok200

    describe "add single tag" $ do

        it "should give 400 for missing name" $
            runTest $ \app -> do
                let insertBody = Tag ""
                status <- simpleStatus <$> (app `post` apiBase) insertBody
                status `shouldBe` badRequest400

        it "should give 200 for add tag" $
            runTest $ \app -> do
                let insertBody = Tag "tag-name"
                status <- simpleStatus <$> (app `post` apiBase) insertBody
                status `shouldBe` ok200

        it "should give 409 for duplicate name" $
            runTest $ \app -> do
                let insertBody1 = Tag "tag-name"
                status1 <- simpleStatus <$> (app `post` apiBase) insertBody1
                status1 `shouldBe` ok200
                let insertBody2 = Tag "tag-name"
                status2 <- simpleStatus <$> (app `post` apiBase) insertBody2
                status2 `shouldBe` conflict409
