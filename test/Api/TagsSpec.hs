
{-# LANGUAGE OverloadedStrings #-}

module Api.TagsSpec
(
  spec
)
where

import Control.Applicative        ( (<$>) )
import Network.HTTP.Types.Status  ( ok200, notFound404 )
import Network.Wai.Test           ( simpleStatus )
import Test.Hspec                 ( Spec, describe, it, shouldBe, shouldSatisfy )

import TestUtility                ( runTest, get, post, jsonBody )
import Json.Tag                   ( Tag(..) )
import Json.WithId                ( WithId(..), getId )
import Uri                        ( (+/+) )


spec :: Spec
spec = do

    describe "get all tags" $ do

        it "should give 200" $
            runTest $ \app -> do
                status <- simpleStatus <$> app `get` "/api/tags"
                status `shouldBe` ok200

        it "should give empty list" $
            runTest $ \app -> do
                tags <- (jsonBody <$> app `get` "/api/tags") :: IO [WithId Tag]
                tags `shouldSatisfy` null

    describe "get single tag" $ do

        it "should give 404 for bad key" $
            runTest $ \app -> do
                status <- simpleStatus <$> app `get` "/api/tags/bad-key"
                status `shouldBe` notFound404

        it "should give 404 for missing key" $
            runTest $ \app -> do
                status <- simpleStatus <$> app `get` "/api/tags/123"
                status `shouldBe` notFound404

        it "should give 200 for get tag" $
            runTest $ \app -> do
                let insertBody = Tag "tag-name"
                inserted <- jsonBody <$> (app `post` "/api/tags") insertBody :: IO (WithId Tag)
                let tid = getId inserted
                status <- simpleStatus <$> app `get` ("/api/tags" +/+ tid)
                status `shouldBe` ok200
