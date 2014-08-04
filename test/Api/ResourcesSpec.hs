
{-# LANGUAGE OverloadedStrings #-}

module Api.ResourcesSpec
(
  spec
)
where

import Control.Applicative        ( (<$>) )
import Network.HTTP.Types.Status  ( ok200, noContent204, badRequest400, notFound404, conflict409 )
import Network.Wai.Test           ( simpleStatus )
import Test.Hspec                 ( Spec, describe, it, shouldBe, shouldSatisfy )

import Json.Resource              ( Resource(..) )
import Json.WithId                ( WithId(..), getId )
import TestUtility                ( runTest, get, put, post, delete, jsonBody )
import Uri                        ( (+/+) )
import UserTestUtility            ( runTestWithUser )


spec :: Spec
spec = do

    let resourcesBase = "/api/resources"

    describe "get all resources" $ do

        it "should give 200" $
            runTest $ \app -> do
                status <- simpleStatus <$> app `get` resourcesBase
                status `shouldBe` ok200

        it "should give empty list" $
            runTest $ \app -> do
                resources <- (jsonBody <$> app `get` resourcesBase) :: IO [WithId Resource]
                resources `shouldSatisfy` null

    describe "get single resource" $ do

        it "should give 404 for bad key" $
            runTest $ \app -> do
                status <- simpleStatus <$> app `get` (resourcesBase +/+ "bad-key")
                status `shouldBe` notFound404

        it "should give 404 for missing key" $
            runTest $ \app -> do
                status <- simpleStatus <$> app `get` (resourcesBase +/+ "123")
                status `shouldBe` notFound404

        it "should give 200 for get resource" $
            runTestWithUser $ \app uid -> do
                let insertBody = Resource { path = "/path/to/res"
                                          , createdByUserId = uid
                                          , createdAt = Nothing
                                          , public = True
                                          , contentType = "x-application/any" }
                inserted <- jsonBody <$> (app `post` resourcesBase) insertBody :: IO (WithId Resource)
                let tid = getId inserted
                status <- simpleStatus <$> app `get` (resourcesBase +/+ tid)
                status `shouldBe` ok200

    describe "add single resource" $ do

        it "should give 400 for missing path" $
            runTestWithUser $ \app uid -> do
                let insertBody = Resource { path = ""
                                          , createdByUserId = uid
                                          , createdAt = Nothing
                                          , public = True
                                          , contentType = "x-application/any" }
                status <- simpleStatus <$> (app `post` resourcesBase) insertBody
                status `shouldBe` badRequest400

        it "should give 200 for add resource" $
            runTestWithUser $ \app uid -> do
                let insertBody = Resource { path = "/path/to/res"
                                          , createdByUserId = uid
                                          , createdAt = Nothing
                                          , public = True
                                          , contentType = "x-application/any" }
                status <- simpleStatus <$> (app `post` resourcesBase) insertBody
                status `shouldBe` ok200

        it "should give 409 for duplicate path" $
            runTestWithUser $ \app uid -> do
                let insertBody1 = Resource { path = "/path/to/res"
                                          , createdByUserId = uid
                                          , createdAt = Nothing
                                          , public = True
                                          , contentType = "x-application/any" }
                status1 <- simpleStatus <$> (app `post` resourcesBase) insertBody1
                status1 `shouldBe` ok200
                let insertBody2 = Resource { path = "/path/to/res"
                                          , createdByUserId = uid
                                          , createdAt = Nothing
                                          , public = True
                                          , contentType = "x-application/any" }
                status2 <- simpleStatus <$> (app `post` resourcesBase) insertBody2
                status2 `shouldBe` conflict409

    describe "update single resource" $ do

        it "should give 400 for missing name" $
            runTestWithUser $ \app uid -> do
                let insertBody = Resource { path = "/path/to/res"
                                          , createdByUserId = uid
                                          , createdAt = Nothing
                                          , public = True
                                          , contentType = "x-application/any" }
                inserted <- jsonBody <$> (app `post` resourcesBase) insertBody :: IO (WithId Resource)
                let rid = getId inserted
                let updateBody = Resource { path = ""
                                          , createdByUserId = uid
                                          , createdAt = Nothing
                                          , public = True
                                          , contentType = "x-application/any" }
                status <- simpleStatus <$> (app `put` (resourcesBase +/+ rid)) updateBody
                status `shouldBe` badRequest400

        it "should give 200 for update resource" $
            runTestWithUser $ \app uid -> do
                let insertBody = Resource { path = "/path/to/res"
                                          , createdByUserId = uid
                                          , createdAt = Nothing
                                          , public = True
                                          , contentType = "x-application/any" }
                inserted <- jsonBody <$> (app `post` resourcesBase) insertBody :: IO (WithId Resource)
                let rid = getId inserted
                let updateBody = Resource { path = "/new/path/to/res"
                                          , createdByUserId = uid
                                          , createdAt = Nothing
                                          , public = True
                                          , contentType = "x-application/any" }
                status <- simpleStatus <$> (app `put` (resourcesBase +/+ rid)) updateBody
                status `shouldBe` ok200

    describe "delete single resource" $ do

        it "should give 204 for bad key" $
            runTest $ \app -> do
                status <- simpleStatus <$> app `delete` (resourcesBase +/+ "bad-key")
                status `shouldBe` noContent204

        it "should give 204 for missing key" $
            runTest $ \app -> do
                status <- simpleStatus <$> app `delete` (resourcesBase +/+ "123")
                status `shouldBe` noContent204

        it "should give 204 for successful delete resource" $
            runTestWithUser $ \app uid -> do
                let insertBody = Resource { path = "/path/to/res"
                                          , createdByUserId = uid
                                          , createdAt = Nothing
                                          , public = True
                                          , contentType = "x-application/any" }
                inserted <- jsonBody <$> (app `post` resourcesBase) insertBody :: IO (WithId Resource)
                let rid = getId inserted
                status <- simpleStatus <$> app `delete` (resourcesBase +/+ rid)
                status `shouldBe` noContent204

        it "should actually delete the item" $
            runTestWithUser $ \app uid -> do
                let insertBody = Resource { path = "/path/to/res"
                                          , createdByUserId = uid
                                          , createdAt = Nothing
                                          , public = True
                                          , contentType = "x-application/any" }
                inserted <- jsonBody <$> (app `post` resourcesBase) insertBody :: IO (WithId Resource)
                let rid = getId inserted
                get1status <- simpleStatus <$> app `get` (resourcesBase +/+ rid)
                get1status `shouldBe` ok200
                status <- simpleStatus <$> app `delete` (resourcesBase +/+ rid)
                status `shouldBe` noContent204
                get2status <- simpleStatus <$> app `get` (resourcesBase +/+ rid)
                get2status `shouldBe` notFound404
