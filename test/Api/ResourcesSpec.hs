
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Api.ResourcesSpec
(
  spec
)
where

import Control.Applicative        ( (<$>) )
import Data.Monoid                ( (<>) )
import Data.Time.Clock            ( getCurrentTime )
import Network.HTTP.Types.Status  ( ok200, created201, noContent204
                                  , badRequest400, notFound404, conflict409 )
import Network.Wai.Test           ( simpleStatus )
import Test.Hspec                 ( Spec, describe, it, shouldBe, shouldSatisfy )

import Json.Resource              ( Resource(..) )
import Json.WithId                ( WithId(..), getId )
import TestUtility                ( runTest, get, put, post, delete, jsonBody, uri )
import UserTestUtility            ( runTestWithUser, qUid )


deriving instance Show Resource
deriving instance Show a => Show (WithId a)

spec :: Spec
spec = do

    let resourcesBase = uri ["api", "resources"]

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
                status <- simpleStatus <$> app `get` (resourcesBase <> uri ["bad-key"])
                status `shouldBe` notFound404

        it "should give 404 for missing key" $
            runTest $ \app -> do
                status <- simpleStatus <$> app `get` (resourcesBase <> uri ["123"])
                status `shouldBe` notFound404

        it "should give 200 for get resource" $
            runTestWithUser $ \app uid -> do
                let insertBody = Resource { path = "/path/to/res"
                                          , createdByUserId = Nothing
                                          , createdAt = Nothing
                                          , public = True
                                          , contentType = "x-application/any" }
                inserted <- jsonBody <$> (app `post` (resourcesBase <> qUid uid)) insertBody :: IO (WithId Resource)
                let rid = getId inserted
                status <- simpleStatus <$> app `get` (resourcesBase <> uri [rid])
                status `shouldBe` ok200

    describe "add single resource" $ do

        it "should give 400 for missing path" $
            runTestWithUser $ \app uid -> do
                let insertBody = Resource { path = ""
                                          , createdByUserId = Nothing
                                          , createdAt = Nothing
                                          , public = True
                                          , contentType = "x-application/any" }
                status <- simpleStatus <$> (app `post` (resourcesBase <> qUid uid)) insertBody
                status `shouldBe` badRequest400

        it "should give 400 for supplied createdByUserId" $
            runTestWithUser $ \app uid -> do
                let insertBody = Resource { path = "/path/to/res"
                                          , createdByUserId = Just uid
                                          , createdAt = Nothing
                                          , public = True
                                          , contentType = "x-application/any" }
                status <- simpleStatus <$> (app `post` (resourcesBase <> qUid uid)) insertBody
                status `shouldBe` badRequest400

        it "should give 400 for supplied createdAt" $
            runTestWithUser $ \app uid -> do
                now <- getCurrentTime
                let insertBody = Resource { path = "/path/to/res"
                                          , createdByUserId = Nothing
                                          , createdAt = Just now
                                          , public = True
                                          , contentType = "x-application/any" }
                status <- simpleStatus <$> (app `post` (resourcesBase <> qUid uid)) insertBody
                status `shouldBe` badRequest400

        it "should give 201 for add resource" $
            runTestWithUser $ \app uid -> do
                let insertBody = Resource { path = "/path/to/res"
                                          , createdByUserId = Nothing
                                          , createdAt = Nothing
                                          , public = True
                                          , contentType = "x-application/any" }
                status <- simpleStatus <$> (app `post` (resourcesBase <> qUid uid)) insertBody
                status `shouldBe` created201

        it "should give 409 for duplicate path" $
            runTestWithUser $ \app uid -> do
                let insertBody1 = Resource { path = "/path/to/res"
                                          , createdByUserId = Nothing
                                          , createdAt = Nothing
                                          , public = True
                                          , contentType = "x-application/any" }
                status1 <- simpleStatus <$> (app `post` (resourcesBase <> qUid uid)) insertBody1
                status1 `shouldBe` created201
                let insertBody2 = Resource { path = "/path/to/res"
                                          , createdByUserId = Nothing
                                          , createdAt = Nothing
                                          , public = True
                                          , contentType = "x-application/any" }
                status2 <- simpleStatus <$> (app `post` (resourcesBase <> qUid uid)) insertBody2
                status2 `shouldBe` conflict409

    describe "update single resource" $ do

        it "should give 400 for missing name" $
            runTestWithUser $ \app uid -> do
                let insertBody = Resource { path = "/path/to/res"
                                          , createdByUserId = Nothing
                                          , createdAt = Nothing
                                          , public = True
                                          , contentType = "x-application/any" }
                inserted <- jsonBody <$> (app `post` (resourcesBase <> qUid uid)) insertBody :: IO (WithId Resource)
                let rid = getId inserted
                let updateBody = Resource { path = ""
                                          , createdByUserId = Nothing
                                          , createdAt = Nothing
                                          , public = True
                                          , contentType = "x-application/any" }
                status <- simpleStatus <$> (app `put` (resourcesBase <> uri [rid])) updateBody
                status `shouldBe` badRequest400

        it "should give 400 for supplied createdByUserId" $
            runTestWithUser $ \app uid -> do
                let insertBody = Resource { path = "/path/to/res"
                                          , createdByUserId = Nothing
                                          , createdAt = Nothing
                                          , public = True
                                          , contentType = "x-application/any" }
                inserted <- jsonBody <$> (app `post` (resourcesBase <> qUid uid)) insertBody :: IO (WithId Resource)
                let rid = getId inserted
                let updateBody = Resource { path = "/new/path/to/res"
                                          , createdByUserId = Just uid
                                          , createdAt = Nothing
                                          , public = True
                                          , contentType = "x-application/any" }
                status <- simpleStatus <$> (app `put` (resourcesBase <> uri [rid])) updateBody
                status `shouldBe` badRequest400

        it "should give 400 for supplied createdAt" $
            runTestWithUser $ \app uid -> do
                now <- getCurrentTime
                let insertBody = Resource { path = "/path/to/res"
                                          , createdByUserId = Nothing
                                          , createdAt = Nothing
                                          , public = True
                                          , contentType = "x-application/any" }
                inserted <- jsonBody <$> (app `post` (resourcesBase <> qUid uid)) insertBody :: IO (WithId Resource)
                let rid = getId inserted
                let updateBody = Resource { path = "/new/path/to/res"
                                          , createdByUserId = Nothing
                                          , createdAt = Just now
                                          , public = True
                                          , contentType = "x-application/any" }
                status <- simpleStatus <$> (app `put` (resourcesBase <> uri [rid])) updateBody
                status `shouldBe` badRequest400

        it "should give 200 for update resource" $
            runTestWithUser $ \app uid -> do
                let insertBody = Resource { path = "/path/to/res"
                                          , createdByUserId = Nothing
                                          , createdAt = Nothing
                                          , public = True
                                          , contentType = "x-application/any" }
                inserted <- jsonBody <$> (app `post` (resourcesBase <> qUid uid)) insertBody :: IO (WithId Resource)
                let rid = getId inserted
                let updateBody = Resource { path = "/new/path/to/res"
                                          , createdByUserId = Nothing
                                          , createdAt = Nothing
                                          , public = True
                                          , contentType = "x-application/any" }
                status <- simpleStatus <$> (app `put` (resourcesBase <> uri [rid])) updateBody
                status `shouldBe` ok200

    describe "delete single resource" $ do

        it "should give 204 for bad key" $
            runTest $ \app -> do
                status <- simpleStatus <$> app `delete` (resourcesBase <> uri ["bad-key"])
                status `shouldBe` noContent204

        it "should give 204 for missing key" $
            runTest $ \app -> do
                status <- simpleStatus <$> app `delete` (resourcesBase <> uri ["123"])
                status `shouldBe` noContent204

        it "should give 204 for successful delete resource" $
            runTestWithUser $ \app uid -> do
                let insertBody = Resource { path = "/path/to/res"
                                          , createdByUserId = Nothing
                                          , createdAt = Nothing
                                          , public = True
                                          , contentType = "x-application/any" }
                inserted <- jsonBody <$> (app `post` (resourcesBase <> qUid uid)) insertBody :: IO (WithId Resource)
                let rid = getId inserted
                status <- simpleStatus <$> app `delete` (resourcesBase <> uri [rid])
                status `shouldBe` noContent204

        it "should actually delete the item" $
            runTestWithUser $ \app uid -> do
                let insertBody = Resource { path = "/path/to/res"
                                          , createdByUserId = Nothing
                                          , createdAt = Nothing
                                          , public = True
                                          , contentType = "x-application/any" }
                inserted <- jsonBody <$> (app `post` (resourcesBase <> qUid uid)) insertBody :: IO (WithId Resource)
                let rid = getId inserted
                get1status <- simpleStatus <$> app `get` (resourcesBase <> uri [rid])
                get1status `shouldBe` ok200
                status <- simpleStatus <$> app `delete` (resourcesBase <> uri [rid])
                status `shouldBe` noContent204
                get2status <- simpleStatus <$> app `get` (resourcesBase <> uri [rid])
                get2status `shouldBe` notFound404
