
{-# LANGUAGE OverloadedStrings #-}

module Api.ResourceContentSpec
(
  spec
)
where

import Control.Applicative        ( (<$>) )
import qualified Data.ByteString as BS
                                  ( empty )
import qualified Data.ByteString.Lazy as BSL
                                  ( null )
import Data.Monoid                ( (<>) )
import Network.HTTP.Types.Status  ( ok200, noContent204, notFound404, methodNotAllowed405 )
import Network.Wai.Test           ( simpleStatus, simpleBody )
import Test.Hspec                 ( Spec, describe, it, shouldBe, shouldSatisfy )

import Json.Resource              ( Resource(..) )
import Json.WithId                ( WithId, getId )
import TestUtility                ( runTest, get, putRaw, post, postRaw, delete, uri, jsonBody, simpleHeader )
import UserTestUtility            ( runTestWithUser, qUid )


spec :: Spec
spec = do

    let resourcesBase = uri ["api", "resources"]

    describe "get resource content" $ do

        it "should give 404 for bad key" $
            runTest $ \app -> do
                status <- simpleStatus <$> app `get` (resourcesBase <> uri ["bad-key", "content"])
                status `shouldBe` notFound404

        it "should give 200 for valid get" $
            runTestWithUser $ \app uid -> do
                let insertBody = Resource { path = "/path/to/res"
                                          , createdByUserId = Nothing
                                          , createdAt = Nothing
                                          , public = True
                                          , contentType = "x-application/any" }
                inserted <- jsonBody <$> (app `post` (resourcesBase <> qUid uid)) insertBody :: IO (WithId Resource)
                let rid = getId inserted
                status <- simpleStatus <$> app `get` (resourcesBase <> uri [rid, "content"])
                status `shouldBe` ok200

        it "should have empty body for initial get" $
            runTestWithUser $ \app uid -> do
                let insertBody = Resource { path = "/path/to/res"
                                          , createdByUserId = Nothing
                                          , createdAt = Nothing
                                          , public = True
                                          , contentType = "x-application/any" }
                inserted <- jsonBody <$> (app `post` (resourcesBase <> qUid uid)) insertBody :: IO (WithId Resource)
                let rid = getId inserted
                body <- simpleBody <$> app `get` (resourcesBase <> uri [rid, "content"])
                body `shouldSatisfy` BSL.null

        it "should give correct content-type" $
            runTestWithUser $ \app uid -> do
                let insertBody = Resource { path = "/path/to/res"
                                          , createdByUserId = Nothing
                                          , createdAt = Nothing
                                          , public = True
                                          , contentType = "x-application/any" }
                inserted <- jsonBody <$> (app `post` (resourcesBase <> qUid uid)) insertBody :: IO (WithId Resource)
                let rid = getId inserted
                ct <- simpleHeader "content-type" <$> (app `get` (resourcesBase <> uri [rid, "content"]))
                ct `shouldBe` "x-application/any"

        it "should give correct body" $
            runTestWithUser $ \app uid -> do
                let insertBody = Resource { path = "/path/to/res"
                                          , createdByUserId = Nothing
                                          , createdAt = Nothing
                                          , public = True
                                          , contentType = "x-application/any" }
                inserted <- jsonBody <$> (app `post` (resourcesBase <> qUid uid)) insertBody :: IO (WithId Resource)
                let rid = getId inserted
                _ <- id <$> (app `putRaw` (resourcesBase <> uri [rid, "content"])) "Hello world"
                body <- simpleBody <$> app `get` (resourcesBase <> uri [rid, "content"])
                body `shouldBe` "Hello world"

    describe "post resource content" $ do

        it "should give 405" $
            runTestWithUser $ \app uid -> do
                let insertBody = Resource { path = "/path/to/res"
                                          , createdByUserId = Nothing
                                          , createdAt = Nothing
                                          , public = True
                                          , contentType = "x-application/any" }
                inserted <- jsonBody <$> (app `post` (resourcesBase <> qUid uid)) insertBody :: IO (WithId Resource)
                let rid = getId inserted
                status <- simpleStatus <$> (app `postRaw` (resourcesBase <> uri [rid, "content"])) BS.empty
                status `shouldBe` methodNotAllowed405

    describe "delete resource content" $ do

        it "should give 405" $
            runTestWithUser $ \app uid -> do
                let insertBody = Resource { path = "/path/to/res"
                                          , createdByUserId = Nothing
                                          , createdAt = Nothing
                                          , public = True
                                          , contentType = "x-application/any" }
                inserted <- jsonBody <$> (app `post` (resourcesBase <> qUid uid)) insertBody :: IO (WithId Resource)
                let rid = getId inserted
                status <- simpleStatus <$> app `delete` (resourcesBase <> uri [rid, "content"])
                status `shouldBe` methodNotAllowed405

    describe "put resource content" $ do

        it "should give 404 for bad key" $
            runTest $ \app -> do
                status <- simpleStatus <$> (app `putRaw` (resourcesBase <> uri ["bad-key", "content"])) BS.empty
                status `shouldBe` notFound404

        it "should give 204 for valid key" $
            runTestWithUser $ \app uid -> do
                let insertBody = Resource { path = "/path/to/res"
                                          , createdByUserId = Nothing
                                          , createdAt = Nothing
                                          , public = True
                                          , contentType = "x-application/any" }
                inserted <- jsonBody <$> (app `post` (resourcesBase <> qUid uid)) insertBody :: IO (WithId Resource)
                let rid = getId inserted
                status <- simpleStatus <$> (app `putRaw` (resourcesBase <> uri [rid, "content"])) BS.empty
                status `shouldBe` noContent204
