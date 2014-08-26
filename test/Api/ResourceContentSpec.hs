
{-# LANGUAGE OverloadedStrings #-}

module Api.ResourceContentSpec
(
  spec
)
where

import Control.Applicative        ( (<$>) )
import qualified Data.ByteString as BS
                                  ( empty )
import Data.Monoid                ( (<>) )
import Network.HTTP.Types.Status  ( ok200, notFound404, methodNotAllowed405 )
import Network.Wai.Test           ( simpleStatus )
import Test.Hspec                 ( Spec, describe, it, shouldBe )

import Json.Resource              ( Resource(..) )
import Json.WithId                ( WithId(..), getId )
import TestUtility                ( runTest, get, post, postRaw, delete, uri, jsonBody )
import UserTestUtility            ( runTestWithUser, qUid )


spec :: Spec
spec = do

    let resourcesBase = uri ["api", "resources"]

    describe "get resource content" $ do

        it "should give 404 for bad key" $
            runTest $ \app -> do
                status <- simpleStatus <$> app `get` (resourcesBase <> uri ["bad-key", "content"])
                status `shouldBe` notFound404

        it "should give 200 for get resource content" $
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

    describe "post resource content" $ do

        it "should give 405 for post resource content" $
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

        it "should give 405 for delete resource content" $
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
