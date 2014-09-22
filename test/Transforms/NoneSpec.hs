
{-# LANGUAGE OverloadedStrings #-}

module Transforms.NoneSpec
(
  spec
)
where

import Control.Applicative        ( (<$>) )
import Data.Monoid                ( (<>) )
import Test.Hspec                 ( Spec, describe, it, shouldBe )
import Network.HTTP.Types.Status  ( ok200, notFound404 )
import Network.Wai.Test           ( simpleStatus )

import Json.Resource              ( Resource(..) )
import Json.WithId                ( WithId, getId )
import TestUtility                ( runTest, get, putRaw, post, uri, jsonBody )
import UserTestUtility            ( runTestWithUser, qUid )


spec :: Spec
spec = do

    let resourcesBase = uri ["api", "resources"]

    describe "get untransformed" $ do

        it "should give 404 for missing resource" $
            runTest $ \app -> do
                status <- simpleStatus <$> app `get` uri ["foo", "bar"]
                status `shouldBe` notFound404

        it "should give 200 for found resource" $
            runTestWithUser $ \app uid -> do
                let insertBody = Resource { path = "/foo/bar"
                                          , createdByUserId = Nothing
                                          , createdAt = Nothing
                                          , public = True
                                          , contentType = "text/plain" }
                inserted <- jsonBody <$> (app `post` (resourcesBase <> qUid uid)) insertBody :: IO (WithId Resource)
                let rid = getId inserted
                _ <- id <$> (app `putRaw` (resourcesBase <> uri [rid, "content"])) "Hello world"
                status <- simpleStatus <$> app `get` uri ["foo", "bar"]
                status `shouldBe` ok200
