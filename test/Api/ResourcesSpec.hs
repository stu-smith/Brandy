
{-# LANGUAGE OverloadedStrings #-}

module Api.ResourcesSpec
(
  spec
)
where

import Control.Applicative        ( (<$>) )
import Network.HTTP.Types.Status  ( badRequest400 )
import Network.Wai.Test           ( simpleStatus )
import Test.Hspec                 ( Spec, describe, it, shouldBe )

import TestUtility                ( runTest, get )


spec :: Spec
spec = do

    describe "get single resource" $ do

        it "should give 400 for bad key" $
            runTest $ \app -> do
                status <- simpleStatus <$> app `get` "/api/resources/bad-key"
                status `shouldBe` badRequest400

        --it "should give 404 for missing key" $
        --    runTest $ \app -> do
        --        status <- simpleStatus <$> app `get` "/api/resources/123"
        --        status `shouldBe` notFound404
