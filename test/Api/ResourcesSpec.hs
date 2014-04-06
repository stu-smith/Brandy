
{-# LANGUAGE OverloadedStrings #-}

module Api.ResourcesSpec where

import Control.Applicative
import Test.Hspec
import Control.Monad.IO.Class
import Network.HTTP.Types.Status
import qualified Web.Scotty        as S
import qualified Network.Wai.Test  as WT

import Routing
import Utility


spec :: Spec
spec = do

  describe "get single resource" $ do
    it "should give 400 for bad key" $ do
      app <- liftIO $ S.scottyApp routes
      status <- WT.simpleStatus <$> app `get` "/api/resources/bad-key"
      status `shouldBe` badRequest400
