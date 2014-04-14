
{-# LANGUAGE OverloadedStrings #-}

module Api.ResourcesSpec where

--import Control.Applicative                 ( (<$>) )
--import Control.Monad.Trans.Reader          ( runReader )
--import Data.Text                           ( Text )
--import Control.Monad.Trans                 ( liftIO )
--import Network.Wai                         ( Application )
import Test.Hspec
--import Network.HTTP.Types.Status
--import qualified Web.Scotty.Trans  as ST
--import qualified Network.Wai.Test  as WT

--import Routing
--import Utility
--import Core

spec :: Spec
spec = return ()

--spec :: Spec
--spec = do

--  describe "get single resource" $ do
--    it "should give 400 for bad key" $ do
--      app <- liftIO $ scottyApp "test.sqlite" routes
--      status <- WT.simpleStatus <$> app `get` "/api/resources/bad-key"
--      status `shouldBe` badRequest400

--scottyApp :: Text -> BrandyScottyM () -> IO Application
--scottyApp file = ST.scottyAppT (\f -> runReader f file) id
