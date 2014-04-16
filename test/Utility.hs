
{-# LANGUAGE OverloadedStrings #-}

module Utility
(
  runTest
, get
)
where

import Control.Monad.Trans         ( liftIO )
import Control.Monad.Trans.Reader  ( runReaderT )
import Data.ByteString             ( ByteString )
import Data.Text                   ( Text, pack )
import Database.Persist.Sql        ( runMigration )
import Network.Wai                 ( Application )
import Network.Wai.Test            ( SRequest(..), SResponse, runSession, srequest
                                   , setRawPathInfo, defaultRequest )
import System.IO.Temp              ( withSystemTempFile )
import Web.Scotty.Trans            ( scottyAppT )

import Core                        ( BrandyScottyM )
import Database                    ( runSql )
import Routing                     ( routes )
import Schema                      ( migrate )


runTest :: (Application -> IO ()) -> IO ()
runTest test =
  withSystemTempFile "brandytest.sqlite3" $ \f _ -> do
    let file = pack f
    runReaderT (runSql $ runMigration migrate) file
    app <- liftIO $ scottyApp file routes
    test app

scottyApp :: Text -> BrandyScottyM () -> IO Application
scottyApp file = scottyAppT (`runReaderT` file) (`runReaderT` file)

get :: Application -> ByteString -> IO SResponse
get app path =
  runSession (srequest (SRequest req "")) app
    where req = setRawPathInfo defaultRequest path
