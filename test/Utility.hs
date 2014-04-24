
{-# LANGUAGE OverloadedStrings #-}

module Utility
(
  runTest
, get
, post
)
where

import Control.Monad.Trans         ( liftIO )
import Control.Monad.Trans.Reader  ( runReaderT )
import Data.Aeson                  ( ToJSON, encode )
import qualified Data.ByteString as BS
                                   ( ByteString )
import qualified Data.ByteString.Lazy as BSL
                                   ( toStrict )
import Data.Conduit                ( yield )
import Data.Text                   ( Text, pack )
import Database.Persist.Sql        ( runMigrationSilent )
import Network.Wai                 ( Application, Request(..) )
import Network.Wai.Test            ( SRequest(..), SResponse
                                   , runSession, srequest, setRawPathInfo, defaultRequest )
import Network.HTTP.Types.Header   ( hAccept )
import Network.HTTP.Types.Method   ( methodPost )
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
        _ <- runReaderT (runSql $ runMigrationSilent migrate) file
        app <- liftIO $ scottyApp file routes
        test app

scottyApp :: Text -> BrandyScottyM () -> IO Application
scottyApp file = scottyAppT (`runReaderT` file) (`runReaderT` file)

get :: Application -> BS.ByteString -> IO SResponse
get app path =
    runSession (srequest (SRequest req "")) app
  where req = setRawPathInfo getRequest path

post :: (ToJSON a) => Application -> BS.ByteString -> a -> IO SResponse
post app path payload =
    runSession (srequest (SRequest req "")) app
  where req = setRawPathInfo (postRequest $ BSL.toStrict $ encode payload) path

jsonRequest :: Request
jsonRequest =
    defaultRequest { requestHeaders = [(hAccept, "application/json")] }

getRequest :: Request
getRequest = jsonRequest

postRequest :: BS.ByteString -> Request
postRequest payload =
    jsonRequest { requestMethod = methodPost
                , requestBody   = yield payload }
