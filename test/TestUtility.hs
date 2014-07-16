
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module TestUtility
(
  runTest
, get
, post
, put
, delete
, jsonBody
)
where

import Control.Monad.Trans         ( liftIO )
import Control.Monad.Trans.Reader  ( runReaderT )
import Data.Aeson                  ( ToJSON, FromJSON, encode, decode )
import qualified Data.ByteString as BS
                                   ( ByteString )
import qualified Data.ByteString.Lazy as BSL
                                   ( toStrict )
import qualified Data.Text as T    ( Text, pack )
import Data.Maybe                  ( fromJust )
import Data.Text.Encoding          ( encodeUtf8 )
import Database.Persist.Sql        ( runMigrationSilent )
import Network.Wai                 ( Application, Request(..) )
import Network.Wai.Test            ( SRequest(..), SResponse
                                   , runSession, srequest, setRawPathInfo, defaultRequest, simpleBody )
import Network.HTTP.Types.Header   ( hAccept )
import Network.HTTP.Types.Method   ( Method, methodGet, methodPost, methodPut, methodDelete )
import System.IO.Temp              ( withSystemTempFile )
import Web.Scotty.Trans            ( scottyAppT )

import Core                        ( BrandyScottyM )
import Database                    ( runSql )
import Routing                     ( routes )
import Schema                      ( migrate )


runTest :: (Application -> IO ()) -> IO ()
runTest test =
    withSystemTempFile "brandytest.sqlite3" $ \f _ -> do
        let file = T.pack f
        _ <- runReaderT (runSql $ runMigrationSilent migrate) file
        app <- liftIO $ scottyApp file routes
        test app

scottyApp :: T.Text -> BrandyScottyM () -> IO Application
scottyApp file = scottyAppT (`runReaderT` file) (`runReaderT` file)

get :: Application -> T.Text -> IO SResponse
get app path =
    actionWithoutBody (jsonRequest methodGet) app path

post :: (ToJSON a) => Application -> T.Text -> a -> IO SResponse
post =
    actionWithBody $ jsonRequestWithBody methodPost

put :: (ToJSON a) => Application -> T.Text -> a -> IO SResponse
put =
    actionWithBody $ jsonRequestWithBody methodPut

delete :: Application -> T.Text ->IO SResponse
delete app path =
    actionWithoutBody (jsonRequest methodDelete) app path

jsonBody :: FromJSON a => SResponse -> a
jsonBody =
    fromJust . decode . simpleBody

actionWithBody :: (ToJSON a) => (BS.ByteString -> Request) -> Application -> T.Text -> a -> IO SResponse
actionWithBody request app path payload =
    runSession (srequest sreq) app
  where req  = setRawPathInfo (request $ BSL.toStrict body) $ encodeUri path
        sreq = SRequest req body
        body = encode payload

actionWithoutBody :: Request -> Application -> T.Text -> IO SResponse
actionWithoutBody request app path =
    runSession (srequest sreq) app
  where req  = setRawPathInfo request $ encodeUri path
        sreq = SRequest req ""

jsonRequest :: Method -> Request
jsonRequest method =
    defaultRequest
        { requestMethod  = method
        , requestHeaders = [(hAccept, "application/json")]
        }

jsonRequestWithBody :: Method -> BS.ByteString -> Request
jsonRequestWithBody method payload =
    (jsonRequest method)
        { requestBody = return payload
        }

encodeUri :: T.Text -> BS.ByteString
encodeUri = encodeUtf8
