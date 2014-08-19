
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

module TestUtility
(
  URIBuilder
, uri
, query
, runTest
, get
, post
, put
, delete
, jsonBody
)
where

import Blaze.ByteString.Builder    ( toByteString )
import Control.Monad.Trans         ( liftIO )
import Control.Monad.Trans.Reader  ( runReaderT )
import Data.Aeson                  ( ToJSON, FromJSON, encode, decode )
import qualified Data.ByteString as BS
                                   ( ByteString )
import qualified Data.ByteString.Lazy as BSL
                                   ( toStrict )
import qualified Data.Text as T    ( Text, pack )
import Data.Text.Encoding          ( encodeUtf8 )
import Data.Maybe                  ( fromJust )
import Data.Monoid                 ( Monoid(..) )
import Database.Persist.Sql        ( runMigrationSilent )
import Network.Wai                 ( Application, Request(..) )
import Network.Wai.Test            ( SRequest(..), SResponse
                                   , runSession, srequest, setRawPathInfo, defaultRequest, simpleBody )
import Network.HTTP.Types.Header   ( hAccept )
import Network.HTTP.Types.Method   ( Method, methodGet, methodPost, methodPut, methodDelete )
import Network.HTTP.Types.URI      ( Query, encodePath )
import System.IO.Temp              ( withSystemTempFile )
import Web.Scotty.Trans            ( scottyAppT )

import Core                        ( BrandyScottyM )
import Database                    ( runSql )
import Routing                     ( routes )
import Schema                      ( migrate )


data URIBuilder = URIBuilder [T.Text] Query

instance Monoid URIBuilder where
    mappend (URIBuilder p1 q1) (URIBuilder p2 q2) =
        URIBuilder (p1 ++ p2) (q1 ++ q2)
    mempty =
        URIBuilder [] []

uri :: [T.Text] -> URIBuilder
uri path =
    URIBuilder path []

query :: BS.ByteString -> T.Text -> URIBuilder
query q v =
    URIBuilder [] [(q, Just $ encodeUtf8 v)]

runTest :: (Application -> IO ()) -> IO ()
runTest test =
    withSystemTempFile "brandytest.sqlite3" $ \f _ -> do
        let file = T.pack f
        _   <- runReaderT (runSql . runMigrationSilent $ migrate) file
        app <- liftIO $ scottyApp file routes
        test app

scottyApp :: T.Text -> BrandyScottyM () -> IO Application
scottyApp file = scottyAppT (`runReaderT` file) (`runReaderT` file)

get :: Application -> URIBuilder -> IO SResponse
get app path =
    actionWithoutBody (jsonRequest methodGet) app path

post :: (ToJSON a) => Application -> URIBuilder -> a -> IO SResponse
post =
    actionWithBody $ jsonRequestWithBody methodPost

put :: (ToJSON a) => Application -> URIBuilder -> a -> IO SResponse
put =
    actionWithBody $ jsonRequestWithBody methodPut

delete :: Application -> URIBuilder -> IO SResponse
delete app path =
    actionWithoutBody (jsonRequest methodDelete) app path

jsonBody :: FromJSON a => SResponse -> a
jsonBody =
    fromJust . decode . simpleBody

actionWithBody :: (ToJSON a) => (BS.ByteString -> Request) -> Application -> URIBuilder -> a -> IO SResponse
actionWithBody request app uriBuilder payload =
    runSession (srequest sreq) app
  where req  = setRawPathInfo (request $ BSL.toStrict body) $ uriToByteString uriBuilder
        sreq = SRequest req body
        body = encode payload

actionWithoutBody :: Request -> Application -> URIBuilder -> IO SResponse
actionWithoutBody request app uriBuilder =
    runSession (srequest sreq) app
  where req  = setRawPathInfo request $ uriToByteString uriBuilder
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

uriToByteString :: URIBuilder -> BS.ByteString
uriToByteString (URIBuilder p q) =
    toByteString $ encodePath p q
