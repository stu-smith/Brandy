
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
, postRaw
, put
, putRaw
, delete
, jsonBody
, simpleHeader
)
where

import Blaze.ByteString.Builder    ( toByteString )
import Control.Monad.Trans         ( liftIO )
import Control.Monad.Trans.Reader  ( runReaderT )
import Data.Aeson                  ( ToJSON, FromJSON, encode, decode )
import qualified Data.ByteString as BS
                                   ( ByteString )
import qualified Data.ByteString.Lazy as BSL
                                   ( ByteString, toStrict, fromStrict )
import Data.List                   ( find )
import qualified Data.Text as T    ( Text, pack )
import Data.Text.Encoding          ( encodeUtf8 )
import Data.Maybe                  ( fromJust )
import Database.Persist.Sql        ( runMigrationSilent )
import Network.Wai                 ( Application, Request(..) )
import Network.Wai.Test            ( SRequest(..), SResponse
                                   , runSession, srequest, setPath, defaultRequest, simpleBody, simpleHeaders )
import Network.HTTP.Types.Header   ( HeaderName, hAccept )
import Network.HTTP.Types.Method   ( Method, methodGet, methodPost, methodPut, methodDelete )
import Network.HTTP.Types.URI      ( Query, encodePath )
import System.IO.Temp              ( withSystemTempFile )
import Web.Scotty.Trans            ( scottyAppT )

import Core                        ( BrandyScottyM )
import Database                    ( runSql )
import Plugins                     ( mkPlugin )
import Routing                     ( routes )
import Schema                      ( migrate )


data URIBuilder = URIBuilder [T.Text] Query

instance Monoid URIBuilder where
    mappend (URIBuilder p1 q1) (URIBuilder p2 q2) =
        URIBuilder (p1 ++ p2) (q1 ++ q2)
    mempty =
        URIBuilder [] []

instance Show URIBuilder where
    show (URIBuilder p q) =
        show $ toByteString $ encodePath p q

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
        app <- liftIO $ scottyApp file $ routes mkPlugin
        test app

scottyApp :: T.Text -> BrandyScottyM () -> IO Application
scottyApp file = scottyAppT (`runReaderT` file) (`runReaderT` file)

get :: Application -> URIBuilder -> IO SResponse
get app path =
    actionWithoutBody (jsonRequest methodGet) app path

post :: (ToJSON a) => Application -> URIBuilder -> a -> IO SResponse
post app u v =
    actionWithBody (encode v) (jsonRequestWithBody methodPost) app u

postRaw :: Application -> URIBuilder -> BS.ByteString -> IO SResponse
postRaw app u v =
    actionWithBody (BSL.fromStrict v) (defaultRequestWithBody methodPost) app u

put :: (ToJSON a) => Application -> URIBuilder -> a -> IO SResponse
put app u v =
    actionWithBody (encode v) (jsonRequestWithBody methodPut) app u

putRaw :: Application -> URIBuilder -> BS.ByteString -> IO SResponse
putRaw app u v =
    actionWithBody (BSL.fromStrict v) (defaultRequestWithBody methodPut) app u

delete :: Application -> URIBuilder -> IO SResponse
delete app path =
    actionWithoutBody (jsonRequest methodDelete) app path

jsonBody :: FromJSON a => SResponse -> a
jsonBody =
    fromJust . decode . simpleBody

actionWithBody :: BSL.ByteString -> (BS.ByteString -> Request) -> Application -> URIBuilder -> IO SResponse
actionWithBody body request app uriBuilder =
    runSession (srequest sreq) app
  where req  = setPath (request $ BSL.toStrict body) $ uriToByteString uriBuilder
        sreq = SRequest req body

actionWithoutBody :: Request -> Application -> URIBuilder -> IO SResponse
actionWithoutBody request app uriBuilder =
    runSession (srequest sreq) app
  where req  = setPath request $ uriToByteString uriBuilder
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

defaultRequestWithBody :: Method -> BS.ByteString -> Request
defaultRequestWithBody method payload =
    defaultRequest
        { requestMethod = method
        , requestBody   = return payload
        }

uriToByteString :: URIBuilder -> BS.ByteString
uriToByteString (URIBuilder p q) =
    toByteString $ encodePath p q

simpleHeader :: HeaderName -> SResponse -> BS.ByteString
simpleHeader name =
    snd . fromJust . find ((name ==) . fst) . simpleHeaders
