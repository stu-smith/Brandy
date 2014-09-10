
{-# LANGUAGE OverloadedStrings #-}

module Api.ResourceContent
(
  apiGetResourceContent
, apiUpdateResourceContent
)
where

import Control.Applicative         ( (<$>) )
import qualified Data.ByteString.Lazy as BSL
                                   ( fromStrict, toStrict, empty )
import qualified Data.Text as T    ( Text )
import qualified Data.Text.Lazy as TL
                                   ( fromStrict )
import Network.HTTP.Types.Status   ( ok200, noContent204, notFound404 )
import Web.Scotty.Trans            ( body )

import ApiUtility                  ( RawResponse(..), runApiRaw, readKey, liftDB, liftWeb, validateDbExists, apiFail )
import Core                        ( BrandyActionM )
import DataAccess.ResourceContent  ( dbGetResourceContent, dbUpdateResourceContent )

apiGetResourceContent :: T.Text -> BrandyActionM ()
apiGetResourceContent keyText =
    runApiRaw ok200 $ do
        key     <- readKey keyText
        maybeDb <- liftDB $ dbGetResourceContent key
        validateDbExists (toRawResponse <$> maybeDb)
  where
    toRawResponse (ct, bv) = RawResponse (Just $ TL.fromStrict ct) (BSL.fromStrict bv)

apiUpdateResourceContent :: T.Text -> BrandyActionM ()
apiUpdateResourceContent keyText =
    runApiRaw noContent204 $ do
        key   <- readKey keyText
        bv    <- liftWeb body
        found <- liftDB $ dbUpdateResourceContent key $ BSL.toStrict bv
        if found
            then return $ RawResponse Nothing BSL.empty
            else apiFail notFound404 "Not found."
