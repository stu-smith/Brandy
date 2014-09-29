
{-# LANGUAGE OverloadedStrings #-}

module Transforms.Resource
(
  handleResource
)
where

import Control.Monad.Trans         ( lift )
import qualified Data.ByteString.Lazy as BSL
                                   ( fromStrict )
import qualified Data.Text as T    ( Text )
import qualified Data.Text.Lazy as TL
                                   ( fromStrict )
import Web.Scotty.Trans            ( next, setHeader, raw, status, text )
import Network.HTTP.Types.Status   ( notFound404 )

import Core                        ( BrandyActionM )
import DataAccess.Resources        ( dbGetResourceByPath )
import DataAccess.ResourceContent  ( dbGetResourceContent )
import Json.Resource               ( Resource )
import Json.WithId                 ( WithId, getKey )


handleResource :: T.Text -> BrandyActionM ()
handleResource path = do
    maybeResource <- lift $ dbGetResourceByPath path
    case maybeResource of
        Nothing       -> next
        Just r -> sendResource r

sendResource :: WithId Resource -> BrandyActionM ()
sendResource resource = do
    let key = getKey resource
    maybeContent <- lift $ dbGetResourceContent key
    case maybeContent of
        Nothing -> status notFound404 >> text "Not found."
        Just (ct, bv) -> do
            setHeader "Content-Type" $ TL.fromStrict ct
            raw $ BSL.fromStrict bv
