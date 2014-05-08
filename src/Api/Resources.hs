
{-# LANGUAGE OverloadedStrings #-}

module Api.Resources
(
  apiGetResources
, apiGetResourceByKey
, apiInsertResource
, apiDeleteResourceByKey
)
where

import Data.Aeson as J                 ( Value, object, (.=) )
import Data.Int                        ( Int64 )
import qualified Data.Text as T        ( Text, pack )
import qualified Data.Text.Lazy as TL  ( fromStrict )
import Control.Monad.Trans             ( lift )
import Web.Scotty.Trans                ( json, text, status, jsonData )
import Network.HTTP.Types.Status       ( badRequest400, notFound404 )
import Database.Esqueleto as Sql       ( Value(..), select, deleteCount, from, where_, (^.), (==.), val )
import Database.Persist                ( Key, Entity(..), insert )

import Schema
import Database                        ( runSql )
import ApiUtility                      ( readKeyOld )
import Core                            ( DatabaseEnvironmentT, BrandyActionM )


apiGetResources :: BrandyActionM ()
apiGetResources = do
    res <- lift sqlGetAllResourcesSummary
    json res

apiGetResourceByKey :: T.Text -> BrandyActionM ()
apiGetResourceByKey keyText =
    case readKeyOld keyText of
        Nothing  -> status badRequest400 >> text "Invalid ID."
        Just key -> text $ TL.fromStrict $ T.pack $ show key

apiInsertResource :: BrandyActionM ()
apiInsertResource = do
    res <- jsonData
    key <- lift $ sqlInsertResource res
    json Entity { entityKey = key, entityVal = res }

apiDeleteResourceByKey :: T.Text -> BrandyActionM ()
apiDeleteResourceByKey keyText =
    case readKeyOld keyText of
        Nothing  -> status badRequest400 >> text "Invalid ID."
        Just key -> do
            numRows <- lift $ sqlDeleteResource key
            if numRows == 1
                then text "Resource deleted."
                else status notFound404 >> text "Resource not found."


sqlGetAllResourcesSummary :: DatabaseEnvironmentT [J.Value]
sqlGetAllResourcesSummary =
    runSql $ do
        rows <- select $ from $ \resource ->
                return (resource ^. ResourceId, resource ^. ResourcePath)
        return $ map idAndPathAsJSON rows

idAndPathAsJSON :: (Sql.Value (Key Resource), Sql.Value T.Text) -> J.Value
idAndPathAsJSON (Sql.Value key, Sql.Value path) =
    object ["id" .= key, "path" .= path]

sqlInsertResource :: Resource -> DatabaseEnvironmentT (Key Resource)
sqlInsertResource res =
    runSql $ insert res

sqlDeleteResource :: ResourceId -> DatabaseEnvironmentT Int64
sqlDeleteResource key =
    runSql $ deleteCount $ from $ \resource ->
             where_ (resource ^. ResourceId ==. val key)
