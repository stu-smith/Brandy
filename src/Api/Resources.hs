
{-# LANGUAGE OverloadedStrings #-}

module Api.Resources
(
  apiGetResources
, apiGetResourceByKey
, apiInsertResource
, apiDeleteResourceByKey
)
where

import Data.Int                   ( Int64 )
import Data.Aeson as J            ( Value, object, (.=) )
import Data.Text as T             ( Text, pack )
import Data.Text.Lazy as TL       ( fromStrict )
import Control.Monad.Trans        ( lift, liftIO )
import Control.Monad.Reader       ( ask )
import Web.Scotty.Trans           ( json, text, status, jsonData )
import Network.HTTP.Types.Status  ( notFound404 )
import Database.Esqueleto as Sql  ( Value(..), select, deleteCount, from, where_, (^.), (==.), val )
import Database.Persist           ( Key, Entity(..), insert )

import Schema
import Database                   ( runSql )
import ApiUtility                 ( parseKey )
import Core                       ( BrandyActionM )


apiGetResources :: BrandyActionM ()
apiGetResources = do
  conn <- lift ask
  res <- liftIO $ sqlGetAllResourcesSummary conn
  json res

apiGetResourceByKey :: Text -> BrandyActionM ()
apiGetResourceByKey keyText =
  parseKey keyText $ \key -> do
    text $ fromStrict $ pack $ show key

apiInsertResource :: BrandyActionM ()
apiInsertResource = do
  conn <- lift ask
  res <- jsonData
  key <- liftIO $ sqlInsertResource conn res
  json Entity { entityKey = key, entityVal = res }

apiDeleteResourceByKey :: Text -> BrandyActionM ()
apiDeleteResourceByKey keyText = do
  conn <- lift ask
  parseKey keyText $ \key -> do
    numRows <- liftIO $ sqlDeleteResource conn key
    if numRows == 1
      then text "Resource deleted."
      else status notFound404 >> text "Resource not found."


sqlGetAllResourcesSummary :: T.Text -> IO [J.Value]
sqlGetAllResourcesSummary conn =
  runSql conn $ do
    rows <- select $ from $ \resource ->
            return (resource ^. ResourceId, resource ^. ResourcePath)
    return $ map idAndPathAsJSON rows

idAndPathAsJSON :: (Sql.Value (Key Resource), Sql.Value Text) -> J.Value
idAndPathAsJSON (Sql.Value key, Sql.Value path) =
  object ["id" .= key, "path" .= path]

sqlInsertResource :: T.Text -> Resource -> IO (Key Resource)
sqlInsertResource conn res =
  runSql conn $ insert res

sqlDeleteResource :: T.Text -> ResourceId -> IO Int64
sqlDeleteResource conn key =
  runSql conn $ deleteCount $ from $ \resource ->
           where_ (resource ^. ResourceId ==. val key)
