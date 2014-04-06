
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
import Data.Text                  ( Text, pack )
import Data.Text.Lazy             ( fromStrict )
import Control.Monad.IO.Class     ( liftIO )
import Web.Scotty                 ( ActionM, json, text, status, jsonData )
import Network.HTTP.Types.Status  ( notFound404 )
import Database.Esqueleto as Sql  ( Value(..), select, deleteCount, from, where_, (^.), (==.), val )
import Database.Persist           ( Key, Entity(..), insert )

import Schema
import Database                   ( runSql )
import ApiUtility                 ( parseKey )


apiGetResources :: ActionM ()
apiGetResources =
  json =<< liftIO sqlGetAllResourcesSummary

apiGetResourceByKey :: Text -> ActionM ()
apiGetResourceByKey keyText =
  parseKey keyText $ \key -> do
    text $ fromStrict $ pack $ show key

apiInsertResource :: ActionM ()
apiInsertResource = do
  res <- jsonData
  key <- liftIO $ sqlInsertResource res
  json Entity { entityKey = key, entityVal = res }

apiDeleteResourceByKey :: Text -> ActionM ()
apiDeleteResourceByKey keyText =
  parseKey keyText $ \key -> do
    numRows <- liftIO $ sqlDeleteResource key
    if numRows == 1
      then text "Resource deleted."
      else status notFound404 >> text "Resource not found."


sqlGetAllResourcesSummary :: IO [J.Value]
sqlGetAllResourcesSummary =
  runSql $ do
    rows <- select $ from $ \resource ->
            return (resource ^. ResourceId, resource ^. ResourcePath)
    return $ map idAndPathAsJSON rows

idAndPathAsJSON :: (Sql.Value (Key Resource), Sql.Value Text) -> J.Value
idAndPathAsJSON (Sql.Value key, Sql.Value path) =
  object ["id" .= key, "path" .= path]

sqlInsertResource :: Resource -> IO (Key Resource)
sqlInsertResource res =
  runSql $ insert res

sqlDeleteResource :: ResourceId -> IO Int64
sqlDeleteResource key =
  runSql $ deleteCount $ from $ \resource ->
           where_ (resource ^. ResourceId ==. val key)
