
{-# LANGUAGE FlexibleContexts #-}

module DataAccess.Resources
(
  dbGetAllResources
, dbGetResource
, dbGetResourceByPath
, dbInsertResource
, dbUpdateResource
, dbDeleteResource
)
where

import Control.Monad.IO.Class  ( liftIO )
import qualified Data.ByteString as BS
                               ( empty )
import qualified Data.Text as T
                               ( Text )
import Data.Time               ( UTCTime )
import Data.Time.Clock         ( getCurrentTime )
import Database.Esqueleto      ( Esqueleto, Value(..), val, select, update, from, set, where_
                               , (^.), (==.), (=.) )
import Database.Persist        ( Key, Entity, insert )

import Core                    ( DatabaseEnvironmentT )
import Database                ( runSql, runSqlMaybe, standardDelete )
import Json.Resource           ( Resource(..) )
import Json.WithId             ( WithId, addId, idToText )
import qualified Schema as DB


dbGetAllResources :: DatabaseEnvironmentT [WithId Resource]
dbGetAllResources =
    runSql $ do
        resources <- select $ from $ \r ->
                     return $ columnsWithoutContent r
        return $ map dvToJ resources

dbGetResource :: Key DB.Resource -> DatabaseEnvironmentT (Maybe (WithId Resource))
dbGetResource key =
    runSql $ do
        resource <- select $ from $ \r -> do
                    where_ (r ^. DB.ResourceId ==. val key)
                    return $ columnsWithoutContent r
        expectSingleRow resource

dbGetResourceByPath :: T.Text -> DatabaseEnvironmentT (Maybe (WithId Resource))
dbGetResourceByPath p =
    runSql $ do
        resource <- select $ from $ \r -> do
                    where_ (r ^. DB.ResourcePath ==. val p)
                    return $ columnsWithoutContent r
        expectSingleRow resource

dbInsertResource :: Key DB.User -> Resource -> DatabaseEnvironmentT (Maybe (WithId Resource))
dbInsertResource userId json = do
    db      <- liftIO $ jToNewD json userId
    maybeId <- runSqlMaybe $ insert db
    return $ toApi db <$> maybeId
  where
    toApi db key = addId key $ dToJ db

dbUpdateResource :: Key DB.Resource -> Resource -> DatabaseEnvironmentT (Maybe (WithId Resource))
dbUpdateResource key json = do
    runSql $ update $ \r -> do
                 set r [ DB.ResourcePath        =. (val . path        $ json)
                       , DB.ResourcePublic      =. (val . public      $ json)
                       , DB.ResourceContentType =. (val . contentType $ json)
                       ]
                 where_ (r ^. DB.ResourceId ==. val key)
    dbGetResource key

dbDeleteResource :: Key DB.Resource -> DatabaseEnvironmentT ()
dbDeleteResource =
    standardDelete

dvToJ :: ( Value (Key DB.Resource)
         , Value T.Text
         , Value (Key DB.User)
         , Value UTCTime
         , Value Bool 
         , Value T.Text
         ) -> WithId Resource
dvToJ ( Value rId
      , Value rPath
      , Value rCreatedBy
      , Value rCreatedAt
      , Value rPublic
      , Value rContentType
      ) =
    addId rId $ Resource rPath (Just $ idToText rCreatedBy) (Just rCreatedAt) rPublic rContentType

dToJ :: DB.Resource -> Resource
dToJ =
    Resource <$> DB.resourcePath
             <*> Just . idToText . DB.resourceCreatedBy
             <*> Just . DB.resourceCreatedAt
             <*> DB.resourcePublic
             <*> DB.resourceContentType

jToNewD :: Resource -> Key DB.User -> IO DB.Resource
jToNewD (Resource rPath _ _ rPublic rContentType) user = do
    now <- getCurrentTime
    return $ DB.Resource rPath user now rPublic rContentType BS.empty

columnsWithoutContent :: Esqueleto query expr backend
                      => expr (Entity DB.Resource)
                      -> ( expr (Value (Key DB.Resource))
                         , expr (Value T.Text)
                         , expr (Value (Key DB.User))
                         , expr (Value UTCTime)
                         , expr (Value Bool)
                         , expr (Value T.Text)
                         )
columnsWithoutContent r =
    ( r ^. DB.ResourceId
    , r ^. DB.ResourcePath
    , r ^. DB.ResourceCreatedBy
    , r ^. DB.ResourceCreatedAt
    , r ^. DB.ResourcePublic
    , r ^. DB.ResourceContentType
    )

expectSingleRow :: Monad m
                => [
                      ( Value (Key DB.Resource)
                      , Value T.Text
                      , Value (Key DB.User)
                      , Value UTCTime
                      , Value Bool
                      , Value T.Text
                      )
                   ]
                -> m (Maybe (WithId Resource))
expectSingleRow resource =
    case resource of
      []     -> return Nothing
      (r:[]) -> return $ Just $ dvToJ r
      _      -> undefined 
