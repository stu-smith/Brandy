
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module Database
(
  JsonDataAccessMapping(..)
, runSql
, runSqlMaybe
, standardGetAll
, standardGetByKey
, standardInsert
, standardUpdate
, standardDelete
)
where

import Control.Monad.CatchIO         ( MonadCatchIO, try )
import Control.Monad.IO.Class        ( MonadIO )
import Control.Monad.Logger          ( NoLoggingT )
import Control.Monad.Reader          ( ask )
import Control.Monad.Reader.Class    ( MonadReader )
import Control.Monad.Trans           ( lift )
import Control.Monad.Trans.Class     ( MonadTrans )
import Control.Monad.Trans.Control   ( MonadBaseControl )
import Control.Monad.Trans.Resource  ( ResourceT )
import Data.Aeson                    ( FromJSON, ToJSON )
import Data.Either.Combinators       ( rightToMaybe )
import Database.Persist              ( Key, ToBackendKey, get, insert, replace, delete, selectList )
import Database.Persist.Class        ( PersistEntity, PersistEntityBackend )
import Database.Persist.Sql          ( SqlPersistT, SqlBackend )
import Database.Persist.Sqlite       ( runSqlite )
import Database.Persist.Types        ( Entity(..) )
import Database.Sqlite               ( SqliteException )
import qualified Data.Text as T      ( Text )

import Json.WithId                   ( WithId, addId )
import Core                          ( DatabaseEnvironmentT )


data (PersistEntityBackend d ~ SqlBackend, PersistEntity d) => JsonDataAccessMapping j d = JsonDataAccessMapping
    { jsonToDataAccess :: j -> d
    , dataAccessToJson :: d -> j
    }

runSql :: (MonadIO m, MonadBaseControl IO m, MonadTrans t, MonadReader T.Text (t m))
       => SqlPersistT (NoLoggingT (ResourceT m)) a -> t m a
runSql action = do
    conn <- ask
    lift $ runSqlite conn action

runSqlMaybe :: forall a . forall m . forall t .
               (MonadCatchIO (t m), MonadCatchIO m, MonadIO m, MonadBaseControl IO m, MonadTrans t, MonadReader T.Text (t m))
            => SqlPersistT (NoLoggingT (ResourceT m)) a -> t m (Maybe a)
runSqlMaybe action = do
    result <- (try $ runSql action) :: t m (Either SqliteException a)
    return $ rightToMaybe result

standardGetAll :: (PersistEntityBackend d ~ SqlBackend, PersistEntity d, ToBackendKey SqlBackend d, ToJSON j, FromJSON j)
               => JsonDataAccessMapping j d -> DatabaseEnvironmentT [WithId j]
standardGetAll (JsonDataAccessMapping _ dToJ) = do
    dbs <- runSql $ selectList [] []
    return $ map eToJ dbs
  where
    eToJ (Entity k v) = addId k $ dToJ v

standardGetByKey :: (PersistEntityBackend d ~ SqlBackend, PersistEntity d, ToBackendKey SqlBackend d, ToJSON j, FromJSON j)
                 => JsonDataAccessMapping j d -> Key d -> DatabaseEnvironmentT (Maybe (WithId j))
standardGetByKey (JsonDataAccessMapping _ dToJ) key = do
    maybeDb <- runSql $ get key
    return $ toApi <$> maybeDb
  where
    toApi db = addId key $ dToJ db

standardInsert :: (PersistEntityBackend d ~ SqlBackend, PersistEntity d, ToBackendKey SqlBackend d, ToJSON j, FromJSON j)
               => JsonDataAccessMapping j d -> j -> DatabaseEnvironmentT (Maybe (WithId j))
standardInsert (JsonDataAccessMapping jToD dToJ) json = do
    maybeId <- runSqlMaybe $ insert db
    return $ toApi <$> maybeId
  where
    db        = jToD json
    toApi key = addId key $ dToJ db

standardUpdate :: (PersistEntityBackend d ~ SqlBackend, PersistEntity d, ToBackendKey SqlBackend d, ToJSON j, FromJSON j)
               => JsonDataAccessMapping j d -> Key d -> j -> DatabaseEnvironmentT (Maybe (WithId j))
standardUpdate (JsonDataAccessMapping jToD _) key json =
    runSql $ do
        replace key $ jToD json
        return $ Just $ addId key json

standardDelete :: (PersistEntityBackend d ~ SqlBackend, PersistEntity d)
               => Key d -> DatabaseEnvironmentT ()
standardDelete key =
    runSql $ delete key
