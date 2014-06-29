
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database
(
  runSql
, runSqlMaybe
)
where

import Control.Monad.CatchIO         ( MonadCatchIO, try )
import Control.Exception             ( IOException )
import Control.Monad.IO.Class        ( MonadIO )
import Control.Monad.Logger          ( NoLoggingT )
import Control.Monad.Reader          ( ask )
import Control.Monad.Reader.Class    ( MonadReader )
import Control.Monad.Trans           ( lift )
import Control.Monad.Trans.Class     ( MonadTrans )
import Control.Monad.Trans.Control   ( MonadBaseControl )
import Control.Monad.Trans.Resource  ( ResourceT )
import Database.Persist.Sql          ( SqlPersistT )
import Database.Persist.Sqlite       ( runSqlite )
import qualified Data.Text as T      ( Text )


runSql :: (MonadIO m, MonadBaseControl IO m, MonadTrans t, MonadReader T.Text (t m))
       => SqlPersistT (NoLoggingT (ResourceT m)) a -> t m a
runSql action = do
    conn <- ask
    lift $ runSqlite conn action

runSqlMaybe :: forall a . forall m . forall t .
               (MonadCatchIO m, MonadIO m, MonadBaseControl IO m, MonadTrans t, MonadReader T.Text (t m))
            => SqlPersistT (NoLoggingT (ResourceT m)) a -> t m (Maybe a)
runSqlMaybe action = do
    conn <- ask
    lift $ runWithTry conn
  where runWithTry conn = do
            result <- (try $ runSqlite conn action) :: m (Either IOException a)
            return $ case result of
                Left _  -> Nothing
                Right v -> Just v
