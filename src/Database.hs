
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Database
(
  runSql
)
where

import Control.Monad.IO.Class        ( MonadIO )
import Control.Monad.Logger          ( NoLoggingT )
import Control.Monad.Reader          ( ask )
import Control.Monad.Trans           ( lift )
import Control.Monad.Trans.Control   ( MonadBaseControl )
import Control.Monad.Trans.Resource  ( ResourceT )
import Database.Persist.Sql          ( SqlPersistT )
import Database.Persist.Sqlite       ( runSqlite )

import Core                          ( DatabaseEnvironmentT )


runSql :: (MonadBaseControl IO m, MonadIO m)
       => SqlPersistT (NoLoggingT (ResourceT m)) a -> DatabaseEnvironmentT m a
runSql action = do
  conn <- ask
  lift $ runSqlite conn action
