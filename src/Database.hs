
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Database
(
  runSql
)
where

import Control.Monad.Trans.Control   ( MonadBaseControl )
import Control.Monad.IO.Class        ( MonadIO )
import Control.Monad.Logger          ( NoLoggingT )
import Control.Monad.Trans.Resource  ( ResourceT )
import Database.Persist.Sql          ( SqlPersistT )
import Database.Persist.Sqlite       ( runSqlite )


runSql :: (MonadBaseControl IO m, MonadIO m)
       => SqlPersistT (NoLoggingT (ResourceT m)) a -> m a
runSql =
  runSqlite "brandy.sqlite3"
