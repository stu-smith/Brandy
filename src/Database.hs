
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
import Control.Monad.Reader.Class    ( MonadReader )
import Control.Monad.Trans           ( lift )
import Control.Monad.Trans.Class     ( MonadTrans )
import Control.Monad.Trans.Control   ( MonadBaseControl )
import Control.Monad.Trans.Resource  ( ResourceT )
import Database.Persist.Sql          ( SqlPersistT )
import Database.Persist.Sqlite       ( runSqlite )
import qualified Data.Text as T      ( Text )


runSql :: (MonadIO m, MonadBaseControl IO m, MonadTrans t, MonadReader T.Text (t m))
       => SqlPersistT (NoLoggingT (ResourceT m)) b -> t m b
runSql action = do
    conn <- ask
    lift $ runSqlite conn action
