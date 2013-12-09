
module Database
( runSql
)
where

import Database.Persist.Sqlite  ( withSqliteConn )


runSql :: SqlPersist IO a -> IO a
runSql
    = withSqliteConn dbfile . runSqlConn
        where dbfile = "brandy.sqlite3"
