
module DataAccess.Tags
(
  dbGetAllTags
, dbGetTag
, dbInsertTag
, dbUpdateTag
, dbDeleteTag
)
where

import Database.Persist  ( Key )

import Core              ( DatabaseEnvironmentT )
import Database          ( standardGetAll, standardGetByKey, standardInsert, standardUpdate, standardDelete )
import Json.Tag          ( Tag, tagMapping )
import Json.WithId       ( WithId )
import qualified Schema as DB

dbGetAllTags :: DatabaseEnvironmentT [WithId Tag]
dbGetAllTags =
    standardGetAll tagMapping

dbGetTag :: Key DB.Tag -> DatabaseEnvironmentT (Maybe (WithId Tag))
dbGetTag =
    standardGetByKey tagMapping

dbInsertTag :: Tag -> DatabaseEnvironmentT (Maybe (WithId Tag))
dbInsertTag =
    standardInsert tagMapping

dbUpdateTag :: Key DB.Tag -> Tag -> DatabaseEnvironmentT (Maybe (WithId Tag))
dbUpdateTag =
    standardUpdate tagMapping

dbDeleteTag :: Key DB.Tag -> DatabaseEnvironmentT ()
dbDeleteTag =
    standardDelete
