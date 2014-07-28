
module DataAccess.Tags
(
  getAllTags
, getTagByKey
, insertTag
, updateTag
, deleteTag
)
where

import Database.Persist  ( Key )

import Core              ( DatabaseEnvironmentT )
import Database          ( standardGetAll, standardGetByKey, standardInsert, standardUpdate, standardDelete )
import Json.Tag          ( Tag, tagMapping )
import Json.WithId       ( WithId(..) )
import qualified Schema as DB

getAllTags :: DatabaseEnvironmentT [WithId Tag]
getAllTags =
    standardGetAll tagMapping

getTagByKey :: Key DB.Tag -> DatabaseEnvironmentT (Maybe (WithId Tag))
getTagByKey =
    standardGetByKey tagMapping

insertTag :: Tag -> DatabaseEnvironmentT (Maybe (WithId Tag))
insertTag =
    standardInsert tagMapping

updateTag :: Key DB.Tag -> Tag -> DatabaseEnvironmentT (Maybe (WithId Tag))
updateTag =
    standardUpdate tagMapping

deleteTag :: Key DB.Tag -> DatabaseEnvironmentT ()
deleteTag =
    standardDelete
