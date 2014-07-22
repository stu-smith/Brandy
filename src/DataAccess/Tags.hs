
module DataAccess.Tags
(
  getAllTags
)
where

import Core         ( DatabaseEnvironmentT )
import Database     ( standardGetAll )
import Json.Tag     ( Tag, tagMapping )
import Json.WithId  ( WithId(..) )


getAllTags :: DatabaseEnvironmentT [WithId Tag]
getAllTags =
    standardGetAll tagMapping
