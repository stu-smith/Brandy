
module Api.Tags
(
  apiGetTags
, apiGetTagByKey
, apiAddTag
)
where

import qualified Data.Text as T
                         ( Text )

import ApiUtility       ( runApi
                        , apiDbGetMultiple, apiDbGetSingle, apiDbInsert )
import Core             ( BrandyActionM )
import DataAccess.Tags  ( getAllTags, getTagByKey, insertTag )


apiGetTags :: BrandyActionM ()
apiGetTags = runApi $
    apiDbGetMultiple getAllTags

apiGetTagByKey :: T.Text -> BrandyActionM ()
apiGetTagByKey keyText = runApi $
    apiDbGetSingle keyText getTagByKey

apiAddTag :: BrandyActionM ()
apiAddTag = runApi $
    apiDbInsert insertTag
