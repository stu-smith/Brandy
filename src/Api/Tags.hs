
module Api.Tags
(
  apiGetTags
, apiGetTagByKey
, apiAddTag
, apiUpdateTag
, apiDeleteTag
)
where

import qualified Data.Text as T
                         ( Text )

import ApiUtility       ( runApi, runApi_
                        , apiDbGetMultiple, apiDbGetSingle, apiDbInsert, apiDbUpdate, apiDbDelete )
import Core             ( BrandyActionM )
import DataAccess.Tags  ( getAllTags, getTagByKey, insertTag, updateTag, deleteTag )


apiGetTags :: BrandyActionM ()
apiGetTags = runApi $
    apiDbGetMultiple getAllTags

apiGetTagByKey :: T.Text -> BrandyActionM ()
apiGetTagByKey keyText = runApi $
    apiDbGetSingle keyText getTagByKey

apiAddTag :: BrandyActionM ()
apiAddTag = runApi $
    apiDbInsert insertTag

apiUpdateTag :: T.Text -> BrandyActionM ()
apiUpdateTag keyText = runApi $
    apiDbUpdate keyText updateTag

apiDeleteTag :: T.Text -> BrandyActionM ()
apiDeleteTag keyText = runApi_ $
    apiDbDelete keyText deleteTag
