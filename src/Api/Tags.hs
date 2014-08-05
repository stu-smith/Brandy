
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

import ApiUtility       ( runApiGet, runApiPut, runApiPost, runApiDelete
                        , apiDbGetMultiple, apiDbGetSingle, apiDbInsert, apiDbUpdate, apiDbDelete )
import Core             ( BrandyActionM )
import DataAccess.Tags  ( getAllTags, getTagByKey, insertTag, updateTag, deleteTag )


apiGetTags :: BrandyActionM ()
apiGetTags = runApiGet $
    apiDbGetMultiple getAllTags

apiGetTagByKey :: T.Text -> BrandyActionM ()
apiGetTagByKey keyText = runApiGet $
    apiDbGetSingle keyText getTagByKey

apiAddTag :: BrandyActionM ()
apiAddTag = runApiPost $
    apiDbInsert insertTag

apiUpdateTag :: T.Text -> BrandyActionM ()
apiUpdateTag keyText = runApiPut $
    apiDbUpdate keyText updateTag

apiDeleteTag :: T.Text -> BrandyActionM ()
apiDeleteTag keyText = runApiDelete $
    apiDbDelete keyText deleteTag
