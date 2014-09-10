
module Api.Tags
(
  apiGetTags
, apiGetTag
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
import DataAccess.Tags  ( dbGetAllTags, dbGetTag, dbInsertTag, dbUpdateTag, dbDeleteTag )


apiGetTags :: BrandyActionM ()
apiGetTags = runApiGet $
    apiDbGetMultiple dbGetAllTags

apiGetTag :: T.Text -> BrandyActionM ()
apiGetTag keyText = runApiGet $
    apiDbGetSingle keyText dbGetTag

apiAddTag :: BrandyActionM ()
apiAddTag = runApiPost $
    apiDbInsert dbInsertTag

apiUpdateTag :: T.Text -> BrandyActionM ()
apiUpdateTag keyText = runApiPut $
    apiDbUpdate keyText dbUpdateTag

apiDeleteTag :: T.Text -> BrandyActionM ()
apiDeleteTag keyText = runApiDelete $
    apiDbDelete keyText dbDeleteTag
