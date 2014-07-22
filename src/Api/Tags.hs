
module Api.Tags
(
  apiGetTags
)
where

import ApiUtility       ( runApi
                        , apiDbGetMultiple )
import Core             ( BrandyActionM )
import DataAccess.Tags  ( getAllTags )


apiGetTags :: BrandyActionM ()
apiGetTags = runApi $
    apiDbGetMultiple getAllTags
