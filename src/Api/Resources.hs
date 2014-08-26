
module Api.Resources
(
  apiGetResources
, apiGetResourceByKey
, apiAddResource
, apiUpdateResource
, apiDeleteResource
)
where

import qualified Data.Text as T
                             ( Text )

import ApiUtility            ( runApiGet, runApiPost, runApiPut, runApiDelete
                             , apiDbGetMultiple, apiDbGetSingle, apiDbInsert, apiDbUpdate, apiDbDelete
                             , authenticatedUserId )
import Core                  ( BrandyActionM )
import DataAccess.Resources  ( getAllResources, getResourceByKey, insertResource, updateResource, deleteResource )

apiGetResources :: BrandyActionM ()
apiGetResources = runApiGet $
    apiDbGetMultiple getAllResources

apiGetResourceByKey :: T.Text -> BrandyActionM ()
apiGetResourceByKey keyText = runApiGet $
    apiDbGetSingle keyText getResourceByKey

apiAddResource :: BrandyActionM ()
apiAddResource = runApiPost $ do
    userId <- authenticatedUserId
    apiDbInsert $ insertResource userId

apiUpdateResource :: T.Text -> BrandyActionM ()
apiUpdateResource keyText = runApiPut $
    apiDbUpdate keyText updateResource

apiDeleteResource :: T.Text -> BrandyActionM ()
apiDeleteResource keyText = runApiDelete $
    apiDbDelete keyText deleteResource
