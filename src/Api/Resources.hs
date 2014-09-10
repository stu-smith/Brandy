
module Api.Resources
(
  apiGetResources
, apiGetResource
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
import DataAccess.Resources  ( dbGetAllResources, dbGetResource, dbInsertResource, dbUpdateResource, dbDeleteResource )

apiGetResources :: BrandyActionM ()
apiGetResources = runApiGet $
    apiDbGetMultiple dbGetAllResources

apiGetResource :: T.Text -> BrandyActionM ()
apiGetResource keyText = runApiGet $
    apiDbGetSingle keyText dbGetResource

apiAddResource :: BrandyActionM ()
apiAddResource = runApiPost $ do
    userId <- authenticatedUserId
    apiDbInsert $ dbInsertResource userId

apiUpdateResource :: T.Text -> BrandyActionM ()
apiUpdateResource keyText = runApiPut $
    apiDbUpdate keyText dbUpdateResource

apiDeleteResource :: T.Text -> BrandyActionM ()
apiDeleteResource keyText = runApiDelete $
    apiDbDelete keyText dbDeleteResource
