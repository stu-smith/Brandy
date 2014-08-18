
{-# LANGUAGE OverloadedStrings #-}

module Api.Resources
(
  apiGetResources
, apiGetResourceByKey
, apiAddResource
)
where

import qualified Data.Text as T
                             ( Text )

import ApiUtility            ( runApiGet, runApiPost
                             , apiDbGetMultiple, apiDbGetSingle, apiDbInsert
                             , authenticatedUserId )
import Core                  ( BrandyActionM )
import DataAccess.Resources  ( getAllResources, getResourceByKey, insertResource )

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
