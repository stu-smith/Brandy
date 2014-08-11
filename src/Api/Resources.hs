
{-# LANGUAGE OverloadedStrings #-}

module Api.Resources
(
    apiGetResources
)
where

import ApiUtility            ( runApiGet, apiDbGetMultiple )
import Core                  ( BrandyActionM )
import DataAccess.Resources  ( getAllResources )

apiGetResources :: BrandyActionM ()
apiGetResources = runApiGet $
    apiDbGetMultiple getAllResources
