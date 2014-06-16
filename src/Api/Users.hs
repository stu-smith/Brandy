
{-# LANGUAGE OverloadedStrings #-}

module Api.Users
(
  apiGetUsers
, apiGetUserByKey
, apiAddUser
)
where

import qualified Data.Text as T
                         ( Text )

import ApiUtility        ( runApi
                         , apiDbGetSingle, apiDbGetMultiple, apiDbInsert )
import Core              ( BrandyActionM )
import DataAccess.Users  ( getAllUsers, getUserByKey, insertUser )


apiGetUsers :: BrandyActionM ()
apiGetUsers = runApi $
    apiDbGetMultiple getAllUsers

apiGetUserByKey :: T.Text -> BrandyActionM ()
apiGetUserByKey keyText = runApi $
    apiDbGetSingle keyText getUserByKey

apiAddUser :: BrandyActionM ()
apiAddUser = runApi $
    apiDbInsert insertUser
