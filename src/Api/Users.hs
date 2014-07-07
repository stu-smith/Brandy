
{-# LANGUAGE OverloadedStrings #-}

module Api.Users
(
  apiGetUsers
, apiGetUserByKey
, apiAddUser
, apiUpdateUser
, apiDeleteUser
)
where

import qualified Data.Text as T
                         ( Text )

import ApiUtility        ( runApi
                         , apiDbGetSingle, apiDbGetMultiple, apiDbInsert, apiDbUpdate, apiDbDelete )
import Core              ( BrandyActionM )
import DataAccess.Users  ( getAllUsers, getUserByKey, insertUser, updateUser, deleteUser )


apiGetUsers :: BrandyActionM ()
apiGetUsers = runApi $
    apiDbGetMultiple getAllUsers

apiGetUserByKey :: T.Text -> BrandyActionM ()
apiGetUserByKey keyText = runApi $
    apiDbGetSingle keyText getUserByKey

apiAddUser :: BrandyActionM ()
apiAddUser = runApi $
    apiDbInsert insertUser

apiUpdateUser :: T.Text -> BrandyActionM ()
apiUpdateUser keyText = runApi $
    apiDbUpdate keyText updateUser

apiDeleteUser :: T.Text -> BrandyActionM ()
apiDeleteUser keyText = runApi $
    apiDbDelete keyText deleteUser
