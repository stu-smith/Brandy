
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

import ApiUtility        ( runApiGet, runApiPut, runApiPost, runApiDelete
                         , apiDbGetSingle, apiDbGetMultiple, apiDbInsert, apiDbUpdate, apiDbDelete )
import Core              ( BrandyActionM )
import DataAccess.Users  ( getAllUsers, getUserByKey, insertUser, updateUser, deleteUser )


apiGetUsers :: BrandyActionM ()
apiGetUsers = runApiGet $
    apiDbGetMultiple getAllUsers

apiGetUserByKey :: T.Text -> BrandyActionM ()
apiGetUserByKey keyText = runApiGet $
    apiDbGetSingle keyText getUserByKey

apiAddUser :: BrandyActionM ()
apiAddUser = runApiPost $
    apiDbInsert insertUser

apiUpdateUser :: T.Text -> BrandyActionM ()
apiUpdateUser keyText = runApiPut $
    apiDbUpdate keyText updateUser

apiDeleteUser :: T.Text -> BrandyActionM ()
apiDeleteUser keyText = runApiDelete $
    apiDbDelete keyText deleteUser
