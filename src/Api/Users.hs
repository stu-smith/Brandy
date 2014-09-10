
module Api.Users
(
  apiGetUsers
, apiGetUser
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
import DataAccess.Users  ( dbGetAllUsers, dbGetUser, dbInsertUser, dbUpdateUser, dbDeleteUser )


apiGetUsers :: BrandyActionM ()
apiGetUsers = runApiGet $
    apiDbGetMultiple dbGetAllUsers

apiGetUser :: T.Text -> BrandyActionM ()
apiGetUser keyText = runApiGet $
    apiDbGetSingle keyText dbGetUser

apiAddUser :: BrandyActionM ()
apiAddUser = runApiPost $
    apiDbInsert dbInsertUser

apiUpdateUser :: T.Text -> BrandyActionM ()
apiUpdateUser keyText = runApiPut $
    apiDbUpdate keyText dbUpdateUser

apiDeleteUser :: T.Text -> BrandyActionM ()
apiDeleteUser keyText = runApiDelete $
    apiDbDelete keyText dbDeleteUser
