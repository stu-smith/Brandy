
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

import ApiUtility        ( showKey, runApi
                         , apiDbGetSingle, apiDbGetMultiple, apiDbInsert )
import Core              ( BrandyActionM )
import DataAccess.Users  ( getAllUsers, getUserByKey, insertUser )
import qualified Json.PrivateUser as PU
                         ( PrivateUser(..) )
import qualified Json.PrivateUserPre as PUP
                         ( PrivateUserPre(..) )


apiGetUsers :: BrandyActionM ()
apiGetUsers = runApi $
    apiDbGetMultiple getAllUsers

apiGetUserByKey :: T.Text -> BrandyActionM ()
apiGetUserByKey keyText = runApi $
    apiDbGetSingle keyText getUserByKey

apiAddUser :: BrandyActionM ()
apiAddUser = runApi $
    apiDbInsert insertUser mkUser
  where mkUser userPre key =
          PU.PrivateUser
                { PU.id          = showKey key
                , PU.displayName = PUP.displayName userPre
                , PU.email       = PUP.email userPre
                }
