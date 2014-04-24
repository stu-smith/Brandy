
{-# LANGUAGE OverloadedStrings #-}

module Api.Users
(
  apiGetUsers
, apiGetUserByKey
, apiAddUser
)
where

import Control.Monad.Trans        ( lift )
import qualified Data.Text as T   ( Text, pack )
import Network.HTTP.Types.Status  ( notFound404 )
import Web.Scotty.Trans           ( json, text, status, jsonData )

import ApiUtility                 ( parseKey )
import Core                       ( BrandyActionM )
import DataAccess.Users           ( getAllUsers, getUserByKey, insertUser )
import qualified Json.PrivateUser as PrivateUser
                                  ( PrivateUser(..) )
import qualified Json.PrivateUserPre as PrivateUserPre
                                  ( PrivateUserPre(..) )


apiGetUsers :: BrandyActionM ()
apiGetUsers = do
    users <- lift getAllUsers
    json users

apiGetUserByKey :: T.Text -> BrandyActionM ()
apiGetUserByKey keyText =
    parseKey keyText $ \key -> do
        maybeUser <- lift $ getUserByKey key
        case maybeUser of
            Just user -> json user
            Nothing   -> status notFound404 >> text "User not found."

apiAddUser :: BrandyActionM ()
apiAddUser = do
    userPre <- jsonData
    key <- lift $ insertUser userPre
    json PrivateUser.PrivateUser
        { PrivateUser.id          = T.pack $ show key
        , PrivateUser.displayName = PrivateUserPre.displayName userPre
        , PrivateUser.email       = PrivateUserPre.email userPre
        }
