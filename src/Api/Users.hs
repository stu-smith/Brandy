
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
import Network.HTTP.Types.Status  ( badRequest400, notFound404 )
import Web.Scotty.Trans           ( json, text, status )

import ApiUtility                 ( parseKey, handleJson )
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
            Nothing   -> status notFound404 >> text "User not found."
            Just user -> json user

apiAddUser :: BrandyActionM ()
apiAddUser =
    handleJson
        (status badRequest400 >> text "Invalid request body.")
        $ \userPre -> do
            key <- lift $ insertUser userPre
            json PrivateUser.PrivateUser
                { PrivateUser.id          = T.pack $ show key
                , PrivateUser.displayName = PrivateUserPre.displayName userPre
                , PrivateUser.email       = PrivateUserPre.email userPre
                }
