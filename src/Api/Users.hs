
{-# LANGUAGE OverloadedStrings #-}

module Api.Users
(
  apiGetUsers
, apiGetUserByKey
, apiAddUser
)
where

import Control.Applicative        ( (<$>) )
import Control.Monad.Trans        ( lift )
import Data.Aeson                 ( decode )
import qualified Data.Text as T   ( Text )
import Network.HTTP.Types.Status  ( badRequest400, notFound404 )
import Web.Scotty.Trans           ( json, text, status, body )

import ApiUtility                 ( readKey, showKey )
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
    case readKey keyText of
        Nothing  -> status badRequest400 >> text "Invalid ID."
        Just key -> do
            maybeUser <- lift $ getUserByKey key
            case maybeUser of
                Nothing   -> status notFound404 >> text "User not found."
                Just user -> json user

apiAddUser :: BrandyActionM ()
apiAddUser = do
    maybeUserPre <- decode <$> body
    case maybeUserPre of
        Nothing      -> status badRequest400 >> text "Invalid request body."
        Just userPre -> do
            key <- lift $ insertUser userPre
            json PrivateUser.PrivateUser
                { PrivateUser.id          = showKey key
                , PrivateUser.displayName = PrivateUserPre.displayName userPre
                , PrivateUser.email       = PrivateUserPre.email userPre
                }
