
{-# LANGUAGE OverloadedStrings #-}

module Api.Users
(
  apiGetUsers
, apiGetUserByKey
, apiAddUser
)
where

import Control.Applicative         ( (<$>) )
import Control.Monad.Trans         ( lift )
import Data.Aeson                  ( decode )
import qualified Data.Text as T    ( Text, null )
import Network.HTTP.Types.Status   ( badRequest400, notFound404, conflict409 )
import Web.Scotty.Trans            ( json, text, status, body )

import ApiUtility                  ( readKey, showKey, apiFail, runApi )
import Core                        ( BrandyActionM )
import DataAccess.Users            ( getAllUsers, getUserByKey, insertUser )
import qualified Json.PrivateUser as PU
                                   ( PrivateUser(..) )
import qualified Json.PrivateUserPre as PUP
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
apiAddUser = runApi $ do
    maybeUserPre <- lift $ decode <$> body
    userPre      <- validate maybeUserPre
    maybeKey     <- lift $ lift $ insertUser userPre
    key          <- handleInsert maybeKey
    return PU.PrivateUser
                { PU.id          = showKey key
                , PU.displayName = PUP.displayName userPre
                , PU.email       = PUP.email userPre
                }
  where validate Nothing         = apiFail badRequest400 "Invalid request body."
        validate (Just userPre@(PUP.PrivateUserPre displayName email))
            | T.null displayName = apiFail badRequest400 "Missing displayName."
            | T.null email       = apiFail badRequest400 "Missing email."
            | otherwise          = return userPre
        handleInsert Nothing     = apiFail conflict409 "Already in use."
        handleInsert (Just k)    = return k
