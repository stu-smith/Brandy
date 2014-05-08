
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
import Web.Scotty.Trans            ( body )

import ApiUtility                  ( readKey, showKey, apiFail, runApi )
import Core                        ( BrandyActionM )
import DataAccess.Users            ( getAllUsers, getUserByKey, insertUser )
import qualified Json.PrivateUser as PU
                                   ( PrivateUser(..) )
import qualified Json.PrivateUserPre as PUP
                                   ( PrivateUserPre(..) )


apiGetUsers :: BrandyActionM ()
apiGetUsers = runApi $
    lift $ lift getAllUsers

apiGetUserByKey :: T.Text -> BrandyActionM ()
apiGetUserByKey keyText = runApi $ do
    key       <- readKey keyText
    maybeUser <- lift $ lift $ getUserByKey key
    user      <- handleGet maybeUser
    return user
  where handleGet Nothing  = apiFail notFound404 "User not found."
        handleGet (Just u) = return u

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
