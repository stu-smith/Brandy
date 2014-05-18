
{-# LANGUAGE OverloadedStrings #-}

module Api.Users
(
  apiGetUsers
, apiGetUserByKey
, apiAddUser
)
where

import qualified Data.Text as T    ( Text )

import ApiUtility                  ( readKey, showKey, runApi, liftDB
                                   , validateDbGet, validateDbInsert, validateBody )
import Core                        ( BrandyActionM )
import DataAccess.Users            ( getAllUsers, getUserByKey, insertUser )
import qualified Json.PrivateUser as PU
                                   ( PrivateUser(..) )
import qualified Json.PrivateUserPre as PUP
                                   ( PrivateUserPre(..) )


apiGetUsers :: BrandyActionM ()
apiGetUsers = runApi $
    liftDB getAllUsers

apiGetUserByKey :: T.Text -> BrandyActionM ()
apiGetUserByKey keyText = runApi $ do
    key       <- readKey keyText
    maybeUser <- liftDB $ getUserByKey key
    validateDbGet maybeUser

apiAddUser :: BrandyActionM ()
apiAddUser = runApi $ do
    userPre  <- validateBody
    maybeKey <- liftDB . insertUser $ userPre
    key      <- validateDbInsert maybeKey
    return PU.PrivateUser
                { PU.id          = showKey key
                , PU.displayName = PUP.displayName userPre
                , PU.email       = PUP.email userPre
                }
