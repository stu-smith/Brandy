
{-# LANGUAGE OverloadedStrings #-}

module DataAccess.Users
(
  getAllUsers
, getUserByKey
, insertUser
)
where

import Control.Applicative                          ( (<$>) )
import Data.Text as T                               ( Text, pack )
import Database.Esqueleto                           ( Value(..), select, from, (^.) )
import Database.Persist                             ( get, insert )

import Core                                         ( DatabaseEnvironmentT )
import Database                                     ( runSql )
import Json.PublicUserSummary as PublicUserSummary  ( PublicUserSummary(..) )
import Json.PrivateUser as PrivateUser              ( PrivateUser(..) )
import Json.PrivateUserPre as PrivateUserPre        ( PrivateUserPre(..) )
import Schema


getAllUsers :: DatabaseEnvironmentT IO [PublicUserSummary]
getAllUsers =
    runSql $ do
        users <- select $ from $ \u ->
                 return (u ^. UserId, u ^. UserDisplayName)
        return $ map fromPublic users

getUserByKey :: UserId -> DatabaseEnvironmentT IO (Maybe PrivateUser)
getUserByKey key =
    runSql $ do
        maybeUser <- get key
        return $ fromPrivate key <$> maybeUser

insertUser :: PrivateUserPre -> DatabaseEnvironmentT IO UserId
insertUser (PrivateUserPre uDisplayName uEmail) =
    runSql $ insert User { userDisplayName = uDisplayName
                         , userEmail       = uEmail }

fromPublic :: (Value UserId, Value T.Text) -> PublicUserSummary
fromPublic (Value uId, Value uDisplayName) =
    PublicUserSummary
        { PublicUserSummary.id          = pack $ show uId
        , PublicUserSummary.displayName = uDisplayName
        }

fromPrivate :: UserId -> User -> PrivateUser
fromPrivate uId (User uDisplayName uEmail) =
    PrivateUser
        { PrivateUser.id          = pack $ show uId
        , PrivateUser.displayName = uDisplayName
        , PrivateUser.email       = uEmail
        }
