
{-# LANGUAGE OverloadedStrings #-}

module DataAccess.Users
(
  getAllUsers
, getUserByKey
, insertUser
)
where

import Database.Esqueleto                           ( Value(..), select, from, (^.) )
import Database.Persist                             ( Entity(..), get, insertUnique )
import qualified Data.Text as T                     ( Text )

import Core                                         ( DatabaseEnvironmentT )
import Database                                     ( runSql )
import Json.PublicUserSummary as PublicUserSummary  ( PublicUserSummary(..) )
import Json.PrivateUser as PrivateUser              ( PrivateUser(..) )
import Json.WithId                                  ( WithId(..), withId )
import Schema


getAllUsers :: DatabaseEnvironmentT [WithId PublicUserSummary]
getAllUsers =
    runSql $ do
        users <- select $ from $ \u ->
                 return (u ^. UserId, u ^. UserDisplayName)
        return $ map fromPublic users

getUserByKey :: UserId -> DatabaseEnvironmentT (Maybe (WithId PrivateUser))
getUserByKey key =
    runSql $ do
        maybeUser <- get key
        return $ do
            user <- maybeUser
            return . fromPrivate $ Entity key user

insertUser :: PrivateUser -> DatabaseEnvironmentT (Maybe (WithId PrivateUser))
insertUser (PrivateUser uDisplayName uEmail) = do
    let user = User { userDisplayName = uDisplayName
                    , userEmail       = uEmail }
    maybeId <- runSql $ insertUnique user
    return $ do
        uId <- maybeId
        return . fromPrivate $ Entity uId user

fromPrivate :: Entity User -> WithId PrivateUser
fromPrivate (Entity uId (User uDisplayName uEmail)) =
    withId uId
        PrivateUser
            { PrivateUser.displayName = uDisplayName
            , PrivateUser.email = uEmail
            }

fromPublic :: (Value UserId, Value T.Text) -> WithId PublicUserSummary
fromPublic (Value uId, Value uDisplayName) =
    withId uId
        PublicUserSummary
            { PublicUserSummary.displayName = uDisplayName
            }
