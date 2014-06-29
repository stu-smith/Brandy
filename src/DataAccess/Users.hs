
{-# LANGUAGE OverloadedStrings #-}

module DataAccess.Users
(
  getAllUsers
, getUserByKey
, insertUser
, updateUser
)
where

import Database.Esqueleto                           ( Value(..), select, from, (^.) )
import Database.Persist                             ( Entity(..), get, insert, replace )
import qualified Data.Text as T                     ( Text )

import Core                                         ( DatabaseEnvironmentT )
import Database                                     ( runSql, runSqlMaybe )
import Json.PublicUserSummary as PublicUserSummary  ( PublicUserSummary(..) )
import Json.PrivateUser as PrivateUser              ( PrivateUser(..) )
import Json.WithId                                  ( WithId(..), withId )
import Schema


getAllUsers :: DatabaseEnvironmentT [WithId PublicUserSummary]
getAllUsers =
    runSql $ do
        users <- select $ from $ \u ->
                 return (u ^. UserId, u ^. UserDisplayName)
        return $ map dbToApiPublic users

getUserByKey :: UserId -> DatabaseEnvironmentT (Maybe (WithId PrivateUser))
getUserByKey key =
    runSql $ do
        maybeUser <- get key
        return $ do
            user <- maybeUser
            return . dbToApiPrivate $ Entity key user

insertUser :: PrivateUser -> DatabaseEnvironmentT (Maybe (WithId PrivateUser))
insertUser user = do
    let dbUser = apiToDbPrivate user
    maybeUserId <- runSqlMaybe $ insert dbUser
    return $ case maybeUserId of
        Nothing     -> Nothing
        Just userId -> Just . dbToApiPrivate $ Entity userId dbUser

updateUser :: UserId -> PrivateUser -> DatabaseEnvironmentT (Maybe PrivateUser)
updateUser key privateUser =
    runSql $ do
        replace key $ apiToDbPrivate privateUser
        return $ Just privateUser

dbToApiPrivate :: Entity User -> WithId PrivateUser
dbToApiPrivate (Entity uId (User uDisplayName uEmail)) =
    withId uId
        PrivateUser
            { PrivateUser.displayName = uDisplayName
            , PrivateUser.email = uEmail
            }

apiToDbPrivate :: PrivateUser -> User
apiToDbPrivate (PrivateUser uDisplayName uEmail) =
    User
        { userDisplayName = uDisplayName
        , userEmail       = uEmail
        }

dbToApiPublic :: (Value UserId, Value T.Text) -> WithId PublicUserSummary
dbToApiPublic (Value uId, Value uDisplayName) =
    withId uId
        PublicUserSummary
            { PublicUserSummary.displayName = uDisplayName
            }
