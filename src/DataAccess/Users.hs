
{-# LANGUAGE OverloadedStrings #-}

module DataAccess.Users
(
  getAllUsers
, getUserByKey
, insertUser
, updateUser
, deleteUser
)
where

import Control.Applicative                          ( (<$>) )
import Database.Esqueleto                           ( Value(..), select, from, (^.) )
import Database.Persist                             ( Entity(..), get, insert, replace, delete )
import qualified Data.Text as T                     ( Text )

import Core                                         ( DatabaseEnvironmentT )
import Database                                     ( runSql, runSqlMaybe )
import Json.PublicUserSummary as PublicUserSummary  ( PublicUserSummary(..) )
import Json.PrivateUser as PrivateUser              ( PrivateUser(..) )
import Json.WithId                                  ( WithId(..), addId )
import Schema


getAllUsers :: DatabaseEnvironmentT [WithId PublicUserSummary]
getAllUsers =
    runSql $ do
        users <- select $ from $ \u ->
                 return (u ^. UserId, u ^. UserDisplayName)
        return $ map dbToApiPublic users

getUserByKey :: UserId -> DatabaseEnvironmentT (Maybe (WithId PrivateUser))
getUserByKey key = do
    maybeUser <- runSql $ get key
    return $ toApi <$> maybeUser
  where toApi user = dbToApiPrivate $ Entity key user

insertUser :: PrivateUser -> DatabaseEnvironmentT (Maybe (WithId PrivateUser))
insertUser user = do
    maybeUserId <- runSqlMaybe $ insert dbUser
    return $ toApi <$> maybeUserId
  where dbUser       = apiToDbPrivate user
        toApi userId = dbToApiPrivate $ Entity userId dbUser

updateUser :: UserId -> PrivateUser -> DatabaseEnvironmentT (Maybe (WithId PrivateUser))
updateUser key privateUser =
    runSql $ do
        replace key $ apiToDbPrivate privateUser
        return $ Just $ addId key privateUser

deleteUser :: UserId -> DatabaseEnvironmentT ()
deleteUser key =
    runSql $ delete key

dbToApiPrivate :: Entity User -> WithId PrivateUser
dbToApiPrivate (Entity uId (User uDisplayName uEmail)) =
    addId uId
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
    addId uId
        PublicUserSummary
            { PublicUserSummary.displayName = uDisplayName
            }
