
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DataAccess.Users
(
  getAllUsers
, getUserByKey
, insertUser
, updateUser
, deleteUser
)
where

import Database.Esqueleto                           ( Value(..), select, from, (^.) )
import Database.Persist                             ( Key )
import qualified Data.Text as T                     ( Text )

import Core                                         ( DatabaseEnvironmentT )
import Database                                     ( runSql
                                                    , standardGetByKey, standardInsert, standardUpdate, standardDelete )
import Json.PublicUserSummary as PublicUserSummary  ( PublicUserSummary(..) )
import Json.PrivateUser as PrivateUser              ( PrivateUser(..), privateUserMapping )
import Json.WithId                                  ( WithId(..), addId )
import Schema


getAllUsers :: DatabaseEnvironmentT [WithId PublicUserSummary]
getAllUsers =
    runSql $ do
        users <- select $ from $ \u ->
                 return (u ^. UserId, u ^. UserDisplayName)
        return $ map dbToApiPublic users

getUserByKey :: Key User -> DatabaseEnvironmentT (Maybe (WithId PrivateUser))
getUserByKey = standardGetByKey privateUserMapping

insertUser :: PrivateUser -> DatabaseEnvironmentT (Maybe (WithId PrivateUser))
insertUser = standardInsert privateUserMapping

updateUser :: Key User -> PrivateUser -> DatabaseEnvironmentT (Maybe (WithId PrivateUser))
updateUser  = standardUpdate privateUserMapping

deleteUser :: Key User -> DatabaseEnvironmentT ()
deleteUser = standardDelete

dbToApiPublic :: (Value UserId, Value T.Text) -> WithId PublicUserSummary
dbToApiPublic (Value uId, Value uDisplayName) =
    addId uId
        PublicUserSummary
            { PublicUserSummary.displayName = uDisplayName
            }
