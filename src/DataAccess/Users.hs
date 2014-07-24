
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

import Core                                         ( DatabaseEnvironmentT )
import Database                                     ( runSql
                                                    , standardGetByKey, standardInsert, standardUpdate, standardDelete )
import Json.PublicUserSummary as PublicUserSummary  ( PublicUserSummary(..) )
import Json.PrivateUser as PrivateUser              ( PrivateUser(..), privateUserMapping )
import Json.WithId                                  ( WithId(..), addId )
import qualified Schema as DB


getAllUsers :: DatabaseEnvironmentT [WithId PublicUserSummary]
getAllUsers =
    runSql $ do
        users <- select $ from $ \u ->
                 return (u ^. DB.UserId, u ^. DB.UserDisplayName)
        return $ map dToJ users
  where
    dToJ (Value uId, Value uDisplayName) =
        addId uId $ PublicUserSummary uDisplayName

getUserByKey :: Key DB.User -> DatabaseEnvironmentT (Maybe (WithId PrivateUser))
getUserByKey =
    standardGetByKey privateUserMapping

insertUser :: PrivateUser -> DatabaseEnvironmentT (Maybe (WithId PrivateUser))
insertUser =
    standardInsert privateUserMapping

updateUser :: Key DB.User -> PrivateUser -> DatabaseEnvironmentT (Maybe (WithId PrivateUser))
updateUser =
    standardUpdate privateUserMapping

deleteUser :: Key DB.User -> DatabaseEnvironmentT ()
deleteUser =
    standardDelete
