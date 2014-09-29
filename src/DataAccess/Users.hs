
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DataAccess.Users
(
  dbGetAllUsers
, dbGetUser
, dbInsertUser
, dbUpdateUser
, dbDeleteUser
)
where

import Database.Esqueleto                           ( Value(..), select, from, (^.) )
import Database.Persist                             ( Key )

import Core                                         ( DatabaseEnvironmentT )
import Database                                     ( runSql
                                                    , standardGetByKey, standardInsert, standardUpdate, standardDelete )
import Json.PublicUserSummary as PublicUserSummary  ( PublicUserSummary(..) )
import Json.PrivateUser as PrivateUser              ( PrivateUser(..), privateUserMapping )
import Json.WithId                                  ( WithId, addId )
import qualified Schema as DB


dbGetAllUsers :: DatabaseEnvironmentT [WithId PublicUserSummary]
dbGetAllUsers =
    runSql $ do
        users <- select $ from $ \u ->
                 return (u ^. DB.UserId, u ^. DB.UserDisplayName)
        return $ map dToJ users
  where
    dToJ (Value uId, Value uDisplayName) =
        addId uId $ PublicUserSummary uDisplayName

dbGetUser :: Key DB.User -> DatabaseEnvironmentT (Maybe (WithId PrivateUser))
dbGetUser =
    standardGetByKey privateUserMapping

dbInsertUser :: PrivateUser -> DatabaseEnvironmentT (Maybe (WithId PrivateUser))
dbInsertUser =
    standardInsert privateUserMapping

dbUpdateUser :: Key DB.User -> PrivateUser -> DatabaseEnvironmentT (Maybe (WithId PrivateUser))
dbUpdateUser =
    standardUpdate privateUserMapping

dbDeleteUser :: Key DB.User -> DatabaseEnvironmentT ()
dbDeleteUser =
    standardDelete
