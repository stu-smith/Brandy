
{-# LANGUAGE OverloadedStrings #-}

module DataAccess.Users
(
  getAllUsers
, getUserByKey
)
where

import Control.Applicative                          ( (<$>) )
import Data.Text as T                               ( pack )
import Database.Esqueleto                           ( Value(..), select, from, (^.) )
import Database.Persist                             ( get )

import Core                                         ( DatabaseEnvironmentT )
import Database                                     ( runSql )
import Json.PublicUserSummary as PublicUserSummary  ( PublicUserSummary(..) )
import Json.PrivateUser as PrivateUser              ( PrivateUser(..) )
import Schema


getAllUsers :: DatabaseEnvironmentT IO [PublicUserSummary]
getAllUsers =
    runSql $ do
        users <- select $ from $ \u ->
                 return (u ^. UserId, u ^. UserDisplayName)
        return $ map fromSql users
  where
    fromSql (Value uId, Value uDisplayName) =
      PublicUserSummary
          { PublicUserSummary.id          = pack $ show uId
          , PublicUserSummary.displayName = uDisplayName
          }

getUserByKey :: UserId -> DatabaseEnvironmentT IO (Maybe PrivateUser)
getUserByKey key =
    runSql $ do
        maybeUser <- get key
        return $ fromSql key <$> maybeUser
  where
    fromSql uId (User uDisplayName uEmail) =
      PrivateUser
          { PrivateUser.id          = pack $ show uId
          , PrivateUser.displayName = uDisplayName
          , PrivateUser.email       = uEmail
          }
