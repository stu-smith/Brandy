
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Api.Users
(
  apiGetUsers
)
where

import Control.Monad.Trans   ( lift )
import Web.Scotty.Trans      ( json )
import Database.Esqueleto    ( select, from, entityVal )

import Core                  ( BrandyActionM, DatabaseEnvironmentT )
import Schema
import Database              ( runSql )


apiGetUsers :: BrandyActionM ()
apiGetUsers = do
  users <- lift sqlGetAllUsers
  json users

sqlGetAllUsers :: DatabaseEnvironmentT IO [User]
sqlGetAllUsers =
  runSql $ do
    users <- select $ from return
    return $ map entityVal users
