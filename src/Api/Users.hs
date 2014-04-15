
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
import Control.Monad.Reader  ( ReaderT, ask )
import Data.Text as T        ( Text )

import Web.Scotty.Trans      ( json )
import Database.Esqueleto    ( select, from, entityVal )

import Core                  ( BrandyActionM )
import Schema
import Database              ( runSql )


apiGetUsers :: BrandyActionM ()
apiGetUsers = do
  users <- lift sqlGetAllUsers
  json users

sqlGetAllUsers :: ReaderT T.Text IO [User]
sqlGetAllUsers = do
  conn <- ask
  runSql conn $ do
    users <- select $ from return
    return $ map entityVal users
