
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Api.Users
(
  apiGetUsers
)
where

import Control.Monad.Trans   ( lift, liftIO )
import Control.Monad.Reader  ( ask )
import Data.Text as T        ( Text )

import Web.Scotty.Trans      ( json )
import Database.Esqueleto    ( select, from, entityVal )

import Core                  ( BrandyActionM )
import Schema
import Database              ( runSql )


apiGetUsers :: BrandyActionM ()
apiGetUsers = do
  conn <- lift ask
  users <- liftIO $ sqlGetAllUsers conn
  json users

sqlGetAllUsers :: T.Text -> IO [User]
sqlGetAllUsers conn = do
  runSql conn $ do
    users <- select $ from return
    return $ map entityVal users
