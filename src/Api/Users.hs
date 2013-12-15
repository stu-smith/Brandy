
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Api.Users
(
  apiGetUsers
)
where

import Control.Monad.IO.Class  ( liftIO )
import Web.Scotty              ( ActionM, json )
import Database.Esqueleto      ( select, from, entityVal )

import Schema
import Database                ( runSql )


apiGetUsers :: ActionM ()
apiGetUsers
    = do au <- liftIO allUsers
         json $ map userDisplayName au

allUsers :: IO [User]
allUsers
    = runSql $ do names <- select $ from return
                  return $ map entityVal names