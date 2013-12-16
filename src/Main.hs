
{-# LANGUAGE OverloadedStrings #-}

module Main
(
  main
)
where

import Control.Applicative   ( (<$>) )
import System.Environment    ( getEnv )
import Web.Scotty            ( scotty )
import Database.Persist.Sql  ( runMigration )

import Database              ( runSql )
import Routing               ( routes )
import Schema                ( migrate )


main :: IO ()
main
    = do port <- read <$> getEnv "PORT"
         runSql $ runMigration migrate
         scotty port routes
