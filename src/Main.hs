
{-# LANGUAGE OverloadedStrings #-}

module Main
(
  main
)
where

import Control.Applicative         ( (<$>) )
import Control.Monad.Identity      ( runIdentity )
import Control.Monad.Reader        ( Reader )
import Control.Monad.Trans         ( liftIO )
import Control.Monad.Trans.Reader  ( ReaderT(..), runReader )
import Data.Text                   ( Text )
import System.Environment          ( getEnv )
import Web.Scotty                  ( scotty )
import Database.Persist.Sql        ( runMigration )
import Network.Wai.Handler.Warp    ( Port )
import qualified Web.Scotty.Trans as T

import Core                         ( BrandyScottyM )
import Database                     ( runSql )
import Routing                      ( routes )
import Schema                       ( migrate )


main :: IO ()
main = do
  port <- read <$> getEnv "PORT"
  let file = "brandy.sqlite3"
  runSql file $ runMigration migrate
  runScotty port file routes

runScotty :: Port -> Text -> BrandyScottyM () -> IO ()
runScotty port file = T.scottyT port ((\f -> runReader f file)) id
