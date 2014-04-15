
{-# LANGUAGE OverloadedStrings #-}

module Main
(
  main
)
where

import Control.Applicative         ( (<$>) )
import Control.Monad.Trans.Reader  ( ReaderT(..), runReaderT )
import Data.Text                   ( Text )
import System.Environment          ( getEnv )
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
  runReaderT (runSql $ runMigration migrate) file
  runScotty port file routes

runScotty :: Port -> Text -> BrandyScottyM () -> IO ()
runScotty port file =
  T.scottyT port (`runReaderT` file) (`runReaderT` file)
