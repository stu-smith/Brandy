
{-# LANGUAGE OverloadedStrings #-}

module Main
(
  main
)
where

import Control.Applicative                   ( (<$>) )
import Control.Monad.Trans.Reader            ( ReaderT(..), runReaderT )
import qualified Data.Text as T              ( Text )
import System.Environment                    ( getEnv )
import Database.Persist.Sql                  ( runMigration )
import Network.Wai.Handler.Warp              ( Port )
import Network.Wai.Middleware.RequestLogger  ( logStdoutDev )
import Web.Scotty.Trans                      ( scottyT, middleware )

import Core                                  ( BrandyScottyM )
import Database                              ( runSql )
import Routing                               ( routes )
import Schema                                ( migrate )


main :: IO ()
main = do
    port <- read <$> getEnv "PORT"
    let file = "brandy.sqlite3"
    runReaderT (runSql $ runMigration migrate) file
    runScotty port file $ do
        middleware logStdoutDev
        routes

runScotty :: Port -> T.Text -> BrandyScottyM () -> IO ()
runScotty port file =
    scottyT port action action
  where
    action x = runReaderT x file
