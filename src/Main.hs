
{-# LANGUAGE OverloadedStrings #-}

module Main
(
  main
)
where

import Control.Applicative                   ( (<$>) )
import Control.Monad.Trans.Reader            ( ReaderT(..), runReaderT )
import Data.Monoid                           ( mconcat )
import qualified Data.Text as T              ( Text )
import System.Environment                    ( getEnv )
import Database.Persist.Sql                  ( runMigration )
import Network.Wai.Handler.Warp              ( Port )
import Network.Wai.Middleware.RequestLogger  ( logStdoutDev )
import Web.Scotty.Trans                      ( scottyT, middleware )

import Core                                  ( BrandyScottyM )
import Database                              ( runSql )
import Plugins                               ( PluggedIn )
import Routing                               ( routes )
import Schema                                ( migrate )

import qualified Plugins.AdminSite.Plugin as AdminSite
                                             ( plugin )


main :: IO ()
main = do
    port <- read <$> getEnv "PORT"
    let file = "brandy.sqlite3"
    runReaderT (runSql $ runMigration migrate) file
    runScotty port file $ do
        middleware logStdoutDev
        routes plugins

runScotty :: Port -> T.Text -> BrandyScottyM () -> IO ()
runScotty port file =
    scottyT port action action
  where
    action x = runReaderT x file

plugins :: PluggedIn
plugins =
    mconcat [ AdminSite.plugin
            ]
