
{-# LANGUAGE OverloadedStrings #-}

module Plugins.AdminSite.Plugin
(
  plugin
)
where

import Control.Monad.IO.Class (liftIO)
import Data.Monoid       ( (<>) )
import Web.Scotty.Trans  ( get, text )

import Core              ( BrandyScottyM )
import Plugins           ( PluggedIn, mkPlugin, withRoutesHandler )


plugin :: PluggedIn
plugin =
    mkPlugin <>
    withRoutesHandler testRouteHandler

testRouteHandler :: BrandyScottyM ()
testRouteHandler = do
    liftIO $ putStrLn "testRouteHandler"
    get "/" $ text "ROOT"
    get "/styles.css" $ text "CSS"
