
{-# LANGUAGE OverloadedStrings #-}

module Plugins.AdminSite.Plugin
(
  plugin
)
where

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
    get "/"           $ text "ROOT"
    get "/styles.css" $ text "CSS"
