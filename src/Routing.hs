
{-# LANGUAGE OverloadedStrings #-}

module Routing
(
  routes
)
where

import Web.Scotty  ( ScottyM, get, html, param )

import Api.Users             ( apiGetUsers )
import Api.Resources         ( apiGetResources, apiGetNamedResource )


routes :: ScottyM ()
routes
    = do get "/"                    $ html "ROOT"
         get "/api/users"             apiGetUsers
         get "/api/resources"         apiGetResources
         get "/api/resources/:name" $ apiGetNamedResource =<< param "name"
