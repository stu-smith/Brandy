
{-# LANGUAGE OverloadedStrings #-}

module Routing
(
  routes
)
where

import Web.Scotty  ( ScottyM, get, html )

import Api.Users             ( apiGetUsers )
import Api.Resources         ( apiGetResources )


routes :: ScottyM ()
routes
    = do get "/" $ html "ROOT"
         get "/api/users"     apiGetUsers
         get "/api/resources" apiGetResources
