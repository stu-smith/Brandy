
{-# LANGUAGE OverloadedStrings #-}

module Routing
(
  routes
)
where

import Web.Scotty  ( ScottyM, get, post, delete, html, param , regex)

import Api.Users             ( apiGetUsers )
import Api.Resources         ( apiGetResources, apiGetNamedResource
	                         , apiInsertNamedResource, apiDeleteNamedResource )


routes :: ScottyM ()
routes
    = do get    rootPattern               $ html "ROOT"

         get    userCollectionPattern       apiGetUsers
         
         get    resourceCollectionPattern   apiGetResources
         get    resourceElementPattern    $ apiGetNamedResource =<< param "1"
         post   resourceElementPattern    $ apiInsertNamedResource =<< param "1"
         delete resourceElementPattern    $ apiDeleteNamedResource =<< param "1"

  where rootPattern = "/"
        userCollectionPattern = "/api/users"
        resourceCollectionPattern = "/api/resources"
        resourceElementPattern = regex "^/api/resources/(.*)$" -- "/api/resources/*name"