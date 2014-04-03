
{-# LANGUAGE OverloadedStrings #-}

module Routing
(
  routes
)
where

import Web.Scotty     ( ScottyM, get, post, delete, html, param )

import Api.Users      ( apiGetUsers )
import Api.Resources  ( apiGetResources, apiGetResourceByKey
                      , apiInsertResource, apiDeleteResourceByKey )


routes :: ScottyM ()
routes
    = do get    rootPattern               $ html "ROOT"

         get    userCollectionPattern       apiGetUsers
         
         get    resourceCollectionPattern   apiGetResources
         get    resourceElementPattern    $ apiGetResourceByKey    =<< param "key"
         post   resourceElementPattern      apiInsertResource
         delete resourceElementPattern    $ apiDeleteResourceByKey =<< param "key"

  where rootPattern               = "/"
        userCollectionPattern     = "/api/users"
        resourceCollectionPattern = "/api/resources"
        resourceElementPattern    = "/api/resources/:key"
