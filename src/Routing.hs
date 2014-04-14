
{-# LANGUAGE OverloadedStrings #-}

module Routing
(
  routes
)
where

import Web.Scotty.Trans  ( get, post, delete, html, param )

import Core              ( BrandyScottyM )
import Api.Users         ( apiGetUsers )
import Api.Resources     ( apiGetResources, apiGetResourceByKey
                         , apiInsertResource, apiDeleteResourceByKey )


routes :: BrandyScottyM ()
routes = do
  get    root               $ html "ROOT"

  get    userCollection       apiGetUsers

  get    resourceCollection   apiGetResources
  get    resourceElement    $ apiGetResourceByKey    =<< param "key"
  post   resourceElement      apiInsertResource
  delete resourceElement    $ apiDeleteResourceByKey =<< param "key"

    where root               = "/"
          userCollection     = "/api/users"
          resourceCollection = "/api/resources"
          resourceElement    = "/api/resources/:key"
