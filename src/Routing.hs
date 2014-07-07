
{-# LANGUAGE OverloadedStrings #-}

module Routing
(
  routes
)
where

import Web.Scotty.Trans  ( get, put, post, delete, html, param )

import Core              ( BrandyScottyM )
import Api.Users         ( apiGetUsers, apiGetUserByKey, apiAddUser, apiUpdateUser, apiDeleteUser )
import Api.Resources     ( apiGetResources, apiGetResourceByKey
                         , apiInsertResource, apiDeleteResourceByKey )


routes :: BrandyScottyM ()
routes = do
    get    root               $ html "ROOT"

    get    userCollection       apiGetUsers
    get    userElement        $ apiGetUserByKey        =<< key
    put    userElement        $ apiUpdateUser          =<< key
    post   userCollection       apiAddUser
    delete userElement        $ apiDeleteUser          =<< key

    get    resourceCollection   apiGetResources
    get    resourceElement    $ apiGetResourceByKey    =<< key
    post   resourceElement      apiInsertResource
    delete resourceElement    $ apiDeleteResourceByKey =<< key

  where root               = "/"
        userCollection     = "/api/users"
        userElement        = "/api/users/:key"
        resourceCollection = "/api/resources"
        resourceElement    = "/api/resources/:key"

        key                = param "key"
