
{-# LANGUAGE OverloadedStrings #-}

module Routing
(
  routes
)
where

import Web.Scotty.Trans  ( get, put, post, delete, html, param )

import Core              ( BrandyScottyM )
import Api.Users         ( apiGetUsers, apiGetUserByKey, apiAddUser, apiUpdateUser, apiDeleteUser )
import Api.Resources     ( apiGetResources, apiGetResourceByKey, apiAddResource )
import Api.Tags          ( apiGetTags, apiGetTagByKey, apiAddTag, apiUpdateTag, apiDeleteTag )


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
    post   resourceCollection   apiAddResource

    get    tagCollection        apiGetTags
    get    tagElement         $ apiGetTagByKey         =<< key
    put    tagElement         $ apiUpdateTag           =<< key
    post   tagCollection        apiAddTag
    delete tagElement         $ apiDeleteTag           =<< key

  where
    
    root               = "/"
    userCollection     = "/api/users"
    userElement        = "/api/users/:key"
    resourceCollection = "/api/resources"
    resourceElement    = "/api/resources/:key"
    tagCollection      = "/api/tags"
    tagElement         = "/api/tags/:key"

    key                = param "key"
