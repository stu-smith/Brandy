
{-# LANGUAGE OverloadedStrings #-}

module Routing
(
  routes
)
where

import Network.HTTP.Types.Status  ( methodNotAllowed405 )
import Web.Scotty.Trans           ( get, put, post, delete, html, param )

import Core                       ( BrandyScottyM, ApiError(..) )
import Api.Users                  ( apiGetUsers, apiGetUserByKey, apiAddUser, apiUpdateUser, apiDeleteUser )
import Api.Resources              ( apiGetResources, apiGetResourceByKey, apiAddResource, apiUpdateResource, apiDeleteResource )
import Api.Tags                   ( apiGetTags, apiGetTagByKey, apiAddTag, apiUpdateTag, apiDeleteTag )
import ApiUtility                 ( apiError )


routes :: BrandyScottyM ()
routes = do

    get    root                   $ html "ROOT"

    get    userCollection           apiGetUsers
    get    userElement            $ apiGetUserByKey         =<< key
    put    userElement            $ apiUpdateUser           =<< key
    post   userCollection           apiAddUser
    delete userElement            $ apiDeleteUser           =<< key

    get    resourceCollection       apiGetResources
    get    resourceElement        $ apiGetResourceByKey     =<< key
    put    resourceElement        $ apiUpdateResource       =<< key
    post   resourceCollection       apiAddResource
    delete resourceElement        $ apiDeleteResource       =<< key

    post   resourceContentElement   apiNotAllowed
    delete resourceContentElement   apiNotAllowed

    get    tagCollection            apiGetTags
    get    tagElement             $ apiGetTagByKey          =<< key
    put    tagElement             $ apiUpdateTag            =<< key
    post   tagCollection            apiAddTag
    delete tagElement             $ apiDeleteTag            =<< key

  where
    
    root                   = "/"
    userCollection         = "/api/users"
    userElement            = "/api/users/:key"
    resourceCollection     = "/api/resources"
    resourceElement        = "/api/resources/:key"
    resourceContentElement = "/api/resources/:key/content"
    tagCollection          = "/api/tags"
    tagElement             = "/api/tags/:key"

    key                    = param "key"

    apiNotAllowed          = apiError $ ApiError methodNotAllowed405 "Not allowed."
