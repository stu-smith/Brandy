
{-# LANGUAGE OverloadedStrings #-}

module Routing
(
  routes
)
where

import Network.HTTP.Types.Status  ( methodNotAllowed405 )
import Web.Scotty.Trans           ( get, put, post, delete, html, param )

import Core                       ( BrandyScottyM, ApiError(..) )
import Api.Users                  ( apiGetUsers, apiGetUser, apiAddUser, apiUpdateUser, apiDeleteUser )
import Api.Resources              ( apiGetResources, apiGetResource
                                  , apiAddResource, apiUpdateResource, apiDeleteResource )
import Api.ResourceContent        ( apiGetResourceContent, apiUpdateResourceContent )
import Api.Tags                   ( apiGetTags, apiGetTag, apiAddTag, apiUpdateTag, apiDeleteTag )
import ApiUtility                 ( apiError )


routes :: BrandyScottyM ()
routes = do

    get     root                   $ html "ROOT"

    get     userCollection           apiGetUsers
    get     userElement            $ apiGetUser                 =<< key
    put     userElement            $ apiUpdateUser              =<< key
    post    userCollection           apiAddUser
    delete  userElement            $ apiDeleteUser              =<< key

    get     resourceCollection       apiGetResources
    get     resourceElement        $ apiGetResource             =<< key
    put     resourceElement        $ apiUpdateResource          =<< key
    post    resourceCollection       apiAddResource
    delete  resourceElement        $ apiDeleteResource          =<< key

    get     resourceContentElement $ apiGetResourceContent      =<< key
    put     resourceContentElement $ apiUpdateResourceContent   =<< key
    post    resourceContentElement   apiNotAllowed
    delete  resourceContentElement   apiNotAllowed

    get     tagCollection            apiGetTags
    get     tagElement             $ apiGetTag                  =<< key
    put     tagElement             $ apiUpdateTag               =<< key
    post    tagCollection            apiAddTag
    delete  tagElement             $ apiDeleteTag               =<< key

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
