
{-# LANGUAGE OverloadedStrings #-}

module Routing
(
  routes
)
where

import Web.Scotty.Trans  ( get, put, post, delete, html, param )

import Core              ( BrandyScottyM )
import Api.Users         ( apiGetUsers, apiGetUserByKey, apiAddUser, apiUpdateUser, apiDeleteUser )
import Api.Tags          ( apiGetTags, apiGetTagByKey, apiAddTag )


routes :: BrandyScottyM ()
routes = do

    get    root               $ html "ROOT"

    get    userCollection       apiGetUsers
    get    userElement        $ apiGetUserByKey        =<< key
    put    userElement        $ apiUpdateUser          =<< key
    post   userCollection       apiAddUser
    delete userElement        $ apiDeleteUser          =<< key

    get    tagCollection        apiGetTags
    get    tagElement         $ apiGetTagByKey         =<< key
    post   tagCollection        apiAddTag

  where
    
    root               = "/"
    userCollection     = "/api/users"
    userElement        = "/api/users/:key"
    tagCollection      = "/api/tags"
    tagElement         = "/api/tags/:key"

    key                = param "key"
