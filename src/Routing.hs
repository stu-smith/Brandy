
{-# LANGUAGE OverloadedStrings #-}

module Routing
(
  routes
)
where

import qualified Data.Text as T   ( append, intercalate )
import qualified Data.Text.Lazy as TL
                                  ( Text, fromStrict )
import Network.HTTP.Types.Status  ( methodNotAllowed405 )
import Network.Wai                ( pathInfo )
import Web.Scotty.Trans           ( ScottyT, get, put, post, delete, param, function )

import Core                       ( BrandyScottyM, DatabaseEnvironmentT, ApiError(..) )
import Api.Users                  ( apiGetUsers, apiGetUser, apiAddUser, apiUpdateUser, apiDeleteUser )
import Api.Resources              ( apiGetResources, apiGetResource
                                  , apiAddResource, apiUpdateResource, apiDeleteResource )
import Api.ResourceContent        ( apiGetResourceContent, apiUpdateResourceContent )
import Api.Tags                   ( apiGetTags, apiGetTag, apiAddTag, apiUpdateTag, apiDeleteTag )
import ApiUtility                 ( apiError )
import Transforms.Resource        ( handleResource )


routes :: BrandyScottyM ()
routes = do
    apiRoutes
    resourceRoutes


apiRoutes :: ScottyT TL.Text DatabaseEnvironmentT ()
apiRoutes = do

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

    apiNotAllowed          = apiError $ ApiError methodNotAllowed405 "Not allowed."

    userCollection         = "/api/users"
    userElement            = "/api/users/:key"
    resourceCollection     = "/api/resources"
    resourceElement        = "/api/resources/:key"
    resourceContentElement = "/api/resources/:key/content"
    tagCollection          = "/api/tags"
    tagElement             = "/api/tags/:key"

    key                    = param "key"


resourceRoutes :: ScottyT TL.Text DatabaseEnvironmentT ()
resourceRoutes =
    get (function getEntire) $ handleResource =<< entire
  where
    entireKey     = "__entire"
    entire        = param entireKey
    getEntire req = Just [(entireKey, TL.fromStrict ("/" `T.append` T.intercalate "/" (pathInfo req)))]
