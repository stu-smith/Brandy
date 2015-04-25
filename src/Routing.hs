
{-# LANGUAGE OverloadedStrings #-}

module Routing
(
  routes
)
where

import qualified Data.Text as T   ( append, intercalate )
import qualified Data.Text.Lazy as TL
                                  ( fromStrict )
import Network.HTTP.Types.Status  ( methodNotAllowed405 )
import Network.Wai                ( pathInfo )
import Web.Scotty.Trans           ( get, put, post, delete, param, function )

import Core                       ( BrandyScottyM, ApiError(..) )
import Api.Users                  ( apiGetUsers, apiGetUser, apiAddUser, apiUpdateUser, apiDeleteUser )
import Api.Resources              ( apiGetResources, apiGetResource
                                  , apiAddResource, apiUpdateResource, apiDeleteResource )
import Api.ResourceContent        ( apiGetResourceContent, apiUpdateResourceContent )
import Api.Tags                   ( apiGetTags, apiGetTag, apiAddTag, apiUpdateTag, apiDeleteTag )
import ApiUtility                 ( apiError )
import Plugins                    ( PluggedIn, getRoutesHandlers )
import Transforms.Resource        ( handleResource )


routes :: PluggedIn -> BrandyScottyM ()
routes plugin = do
    apiRoutes
    resourceRoutes
    pluginRoutes plugin


apiRoutes :: BrandyScottyM ()
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


resourceRoutes :: BrandyScottyM ()
resourceRoutes = do
    get (function getEntire) $ handleResource =<< entire
  where
    entireKey     = "__entire"
    entire        = param entireKey
    getEntire req = Just [(entireKey, TL.fromStrict ("/" `T.append` T.intercalate "/" (pathInfo req)))]


pluginRoutes :: PluggedIn -> BrandyScottyM ()
pluginRoutes plugin = do
    sequence_ routeHandlers
  where
    routeHandlers = getRoutesHandlers plugin

--up :: ScottyT TL.Text IO () -> ScottyT TL.Text (ReaderT T.Text IO) ()
