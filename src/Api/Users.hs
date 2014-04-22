
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Api.Users
(
  apiGetUsers
, apiGetUserByKey
)
where

import Control.Monad.Trans        ( lift )
import qualified Data.Text as T   ( Text )
import Network.HTTP.Types.Status  ( notFound404 )
import Web.Scotty.Trans           ( json, text, status )

import ApiUtility                 ( parseKey )
import Core                       ( BrandyActionM )
import DataAccess.Users           ( getAllUsers, getUserByKey )


apiGetUsers :: BrandyActionM ()
apiGetUsers = do
    users <- lift getAllUsers
    json users

apiGetUserByKey :: T.Text -> BrandyActionM ()
apiGetUserByKey keyText =
    parseKey keyText $ \key -> do
        maybeUser <- lift $ getUserByKey key
        case maybeUser of
            Just user -> json user
            Nothing   -> status notFound404 >> text "User not found."
