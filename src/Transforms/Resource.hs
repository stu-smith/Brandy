
{-# LANGUAGE OverloadedStrings #-}

module Transforms.Resource
(
  handleResource
)
where

import Control.Monad.Trans   ( lift )
import qualified Data.Text as T
                             ( Text )
import Web.Scotty.Trans      ( next )

import Core                  ( BrandyActionM )
import DataAccess.Resources  ( dbGetResourceByPath )


handleResource :: T.Text -> BrandyActionM ()
handleResource path = do
    maybeResource <- lift $ dbGetResourceByPath path
    case maybeResource of
        Nothing       -> next
        Just _ -> return ()
