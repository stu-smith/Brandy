
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Json.Tag
(
  Tag(..)
, tagMapping
)
where

import Control.Applicative         ( (<$>) )
import Control.Monad.Trans.Either  ( right )
import Data.Aeson                  ( ToJSON, FromJSON )
import Data.Text as T              ( Text, null )
import GHC.Generics                ( Generic )
import Network.HTTP.Types.Status   ( badRequest400 )

import ApiUtility                  ( Validate(..), apiFail )
import Database                    ( JsonDataAccessMapping(..) )
import qualified Schema as DB


data Tag = Tag
    { name :: T.Text
    }
  deriving (Show, Generic)

instance ToJSON Tag
instance FromJSON Tag

instance Validate Tag where
    validate tag
        | T.null . name $ tag = wrong "Missing name."
        | otherwise           = right tag
      where
        wrong = apiFail badRequest400

tagMapping :: JsonDataAccessMapping Tag DB.Tag
tagMapping = JsonDataAccessMapping
    (DB.Tag <$>       name)
    (Tag    <$> DB.tagName)
