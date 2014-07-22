
{-# LANGUAGE DeriveGeneric #-}

module Json.Tag
(
  Tag(..)
, tagMapping
)
where

import Data.Aeson      ( ToJSON, FromJSON )
import Data.Text as T  ( Text )
import GHC.Generics    ( Generic )

import Database        ( JsonDataAccessMapping(..) )
import qualified Schema as DB


data Tag = Tag
    { name :: T.Text
    }
  deriving (Show, Generic)

instance ToJSON Tag
instance FromJSON Tag

tagMapping :: JsonDataAccessMapping Tag DB.Tag
tagMapping = JsonDataAccessMapping
    {
        jsonToDataAccess = \(Tag tName) -> DB.Tag
            { DB.tagName = tName
            },
        dataAccessToJson = \(DB.Tag tName) -> Tag
            { name = tName
            }
    }
