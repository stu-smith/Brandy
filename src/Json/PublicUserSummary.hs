
{-# LANGUAGE DeriveGeneric #-}

module Json.PublicUserSummary
(
  PublicUserSummary(..)
)
where

import Data.Aeson      ( ToJSON, FromJSON )
import Data.Text as T  ( Text )
import GHC.Generics    ( Generic )


data PublicUserSummary = PublicUserSummary
    { displayName :: T.Text
    }
  deriving (Generic)

instance ToJSON PublicUserSummary
instance FromJSON PublicUserSummary
