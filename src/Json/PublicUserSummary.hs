
{-# LANGUAGE DeriveGeneric #-}

module Json.PublicUserSummary
(
  PublicUserSummary(..)
)
where

import Data.Aeson              ( ToJSON, FromJSON )
import Data.Text as T  ( Text )
import GHC.Generics            ( Generic )


data PublicUserSummary = PublicUserSummary
    { id          :: T.Text
    , displayName :: T.Text
    }
  deriving (Show, Generic)

instance ToJSON PublicUserSummary
instance FromJSON PublicUserSummary
