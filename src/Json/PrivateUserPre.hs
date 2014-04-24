
{-# LANGUAGE DeriveGeneric #-}

module Json.PrivateUserPre
(
  PrivateUserPre(..)
)
where

import Data.Aeson      ( ToJSON, FromJSON )
import Data.Text as T  ( Text )
import GHC.Generics    ( Generic )


data PrivateUserPre = PrivateUserPre
    { displayName :: T.Text
    , email       :: T.Text
    }
  deriving (Show, Generic)

instance ToJSON PrivateUserPre
instance FromJSON PrivateUserPre
