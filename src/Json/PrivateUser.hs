{-# LANGUAGE DeriveGeneric #-}

module Json.PrivateUser
(
  PrivateUser(..)
)
where

import Data.Aeson              ( ToJSON, FromJSON )
import Data.Text as T  ( Text )
import GHC.Generics            ( Generic )


data PrivateUser = PrivateUser
    { id          :: T.Text
    , displayName :: T.Text
    , email       :: T.Text
    }
  deriving (Show, Generic)

instance ToJSON PrivateUser
instance FromJSON PrivateUser
