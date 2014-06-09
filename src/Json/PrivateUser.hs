
{-# LANGUAGE DeriveGeneric #-}

module Json.PrivateUser
(
  PrivateUser(..)
)
where

import Data.Aeson    ( ToJSON, FromJSON )
import qualified Data.Text as T
                     ( Text )
import GHC.Generics  ( Generic )


data PrivateUser = PrivateUser
    { id          :: T.Text
    , displayName :: T.Text
    , email       :: T.Text
    }
  deriving (Show, Eq, Generic)

instance ToJSON PrivateUser
instance FromJSON PrivateUser
