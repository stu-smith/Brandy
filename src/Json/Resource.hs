
{-# LANGUAGE DeriveGeneric #-}

module Json.Resource
(
  Resource(..)
)
where

import Data.Aeson                ( ToJSON, FromJSON )
import qualified Data.Text as T  ( Text )
import Data.Time                 ( UTCTime )
import GHC.Generics              ( Generic )


data Resource = Resource
    { path            :: T.Text
    , createdByUserId :: T.Text
    , createdAt       :: Maybe UTCTime
    , public          :: Bool
    , contentType     :: T.Text
    }
  deriving (Show, Generic)

instance ToJSON Resource
instance FromJSON Resource
