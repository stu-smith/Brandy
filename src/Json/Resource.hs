
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Json.Resource
(
  Resource(..)
)
where

import Control.Monad.Trans.Either  ( right )
import Data.Aeson                  ( ToJSON, FromJSON )
import Data.Maybe                  ( isJust )
import qualified Data.Text as T    ( Text, null )
import Data.Time                   ( UTCTime )
import GHC.Generics                ( Generic )
import Network.HTTP.Types.Status   ( badRequest400 )

import ApiUtility                  ( Validate(..), apiFail )


data Resource = Resource
    { path            :: T.Text
    , createdByUserId :: Maybe T.Text
    , createdAt       :: Maybe UTCTime
    , public          :: Bool
    , contentType     :: T.Text
    }
  deriving (Generic)

instance ToJSON Resource
instance FromJSON Resource

instance Validate Resource where
    validate resource
        | T.null . path            $ resource = wrong "Missing path."
        | isJust . createdByUserId $ resource = wrong "Cannot supply createdByUserId."
        | isJust . createdAt       $ resource = wrong "Cannot supply createdAt."
        | otherwise                           = right resource
      where
        wrong = apiFail badRequest400
