
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Json.PrivateUserPre
(
  PrivateUserPre(..)
)
where

import Control.Monad.Trans.Either  ( right )
import Data.Aeson                  ( ToJSON, FromJSON )
import Data.Text as T              ( Text, null )
import GHC.Generics                ( Generic )
import Network.HTTP.Types.Status   ( badRequest400 )

import ApiUtility                  ( Validate(..), apiFail )


data PrivateUserPre = PrivateUserPre
    { displayName :: T.Text
    , email       :: T.Text
    }
  deriving (Show, Eq, Generic)

instance ToJSON PrivateUserPre
instance FromJSON PrivateUserPre

instance Validate PrivateUserPre where
    validate userPre
        | T.null . displayName $ userPre = apiFail badRequest400 "Missing displayName."
        | T.null . email       $ userPre = apiFail badRequest400 "Missing email."
        | otherwise                      = right userPre
