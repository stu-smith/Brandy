
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Json.PrivateUser
(
  PrivateUser(..)
)
where

import Control.Monad.Trans.Either  ( right )
import Data.Aeson                  ( ToJSON, FromJSON )
import Data.Text as T              ( Text, null )
import GHC.Generics                ( Generic )
import Network.HTTP.Types.Status   ( badRequest400 )

import ApiUtility                  ( Validate(..), apiFail )


data PrivateUser = PrivateUser
    { displayName :: T.Text
    , email       :: T.Text
    }
  deriving (Show, Eq, Generic)

instance ToJSON PrivateUser
instance FromJSON PrivateUser

instance Validate PrivateUser where
    validate userPre
        | T.null . displayName $ userPre = apiFail badRequest400 "Missing displayName."
        | T.null . email       $ userPre = apiFail badRequest400 "Missing email."
        | otherwise                      = right userPre
