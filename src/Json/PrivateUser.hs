
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Json.PrivateUser
(
  PrivateUser(..)
, privateUserMapping
)
where

import Control.Monad.Trans.Either  ( right )
import Data.Aeson                  ( ToJSON, FromJSON )
import Data.Text as T              ( Text, null )
import GHC.Generics                ( Generic )
import Network.HTTP.Types.Status   ( badRequest400 )

import ApiUtility                  ( Validate(..), apiFail )
import Database                    ( JsonDataAccessMapping(..) )
import qualified Schema as DB


data PrivateUser = PrivateUser
    { displayName :: T.Text
    , email       :: T.Text
    }
  deriving (Generic)

instance ToJSON PrivateUser
instance FromJSON PrivateUser

instance Validate PrivateUser where
    validate user
        | T.null . displayName $ user = wrong "Missing displayName."
        | T.null . email       $ user = wrong "Missing email."
        | otherwise                   = right user
      where
        wrong = apiFail badRequest400

privateUserMapping :: JsonDataAccessMapping PrivateUser DB.User
privateUserMapping = JsonDataAccessMapping
    (DB.User     <$>        displayName <*>        email)
    (PrivateUser <$> DB.userDisplayName <*> DB.userEmail)
