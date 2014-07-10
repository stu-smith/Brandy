
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

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
import Schema


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

privateUserMapping :: JsonDataAccessMapping PrivateUser User
privateUserMapping = JsonDataAccessMapping
    {
        jsonToDataAccess = \(PrivateUser uDisplayName uEmail) -> User
            { userDisplayName = uDisplayName
            , userEmail       = uEmail
            },
        dataAccessToJson = \(User uDisplayName uEmail) -> PrivateUser
            { displayName = uDisplayName
            , email = uEmail
            }
    }
