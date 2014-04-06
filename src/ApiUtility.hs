
{-# LANGUAGE OverloadedStrings #-}

module ApiUtility
(
  parseKey
)
where

import Data.Int                   ( Int64 )
import Web.Scotty                 ( ActionM, text, status )
import Data.Text                  ( Text )
import Database.Persist           ( Key, KeyBackend(Key), toPersistValue )
import Network.HTTP.Types.Status  ( badRequest400 )
import Data.Text.Read             ( decimal )

parseKey :: Text -> (Key a -> ActionM ()) -> ActionM ()
parseKey key func =
  case parseInt64 key of
      Just k -> func k
      _      -> status badRequest400 >> text "Invalid ID."

parseInt64 :: Text -> Maybe (Key a)
parseInt64 s =
  case decimal s of
      Right (v, "") -> Just $ mkKey v
      _             -> Nothing

mkKey :: Int64 -> Key a
mkKey = Key . toPersistValue
