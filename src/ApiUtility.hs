
{-# LANGUAGE OverloadedStrings #-}

module ApiUtility
(
  readKey
, showKey
)
where

import Data.Int           ( Int64 )
import Data.Text          ( Text )
import Database.Persist   ( Key, KeyBackend(Key), toPersistValue )
import Data.Text.Read     ( decimal )
import Web.PathPieces     ( toPathPiece )


readKey :: Text -> Maybe (Key a)
readKey s =
    case decimal s of
        Right (v, "") -> Just $ mkKey v
        _             -> Nothing

showKey :: Key a -> Text
showKey (Key v) =
    toPathPiece v

mkKey :: Int64 -> Key a
mkKey =
    Key . toPersistValue
