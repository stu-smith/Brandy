
{-# LANGUAGE OverloadedStrings #-}

module Json.WithId
(
  WithId
, getId, getKey
, addId
, idToText, textToId
)
where

import Control.Monad        ( mzero )
import Data.Aeson           ( ToJSON(..), FromJSON(..), Value(..), (.:) )
import Data.Aeson.Types     ( Parser )
import Data.HashMap.Strict  ( insert )
import Data.Int             ( Int64 )
import Data.Maybe           ( fromJust )
import qualified Data.Text as T
                            ( Text )
import Data.Text.Read       ( decimal )
import Database.Persist     ( Key, KeyBackend(Key), unKey, toPersistValue )
import Web.PathPieces       ( toPathPiece )


data (ToJSON j, FromJSON j) => WithId j = WithId T.Text j

getId :: (ToJSON j, FromJSON j) => WithId j -> T.Text
getId (WithId i _) = i

getKey :: (ToJSON j, FromJSON j) => WithId j -> Key d
getKey (WithId i _) = fromJust $ textToId i

addId :: (ToJSON j, FromJSON j) => Key d -> j -> WithId j
addId =
    WithId . idToText

idToText :: Key d -> T.Text
idToText =
    toPathPiece . unKey

textToId :: T.Text -> Maybe (Key d)
textToId s =
    case decimal s of
        Right (v, "") -> Just $ mkKey v
        _             -> Nothing

mkKey :: Int64 -> Key d
mkKey =
    Key . toPersistValue

instance (ToJSON j, FromJSON j) => ToJSON (WithId j) where
    toJSON (WithId idv av) =
        Object . insert "id" (String idv) . unObject . toJSON $ av
      where
        unObject x = case x of
            Object v -> v
            _        -> undefined

instance (ToJSON j, FromJSON j) => FromJSON (WithId j) where
    parseJSON vo = case vo of
        Object v -> fromJson v
        _        -> mzero
      where
        fromJson v = do
            av  <- parseJSON vo :: FromJSON a => Parser a
            idv <- v .: "id"
            return $ WithId idv av
