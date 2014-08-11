
{-# LANGUAGE OverloadedStrings #-}

module Json.WithId
(
  WithId(..)
, getId
, addId
, idToText
, textToId
)
where

import Control.Monad        ( mzero )
import Data.Aeson           ( ToJSON(..), FromJSON(..), Value(..), (.:) )
import Data.Aeson.Types     ( Parser )
import Data.HashMap.Strict  ( insert )
import Data.Int             ( Int64 )
import qualified Data.Text as T
                            ( Text )
import Data.Text.Read       ( decimal )
import Database.Persist     ( Key, KeyBackend(Key), unKey, toPersistValue )
import Web.PathPieces       ( toPathPiece )


data WithId a = WithId
    { id     :: T.Text
    , value :: a
    }

getId :: WithId a -> T.Text
getId (WithId i _) = i

addId :: Key d -> v -> WithId v
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

instance (ToJSON a) => ToJSON (WithId a) where
    toJSON (WithId idv av) =
        Object . insert "id" (String idv) . unObject . toJSON $ av
      where
        unObject x = case x of
            Object v -> v
            _        -> undefined

instance (FromJSON a) => FromJSON (WithId a) where
    parseJSON vo = case vo of
        Object v -> fromJson v
        _        -> mzero
      where
        fromJson v = do
            av  <- parseJSON vo :: FromJSON a => Parser a
            idv <- v .: "id"
            return $ WithId idv av
