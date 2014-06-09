
{-# LANGUAGE OverloadedStrings #-}

module Json.WithId
(
    WithId(..)
)
where

import Control.Monad        ( mzero )
import Data.Aeson           ( ToJSON(..), FromJSON(..), Value(..), (.:) )
import Data.Aeson.Types     ( Parser )
import Data.HashMap.Strict  ( insert )
import qualified Data.Text as T
                            ( Text )


data WithId a = WithId
    {
      id    :: T.Text
    , value :: a
    }
  deriving (Show, Eq)

instance (ToJSON a) => ToJSON (WithId a) where
    toJSON (WithId idv av) =
        Object . insert "id" (String idv) . unObject . toJSON $ av
      where unObject (Object v) = v
            unObject _          = undefined

instance (FromJSON a) => FromJSON (WithId a) where
    parseJSON vo@(Object v) = do
        av  <- parseJSON vo :: FromJSON a => Parser a
        idv <- v .: "id"
        return $ WithId idv av
    parseJSON _ = mzero
