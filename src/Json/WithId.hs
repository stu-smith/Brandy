
{-# LANGUAGE OverloadedStrings #-}

module Json.WithId
(
  WithId(..)
, withId
, mkId
)
where

import Control.Monad        ( mzero )
import Data.Aeson           ( ToJSON(..), FromJSON(..), Value(..), (.:) )
import Data.Aeson.Types     ( Parser )
import Data.HashMap.Strict  ( insert )
import qualified Data.Text as T
                            ( Text, pack )


data WithId a = WithId
    { id    :: T.Text
    , value :: a
    }
  deriving (Show, Eq)

withId :: Show k => k -> v -> WithId v
withId k =
    WithId (mkId k)

instance (ToJSON a) => ToJSON (WithId a) where
    toJSON (WithId idv av) =
        Object . insert "id" (String idv) . unObject . toJSON $ av
      where unObject x = case x of
                Object v -> v
                _        -> undefined

instance (FromJSON a) => FromJSON (WithId a) where
    parseJSON vo = case vo of
        Object v -> do
            av  <- parseJSON vo :: FromJSON a => Parser a
            idv <- v .: "id"
            return $ WithId idv av
        _ -> mzero

mkId :: Show a => a -> T.Text
mkId =
    T.pack . show
