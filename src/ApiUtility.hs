
{-# LANGUAGE OverloadedStrings #-}

module ApiUtility
(
  readKey
, showKey
, apiFail
, runApi
)
where

import Control.Monad.Trans.Either  ( EitherT, runEitherT, left )
import Data.Aeson                  ( ToJSON )
import Data.Int                    ( Int64 )
import qualified Data.Text as T
                                   ( Text )
import qualified Data.Text.Lazy as TL
                                   ( Text )
import Database.Persist            ( Key, KeyBackend(Key), toPersistValue )
import Data.Text.Read              ( decimal )
import Network.HTTP.Types.Status   ( Status )
import Web.PathPieces              ( toPathPiece )
import Web.Scotty.Trans            ( json, text, status )

import Core                        ( ApiError(..), BrandyActionM )  


readKey :: T.Text -> Maybe (Key a)
readKey s =
    case decimal s of
        Right (v, "") -> Just $ mkKey v
        _             -> Nothing

showKey :: Key a -> T.Text
showKey (Key v) =
    toPathPiece v

mkKey :: Int64 -> Key a
mkKey =
    Key . toPersistValue

apiFail :: Monad m => Status -> TL.Text -> EitherT ApiError m a
apiFail s m
    = left $ ApiError s m

runApi :: ToJSON v => EitherT ApiError BrandyActionM v -> BrandyActionM ()
runApi f = do
    e <- runEitherT f
    case e of
        Left (ApiError s m) -> status s >> text m
        Right v             -> json v
