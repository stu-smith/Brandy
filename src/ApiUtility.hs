
{-# LANGUAGE OverloadedStrings #-}

module ApiUtility
(
  Validate(..)
, readKeyOld
, readKey
, showKey
, apiFail
, runApi
, liftDB
, liftWeb
, validateDbGet
, validateDbInsert
, validateBody
, apiDbGetSingle
, apiDbGetMultiple
, apiDbInsert
)
where

import Control.Applicative         ( (<$>) )
import Control.Monad.Trans         ( lift )
import Control.Monad.Trans.Either  ( EitherT, runEitherT, left, right )
import Data.Aeson                  ( ToJSON, FromJSON, decode )
import Data.Int                    ( Int64 )
import qualified Data.Text as T
                                   ( Text )
import qualified Data.Text.Lazy as TL
                                   ( Text )
import Database.Persist            ( Key, KeyBackend(Key), toPersistValue )
import Data.Text.Read              ( decimal )
import Network.HTTP.Types.Status   ( Status, badRequest400, notFound404, conflict409 )
import Web.PathPieces              ( toPathPiece )
import Web.Scotty.Trans            ( json, text, status, body )

import Core                        ( ApiError(..), BrandyActionM, DatabaseEnvironmentT )  


class Validate a where
    validate :: Monad m => a -> EitherT ApiError m a


readKeyOld :: T.Text -> Maybe (Key a)
readKeyOld s =
    case decimal s of
        Right (v, "") -> Just $ mkKey v
        _             -> Nothing

readKey :: Monad m => T.Text -> EitherT ApiError m (Key a)
readKey s =
    case decimal s of
        Right (v, "") -> right $ mkKey v
        _             -> left $ ApiError badRequest400 "Invalid key."

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

liftDB :: DatabaseEnvironmentT a -> EitherT ApiError BrandyActionM a
liftDB = lift . lift

liftWeb :: BrandyActionM a -> EitherT ApiError BrandyActionM a
liftWeb = lift

validateDbGet :: Maybe a -> EitherT ApiError BrandyActionM a
validateDbGet Nothing  = apiFail notFound404 "Not found."
validateDbGet (Just u) = return u

validateDbInsert :: Maybe a -> EitherT ApiError BrandyActionM a
validateDbInsert Nothing     = apiFail conflict409 "Already in use."
validateDbInsert (Just k)    = return k

validateBody :: (FromJSON a, Validate a) => EitherT ApiError BrandyActionM a
validateBody = do
    maybeValue <- decode <$> liftWeb body
    case maybeValue of
        Nothing -> apiFail badRequest400 "Invalid request body."
        Just v  -> validate v

apiDbGetSingle :: T.Text -> (Key d -> DatabaseEnvironmentT (Maybe a)) -> EitherT ApiError BrandyActionM a
apiDbGetSingle keyText dbGet = do
    key        <- readKey keyText
    maybeValue <- liftDB $ dbGet key
    validateDbGet maybeValue

apiDbGetMultiple :: DatabaseEnvironmentT [a] -> EitherT ApiError BrandyActionM [a]
apiDbGetMultiple = liftDB

apiDbInsert :: (FromJSON p, Validate p)
            => (p -> DatabaseEnvironmentT (Maybe (Key d))) -> (p -> Key d -> a) -> EitherT ApiError BrandyActionM a
apiDbInsert dbInsert convert = do
    valuePre <- validateBody
    maybeKey <- liftDB . dbInsert $ valuePre
    key      <- validateDbInsert maybeKey
    return $ convert valuePre key
