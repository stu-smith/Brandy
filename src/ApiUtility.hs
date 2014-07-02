
{-# LANGUAGE OverloadedStrings #-}

module ApiUtility
(
  Validate(..)
, readKeyOld
, apiFail
, runApi
, liftDB
, liftWeb
, apiDbGetSingle
, apiDbGetMultiple
, apiDbInsert
, apiDbUpdate
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
import Web.Scotty.Trans            ( json, text, status, body )

import Core                        ( ApiError(..), BrandyActionM, DatabaseEnvironmentT )  
import Json.WithId                 ( WithId(..) )

class Validate a where
    validate :: Monad m => a -> EitherT ApiError m a


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
    validateDbExists maybeValue

apiDbGetMultiple :: DatabaseEnvironmentT [a] -> EitherT ApiError BrandyActionM [a]
apiDbGetMultiple = liftDB

apiDbInsert :: (FromJSON a, Validate a)
            => (a -> DatabaseEnvironmentT (Maybe (WithId a))) -> EitherT ApiError BrandyActionM (WithId a)
apiDbInsert dbInsert = do
    pre       <- validateBody
    maybePost <- liftDB . dbInsert $ pre
    validateDbInsert maybePost

apiDbUpdate :: (FromJSON a, Validate a)
            => T.Text -> (Key d -> a -> DatabaseEnvironmentT (Maybe (WithId a))) -> EitherT ApiError BrandyActionM (WithId a)
apiDbUpdate keyText dbUpdate = do
    key       <- readKey keyText
    pre       <- validateBody
    maybePost <- liftDB $ dbUpdate key pre
    validateDbExists maybePost


readKeyOld :: T.Text -> Maybe (Key a)
readKeyOld s =
    case decimal s of
        Right (v, "") -> Just $ mkKey v
        _             -> Nothing

readKey :: Monad m => T.Text -> EitherT ApiError m (Key a)
readKey s =
    case decimal s of
        Right (v, "") -> right $ mkKey v
        _             -> left $ ApiError notFound404 "Not found."

mkKey :: Int64 -> Key a
mkKey =
    Key . toPersistValue

validateDbExists :: Maybe a -> EitherT ApiError BrandyActionM a
validateDbExists Nothing  = apiFail notFound404 "Not found."
validateDbExists (Just u) = return u

validateDbInsert :: Maybe a -> EitherT ApiError BrandyActionM a
validateDbInsert Nothing  = apiFail conflict409 "Already in use."
validateDbInsert (Just k) = return k
