
{-# LANGUAGE OverloadedStrings #-}

module ApiUtility
(
  Validate(..)
, apiFail
, runApi, runApi_
, liftDB, liftWeb
, apiDbGetSingle, apiDbGetMultiple
, apiDbInsert, apiDbUpdate, apiDbDelete
)
where

import Control.Applicative         ( (<$>) )
import Control.Monad               ( void )
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
import Network.HTTP.Types.Status   ( Status, noContent204, badRequest400, notFound404, conflict409 )
import Web.Scotty.Trans            ( json, text, status, body )

import Core                        ( ApiError(..), BrandyActionM, DatabaseEnvironmentT )  
import Json.WithId                 ( WithId(..) )


class Validate a where
    validate :: Monad m => a -> EitherT ApiError m a

apiFail :: Monad m => Status -> TL.Text -> EitherT ApiError m a
apiFail s m =
    left $ ApiError s m

runApi :: ToJSON v => EitherT ApiError BrandyActionM v -> BrandyActionM ()
runApi =
    runApiInternal json

runApi_ :: EitherT ApiError BrandyActionM () -> BrandyActionM ()
runApi_ =
    runApiInternal $ const $ status noContent204

runApiInternal :: ToJSON v => (v -> BrandyActionM ()) -> EitherT ApiError BrandyActionM v -> BrandyActionM ()
runApiInternal respond f = do
    e <- runEitherT f
    case e of
        Right v             -> respond v
        Left (ApiError s m) -> status s >> text m

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
            => T.Text -> (Key d -> a -> DatabaseEnvironmentT (Maybe (WithId a)))
            -> EitherT ApiError BrandyActionM (WithId a)
apiDbUpdate keyText dbUpdate = do
    key       <- readKey keyText
    pre       <- validateBody
    maybePost <- liftDB $ dbUpdate key pre
    validateDbExists maybePost

apiDbDelete :: T.Text -> (Key d -> DatabaseEnvironmentT ()) -> EitherT ApiError BrandyActionM ()
apiDbDelete keyText dbDelete =
    readKey_ keyText $ liftDB . dbDelete

readKey :: Monad m => T.Text -> EitherT ApiError m (Key a)
readKey =
    readKeyInternal (right . mkKey)
                    (left $ ApiError notFound404 "Not found.")

readKey_ :: Monad m => T.Text -> (Key a -> EitherT ApiError m ()) -> EitherT ApiError m ()
readKey_ s a =
    void $ readKeyInternal (a . mkKey) (return ()) s

readKeyInternal :: (Int64 -> r) -> r -> T.Text -> r
readKeyInternal rf lf s =
    case decimal s of
        Right (v, "") -> rf v
        _             -> lf

mkKey :: Int64 -> Key a
mkKey =
    Key . toPersistValue

validateDbExists :: Maybe a -> EitherT ApiError BrandyActionM a
validateDbExists =
    validateDbInternal notFound404 "Not found."

validateDbInsert :: Maybe a -> EitherT ApiError BrandyActionM a
validateDbInsert =
    validateDbInternal conflict409 "Already in use."

validateDbInternal :: Status -> TL.Text -> Maybe a -> EitherT ApiError BrandyActionM a
validateDbInternal s msg db =
    case db of
        Just v  -> return v
        Nothing -> apiFail s msg
