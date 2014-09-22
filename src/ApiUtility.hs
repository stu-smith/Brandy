
{-# LANGUAGE OverloadedStrings #-}

module ApiUtility
(
  Validate(..)
, RawResponse(..)
, apiFail, apiError
, runApiGet, runApiPut, runApiPost, runApiDelete, runApiRaw
, liftDB, liftWeb
, apiDbGetSingle, apiDbGetMultiple
, apiDbInsert, apiDbUpdate, apiDbDelete
, authenticatedUserId
, readKey, validateDbExists
)
where

import Control.Applicative         ( (<$>) )
import Control.Monad               ( void )
import Control.Monad.Trans.Either  ( EitherT, runEitherT, left, right )
import Data.Aeson                  ( ToJSON, FromJSON, decode )
import qualified Data.ByteString.Lazy as BSL
                                   ( ByteString )
import qualified Data.Text as T    ( Text )
import qualified Data.Text.Lazy as TL
                                   ( Text, toStrict )
import Database.Persist            ( Key )
import Network.HTTP.Types.Status   ( Status, ok200, created201, noContent204
                                   , badRequest400, unauthorized401, notFound404, conflict409 )
import Web.Scotty.Trans            ( json, text, status, body, params, raw, setHeader )

import Core                        ( ApiError(..), BrandyActionM, DatabaseEnvironmentT, liftWeb, liftDB )  
import Json.WithId                 ( WithId(..), textToId )
import qualified Schema as DB


data RawResponse = RawResponse
    { contentType :: Maybe TL.Text
    , bodyValue   :: BSL.ByteString
    }

class Validate a where
    validate :: Monad m => a -> EitherT ApiError m a

apiFail :: Monad m => Status -> TL.Text -> EitherT ApiError m a
apiFail s m =
    left $ ApiError s m

apiError :: ApiError -> BrandyActionM ()
apiError (ApiError s m) =
    status s >> text m

runApiGet :: ToJSON v => EitherT ApiError BrandyActionM v -> BrandyActionM ()
runApiGet =
    runApi ok200

runApiPut :: ToJSON v => EitherT ApiError BrandyActionM v -> BrandyActionM ()
runApiPut =
    runApi ok200

runApiPost :: ToJSON v => EitherT ApiError BrandyActionM v -> BrandyActionM ()
runApiPost =
    runApi created201

runApiDelete :: EitherT ApiError BrandyActionM () -> BrandyActionM ()
runApiDelete =
    runApiInternal $ const $ status noContent204

runApi :: ToJSON v => Status -> EitherT ApiError BrandyActionM v -> BrandyActionM ()
runApi s =
    runApiInternal (\v -> status s >> json v)

runApiRaw :: Status -> EitherT ApiError BrandyActionM RawResponse -> BrandyActionM ()
runApiRaw s getRawResponse = do
    rr <- runEitherT getRawResponse
    case rr of
        Right (RawResponse ct bv) -> send ct bv
        Left  ae                  -> apiError ae
  where
    send mct bv = do
        status s
        case mct of
            Just ct -> setHeader "Content-Type" ct
            Nothing -> return ()
        raw bv

runApiInternal :: (v -> BrandyActionM ()) -> EitherT ApiError BrandyActionM v -> BrandyActionM ()
runApiInternal respond f = do
    e <- runEitherT f
    case e of
        Right rv -> respond  rv
        Left  ae -> apiError ae

validateBody :: (FromJSON a, Validate a) => EitherT ApiError BrandyActionM a
validateBody =
    decode <$> liftWeb body >>= maybe invalid validate
  where
    invalid = apiFail badRequest400 "Invalid request body."

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
    readKeyInternal right
                    (left $ ApiError notFound404 "Not found.")

readKey_ :: Monad m => T.Text -> (Key a -> EitherT ApiError m ()) -> EitherT ApiError m ()
readKey_ s a =
    void $ readKeyInternal a (return ()) s

readKeyInternal :: (Key d -> r) -> r -> T.Text -> r
readKeyInternal rf lf =
    maybe lf rf . textToId

validateDbExists :: Maybe a -> EitherT ApiError BrandyActionM a
validateDbExists =
    validateDbInternal notFound404 "Not found."

validateDbInsert :: Maybe a -> EitherT ApiError BrandyActionM a
validateDbInsert =
    validateDbInternal conflict409 "Already in use."

validateDbInternal :: Status -> TL.Text -> Maybe a -> EitherT ApiError BrandyActionM a
validateDbInternal s msg =
    maybe (apiFail s msg) return

--
-- This clearly isn't authentication.
-- It's a massive security hole.
-- WIP
--
authenticatedUserId :: EitherT ApiError BrandyActionM (Key DB.User)
authenticatedUserId = do
    ps <- liftWeb params
    let val = lookup "uid" ps
    case val of
        Nothing -> apiFail unauthorized401 "Missing uid parameter."
        Just v  -> readKey $ TL.toStrict v
