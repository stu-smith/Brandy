
module Core
(
  DatabaseEnvironmentT
, BrandyScottyM
, BrandyActionM
, ApiError(..)
, liftDB, liftWeb
)
where

import Control.Monad.Reader       ( ReaderT )
import Control.Monad.Trans        ( MonadTrans, lift )
import Network.HTTP.Types.Status  ( Status )
import Web.Scotty.Trans           ( ActionT, ScottyT )
import qualified Data.Text as T   ( Text )
import qualified Data.Text.Lazy as TL
                                  ( Text )


type DatabaseEnvironmentT = ReaderT T.Text IO
type BrandyScottyM = ScottyT TL.Text DatabaseEnvironmentT
type BrandyActionM = ActionT TL.Text DatabaseEnvironmentT

data ApiError = ApiError
    { httpStatus :: Status
    , message    :: TL.Text
    }

liftDB :: MonadTrans m => DatabaseEnvironmentT a -> m BrandyActionM a
liftDB =
    lift . lift

liftWeb :: MonadTrans m => BrandyActionM a -> m BrandyActionM a
liftWeb =
    lift
