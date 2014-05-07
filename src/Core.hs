
module Core
(
  DatabaseEnvironmentT
, BrandyScottyM
, BrandyActionM
, ApiError(..)
)
where

import Control.Monad.Reader       ( ReaderT )
import Network.HTTP.Types.Status  ( Status )
import Web.Scotty.Trans           ( ActionT, ScottyT )
import qualified Data.Text as T   ( Text )
import qualified Data.Text.Lazy as TL
                                  ( Text )


type DatabaseEnvironmentT = ReaderT T.Text
type BrandyScottyM = ScottyT TL.Text (DatabaseEnvironmentT IO)
type BrandyActionM = ActionT TL.Text (DatabaseEnvironmentT IO)

data ApiError = ApiError
    { httpStatus :: Status
    , message    :: TL.Text
    }
