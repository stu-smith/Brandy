
module Core
(
  DatabaseEnvironmentT
, BrandyScottyM
, BrandyActionM
)
where

import Control.Monad.Reader  ( ReaderT )
import Web.Scotty.Trans      ( ActionT, ScottyT )
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL


type DatabaseEnvironmentT = ReaderT T.Text
type BrandyScottyM = ScottyT TL.Text (DatabaseEnvironmentT IO)
type BrandyActionM = ActionT TL.Text (DatabaseEnvironmentT IO)
