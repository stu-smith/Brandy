
module Core
(
  BrandyScottyM
, BrandyActionM
)
where

import Control.Monad.Reader  ( ReaderT )
import Web.Scotty.Trans      ( ActionT, ScottyT )
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL


type BrandyScottyM = ScottyT TL.Text (ReaderT T.Text IO)
type BrandyActionM = ActionT TL.Text (ReaderT T.Text IO)
