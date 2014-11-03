
module Plugins
(
  PluggedIn
, mkPlugin
, withResourceTransform
, withRoutesHandler
, getRoutesHandlers
)
where

import Data.Monoid                   ( Monoid(..) )

import Core                          ( BrandyScottyM )
import Transforms.ResourceTransform  ( ResourceTransform )


data PluggedIn = PluggedIn [ResourceTransform] [BrandyScottyM ()]

instance Monoid PluggedIn where
    mempty                      = PluggedIn [] []
    mappend (PluggedIn rta rha)
            (PluggedIn rtb rhb) = PluggedIn (rta ++ rtb)
                                            (rha ++ rhb)


mkPlugin :: PluggedIn
mkPlugin =
    PluggedIn [] []

withResourceTransform :: ResourceTransform -> PluggedIn
withResourceTransform rt =
    PluggedIn [rt] []

withRoutesHandler :: BrandyScottyM () -> PluggedIn
withRoutesHandler rh =
    PluggedIn [] [rh]

getRoutesHandlers :: PluggedIn -> [BrandyScottyM ()]
getRoutesHandlers (PluggedIn _ rhs) = rhs
