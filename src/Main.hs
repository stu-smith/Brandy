
module Main
( main
)
where

import Control.Applicative  ( (<$>) )
import System.Environment   ( getEnv )
import Web.Scotty           ( scotty )

import Routing              ( routes )


main :: IO ()
main
    = do port <- read <$> getEnv "PORT"
         scotty port routes
