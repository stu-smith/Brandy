
module Routing
( routes
)
where

import Web.Scotty  ( ScottyM, get, html )

routes :: ScottyM ()
routes
    = get "/" $ html "ROOT"
