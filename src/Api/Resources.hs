
{-# LANGUAGE OverloadedStrings #-}

module Api.Resources
(
  apiGetResources
, apiGetNamedResource
)
where

import Data.Aeson as J            ( Value, object, (.=) )
import Data.Text                  ( Text )
import Data.Text.Lazy             ( fromStrict )
import Control.Monad.IO.Class     ( liftIO )
import Web.Scotty                 ( ActionM, json, text )
import Database.Esqueleto as Sql  ( select, from, (^.), Value(..) )

import Schema
import Database                   ( runSql )


apiGetResources :: ActionM ()
apiGetResources
    = json =<< liftIO sqlGetAllResourcesSummary

apiGetNamedResource :: Text -> ActionM ()
apiGetNamedResource name
    = text $ fromStrict name

sqlGetAllResourcesSummary :: IO [J.Value]
sqlGetAllResourcesSummary
    = runSql $ do rows <- select $ from $ \resource ->
                          return $ resource ^. ResourcePath
                  return $ map asJSON rows

asJSON :: Sql.Value Text -> J.Value
asJSON (Sql.Value path)
    = object ["path" .= path]
