
{-# LANGUAGE OverloadedStrings #-}

module Api.Resources
(
    apiGetResources
)
where

import Data.Aeson as J            ( Value, object, (.=) )
import Data.Text                  ( Text )
import Control.Monad.IO.Class     ( liftIO )
import Web.Scotty                 ( ActionM, json )
import Database.Esqueleto as Sql  ( select, from, (^.), Value(..) )

import Schema
import Database                   ( runSql )


apiGetResources :: ActionM ()
apiGetResources
    = json =<< liftIO sqlGetAllResourcesSummary

sqlGetAllResourcesSummary :: IO [J.Value]
sqlGetAllResourcesSummary
    = runSql $ do rows <- select $ from $ \resource ->
                          return $ resource ^. ResourcePath
                  return $ map asJSON rows

asJSON :: Sql.Value Text -> J.Value
asJSON (Sql.Value path)
    = object ["path" .= path]
