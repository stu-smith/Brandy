
{-# LANGUAGE OverloadedStrings #-}

module Api.Resources
(
  apiGetResources
, apiGetNamedResource
, apiInsertNamedResource
, apiDeleteNamedResource
)
where

import Data.Aeson as J            ( Value, object, (.=) )
import Data.Text                  ( Text )
import Data.Text.Lazy             ( fromStrict )
import Control.Monad.IO.Class     ( liftIO )
import Web.Scotty                 ( ActionM, json, text )
import Database.Esqueleto as Sql  ( Value(..), select, from, (^.) )
import Database.Persist           ( Key, insert )

import Schema
import Database                   ( runSql )


apiGetResources :: ActionM ()
apiGetResources
    = json =<< liftIO sqlGetAllResourcesSummary

apiGetNamedResource :: Text -> ActionM ()
apiGetNamedResource name
    = text $ fromStrict name

apiInsertNamedResource :: Text -> ActionM ()
apiInsertNamedResource path
    = do let res = Resource path
    	 _ <- liftIO $ sqlInsertResource res
         text "INSERT"

apiDeleteNamedResource :: Text -> ActionM ()
apiDeleteNamedResource _
    = text "INSERT"

sqlGetAllResourcesSummary :: IO [J.Value]
sqlGetAllResourcesSummary
    = runSql $ do rows <- select $ from $ \resource ->
                          return $ resource ^. ResourcePath
                  return $ map asJSON rows

asJSON :: Sql.Value Text -> J.Value
asJSON (Sql.Value path)
    = object ["path" .= path]

sqlInsertResource :: Resource -> IO (Key Resource)
sqlInsertResource res
    = runSql $ insert res
