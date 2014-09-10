
module DataAccess.ResourceContent
(
  dbGetResourceContent
, dbUpdateResourceContent
)
where

import qualified Data.ByteString as BS
                           ( ByteString )
import qualified Data.Text as T
                           ( Text )
import Database.Esqueleto  ( Value(..), select, updateCount, from, where_, set, val, (^.), (==.), (=.) )

import Core                ( DatabaseEnvironmentT )
import Database            ( runSql )
import Database.Persist    ( Key )
import qualified Schema as DB


dbGetResourceContent :: Key DB.Resource -> DatabaseEnvironmentT (Maybe (T.Text, BS.ByteString))
dbGetResourceContent key =
    runSql $ do
        resource <- select $ from $ \r -> do
                    where_ (r ^. DB.ResourceId ==. val key)
                    return (r ^. DB.ResourceContentType, r ^. DB.ResourceData)
        case resource of
            []                       -> return Nothing
            ((Value ct, Value d):[]) -> return $ Just (ct, d)
            _                        -> undefined 

dbUpdateResourceContent :: Key DB.Resource -> BS.ByteString -> DatabaseEnvironmentT Bool
dbUpdateResourceContent key d = do
    n <- runSql $ updateCount $ \r -> do
             set r [DB.ResourceData =. val d]
             where_ (r ^. DB.ResourceId ==. val key)
    return (n == 1)
