
module DataAccess.Resources
(
  getAllResources
, getResourceByKey
, insertResource
)
where

import qualified Data.Text as T
                           ( Text )
import Data.Time           ( UTCTime )
import Database.Esqueleto  ( Esqueleto, Value(..), val, select, from, where_, (^.), (==.) )
import Database.Persist    ( Key, Entity )

import Core                ( DatabaseEnvironmentT )
import Database            ( runSql )
import Json.Resource       ( Resource(..) )
import Json.WithId         ( WithId(..), addId, idToText )
import qualified Schema as DB


getAllResources :: DatabaseEnvironmentT [WithId Resource]
getAllResources =
    runSql $ do
        resources <- select $ from $ \r ->
                     return $ columnsWithoutContent r
        return $ map dToJ resources

getResourceByKey :: Key DB.Resource -> DatabaseEnvironmentT (Maybe (WithId Resource))
getResourceByKey key =
    runSql $ do
        resource <- select $ from $ \r -> do
                    where_ (r ^. DB.ResourceId ==. val key)
                    return $ columnsWithoutContent r
        case resource of
            []     -> return Nothing
            (r:[]) -> return $ Just $ dToJ r
            _      -> undefined 

insertResource :: Resource -> DatabaseEnvironmentT (Maybe (WithId (Resource)))
insertResource =
    undefined

dToJ :: ( Value (Key DB.Resource)
        , Value T.Text
        , Value (Key DB.User)
        , Value UTCTime
        , Value Bool
        , Value T.Text
        ) -> WithId Resource
dToJ ( Value rId
     , Value rPath
     , Value rCreatedBy
     , Value rCreatedAt
     , Value rPublic
     , Value rContentType
     ) =
    addId rId $ Resource rPath (Just $ idToText rCreatedBy) (Just rCreatedAt) rPublic rContentType

columnsWithoutContent :: Esqueleto query expr backend
                      => expr (Entity DB.Resource)
                      -> ( expr (Value (Key DB.Resource))
                         , expr (Value T.Text)
                         , expr (Value (Key DB.User))
                         , expr (Value UTCTime)
                         , expr (Value Bool)
                         , expr (Value T.Text)
                         )
columnsWithoutContent r =
    ( r ^. DB.ResourceId
    , r ^. DB.ResourcePath
    , r ^. DB.ResourceCreatedBy
    , r ^. DB.ResourceCreatedAt
    , r ^. DB.ResourcePublic
    , r ^. DB.ResourceContentType
    )
