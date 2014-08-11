
module DataAccess.Resources
(
  getAllResources
)
where

import Database.Esqueleto  ( Value(..), select, from, (^.) )

import Core                ( DatabaseEnvironmentT )
import Database            ( runSql )
import Json.Resource       ( Resource(..) )
import Json.WithId         ( WithId(..), addId, idToText )
import qualified Schema as DB


getAllResources :: DatabaseEnvironmentT [WithId Resource]
getAllResources =
    runSql $ do
        resources <- select $ from $ \r ->
                     return ( r ^. DB.ResourceId
                            , r ^. DB.ResourcePath
                            , r ^. DB.ResourceCreatedBy
                            , r ^. DB.ResourceCreatedAt
                            , r ^. DB.ResourcePublic
                            , r ^. DB.ResourceContentType
                            )
        return $ map dToJ resources
  where
    dToJ ( Value rId
         , Value rPath
         , Value rCreatedBy
         , Value rCreatedAt
         , Value rPublic
         , Value rContentType
         ) =
        addId rId $ Resource rPath (Just $ idToText rCreatedBy) (Just rCreatedAt) rPublic rContentType
