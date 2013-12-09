
import           Database.Persist

module Schema
(
)
where


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|

  User
    email        String
    displayName  String

  Content
    path         String
    created      UTCTime
    modified     UTCTime
    contentType  String
    content      String
