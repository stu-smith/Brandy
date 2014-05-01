
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Schema
where

import Data.Text            ( Text )
import Data.Time            ( UTCTime )
import Database.Persist.TH  ( share, mkPersist, sqlOnlySettings, mkMigrate, persistLowerCase )


share [mkPersist sqlOnlySettings, mkMigrate "migrate"] [persistLowerCase|

    User
        email        Text
        displayName  Text
        UniqueUserEmail        email
        UniqueUserDisplayName  displayName
      deriving Show Read Eq Ord

    Resource json
        path         Text
        createdBy    UserId
        createdAt    UTCTime
        public       Bool
        contentType  Text
        content      Text
        UniqueResourcePath  path
      deriving Show Read Eq Ord

|]
