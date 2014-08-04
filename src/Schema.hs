
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

import qualified Data.Text as T
                            ( Text )
import Data.Time            ( UTCTime )
import Database.Persist.TH  ( share, mkPersist, sqlOnlySettings, mkMigrate, persistLowerCase )


share [mkPersist sqlOnlySettings, mkMigrate "migrate"] [persistLowerCase|

    User
        email        T.Text
        displayName  T.Text
        UniqueUserEmail        email
        UniqueUserDisplayName  displayName
      deriving Show Read Eq Ord

    Tag
        name         T.Text
        UniqueTagName           name
      deriving Show Read Eq Ord

    Resource json
        path         T.Text
        createdBy    UserId
        createdAt    UTCTime
        public       Bool
        contentType  T.Text
        UniqueResourcePath      path
      deriving Show Read Eq Ord

|]
