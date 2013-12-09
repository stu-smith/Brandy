
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Schema
( migrate
, User, UserId
, Content, ContentId
)
where

import Data.Time            ( UTCTime )
import Database.Persist.TH  ( share, mkPersist, sqlSettings, mkMigrate, persistLowerCase )


share [mkPersist sqlSettings, mkMigrate "migrate"] [persistLowerCase|

  User
    email        String
    displayName  String
    deriving Show Read Eq Ord

  Content
    path         String
    created      UTCTime
    modified     UTCTime
    contentType  String
    content      String
    deriving Show Read Eq Ord

|]
