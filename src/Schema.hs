
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE GADTs, TypeFamilies                 #-}
{-# LANGUAGE TemplateHaskell, QuasiQuotes        #-}
{-# LANGUAGE OverloadedStrings                   #-}

module Schema
where

import qualified Data.ByteString as BS
                            ( ByteString )
import qualified Data.Text as T
                            ( Text )
import Data.Time            ( UTCTime )
import Database.Persist.TH  ( share, mkPersist, sqlOnlySettings, mkMigrate, persistLowerCase )


share [mkPersist sqlOnlySettings, mkMigrate "migrate"] [persistLowerCase|

    User
        email           T.Text
        displayName     T.Text
        UniqueUserEmail             email
        UniqueUserDisplayName       displayName

    Tag
        name            T.Text
        UniqueTagName               name

    Resource
        path            T.Text
        createdBy       UserId
        createdAt       UTCTime
        public          Bool
        contentType     T.Text
        data            BS.ByteString
        UniqueResourcePath          path

|]
