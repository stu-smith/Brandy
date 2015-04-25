
{-# LANGUAGE OverloadedStrings #-}

module UserTestUtility
(
  runTestWithUser
, qUid
)
where

import qualified Data.Text as T
                         ( Text )
import Network.Wai       ( Application )

import Json.PrivateUser  ( PrivateUser(..) )
import Json.WithId       ( WithId, getId )
import TestUtility       ( URIBuilder, runTest, post, jsonBody, uri, query )


runTestWithUser :: (Application -> T.Text -> IO ()) -> IO ()
runTestWithUser test =
    runTest $ \app -> do
        let usersBase = ["api", "users"]
        let insertBody = PrivateUser "Display Name" "email@example.com"
        inserted <- jsonBody <$> (app `post` (uri usersBase)) insertBody :: IO (WithId PrivateUser)
        let uid = getId inserted
        test app uid

qUid :: T.Text -> URIBuilder
qUid =
    query "uid"
