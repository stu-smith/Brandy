
{-# LANGUAGE OverloadedStrings #-}

module UserTestUtility
(
  runTestWithUser
)
where

import Control.Applicative  ( (<$>) )
import qualified Data.Text as T
                            ( Text )
import Network.Wai          ( Application )

import Json.PrivateUser     ( PrivateUser(..) )
import Json.WithId          ( WithId(..), getId )
import TestUtility          ( runTest, post, jsonBody, uri )


runTestWithUser :: (Application -> T.Text -> IO ()) -> IO ()
runTestWithUser test =
    runTest $ \app -> do
        let usersBase = ["api", "users"]
        let insertBody = PrivateUser "Display Name" "email@example.com"
        inserted <- jsonBody <$> (app `post` (uri usersBase)) insertBody :: IO (WithId PrivateUser)
        let uid = getId inserted
        test app uid
