{-# LANGUAGE OverloadedStrings #-}

module Uri
(
  (+/+)
)
where

import qualified Data.Text as T  ( Text, append )

(+/+) :: T.Text -> T.Text -> T.Text
(+/+) x y =
    x `T.append` "/" `T.append` y
