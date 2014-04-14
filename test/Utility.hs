
{-# LANGUAGE OverloadedStrings #-}

module Utility where

import qualified Data.ByteString   as BS
import qualified Network.Wai       as W
import qualified Network.Wai.Test  as WT


get :: W.Application -> BS.ByteString -> IO WT.SResponse
get app path =
  WT.runSession (WT.srequest (WT.SRequest req "")) app
      where req = WT.setRawPathInfo WT.defaultRequest path
