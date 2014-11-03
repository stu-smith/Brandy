
module Transforms.ResourceTransform
(
  ResourceTransform(..)
)
where

import qualified Data.ByteString as BS  ( ByteString )
import qualified Data.Text as T         ( Text )


data ResourceTransform = ResourceTransform
    { fromContentType :: T.Text
    , toContentType   :: T.Text
    , transform       :: BS.ByteString -> BS.ByteString
    }
