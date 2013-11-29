module WSCommon (
    Message (..),
    textToLazyBS
) where

import qualified Data.ByteString.UTF8        as BU
import qualified Data.ByteString.Lazy        as BL
import qualified Data.Text                   as T


data Message = Message (Maybe T.Text) (Maybe Bool) deriving (Eq, Ord, Show)


textToLazyBS :: T.Text -> BL.ByteString
textToLazyBS = BL.fromStrict . BU.fromString . T.unpack