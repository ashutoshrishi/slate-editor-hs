module Text.Slate (
  Value(..),
  Print(..),
  toJSON, fromJSON
  ) where

import           Data.Aeson           (eitherDecode, encode)
import qualified Data.ByteString.Lazy as BL
import           Text.Slate.Print
import           Text.Slate.Types


toJSON :: Value -> BL.ByteString
toJSON = encode

fromJSON :: BL.ByteString -> Either String Value
fromJSON = eitherDecode
