{-# LANGUAGE OverloadedStrings #-}
module Text.Slate (
  -- * Conversion utilities
    toJSON, fromJSON, plainDeserialize, plainSerialize
  -- * Toplevel Slate model type
  , Value(..)
  -- * Printing utilities
  , Print(..)
  ) where

import           Data.Aeson           (eitherDecode, encode)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy       as TL
import           Text.Slate.Builder   (block, makeValue, plain)
import           Text.Slate.Print
import           Text.Slate.Types


toJSON :: Value -> BL.ByteString
toJSON = encode

fromJSON :: BL.ByteString -> Either String Value
fromJSON = eitherDecode

plainDeserialize :: Value -> TL.Text
plainDeserialize = printPlain

plainSerialize :: TL.Text -> Value
plainSerialize txt = makeValue $ do
  block "paragraph"
  plain txt
