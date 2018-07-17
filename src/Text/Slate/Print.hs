{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Text.Slate.Print (
  -- * Printing class for generating textual representations
    Print, printPlain, printHtml
  ) where

import qualified Data.Text.Lazy as TL

-- | A class of type constructors whose data values have printable
-- textual representations.
class Print a where
  printPlain :: a -> TL.Text

  printHtml :: a -> TL.Text
  printHtml x = "<p>" `mappend` printPlain x `mappend` "<p>"

instance Print TL.Text where
  printPlain = id

instance Print a => Print [a] where
  printPlain = TL.unwords . fmap printPlain

instance Print a => Print (Maybe a) where
  printPlain = maybe "" printPlain
