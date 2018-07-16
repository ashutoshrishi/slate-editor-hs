{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Text.Slate.Types where

import           Control.Monad    ((>=>))
import           Data.Aeson       hiding (Value)
import           Data.Aeson.Types hiding (Value)
import qualified Data.Text.Lazy   as TL
import           Text.Slate.Print


-- | Slate Node monad type.
data NodeM a
  -- | Toplevel Document node
  = Document Data [NodeM a]
  -- | Block nodes
  | Block NodeType Bool Data [NodeM a]
  -- | Text nodes
  | Text [NodeM a]
  -- | Leaf nodes with plain text and marks
  | Leaf [Mark] TL.Text a
  deriving Show

-- | Simplification of the 'NodeM' datatype.
type Node = NodeM ()

-- | Main Slate Value type.
newtype Value = Value Node deriving Show

type NodeType = TL.Text

-- | Opaque data object type, kept as a parsed JSON entity
type Data = Maybe Object

data Mark = Bold
          | Italic
          | Underline
          | OtherMark TL.Text
          deriving Show

--------------------------------------------------------------------
-- Instances                                                      --
--------------------------------------------------------------------

instance FromJSON Value where
  parseJSON = withObject "Value" ((.: "document") >=> return . Value)

instance FromJSON Node where
  parseJSON v = withObject "Node" parseNode v
    where
      parseNode obj = do
        ty <- obj .: "object" :: Parser TL.Text
        case ty of
          -- Parse document
          "document" ->
            Document <$> (obj .:? "data")
                     <*> (obj .: "nodes")

          -- Parse block
          "block" ->
            Block <$> (obj .: "type")
                  <*> (obj .: "isVoid")
                  <*> (obj .:? "data")
                  <*> (obj .: "nodes")

          -- Parse text
          "text" ->
            Text <$> (obj .: "leaves")

          -- Parse leaf
          "leaf" ->
            Leaf <$> (obj .: "marks")
                 <*> (obj .: "text")
                 <*> return ()

          _ ->
            typeMismatch "Node" v

instance FromJSON Mark where
  parseJSON (String "bold")      = return Bold
  parseJSON (String "italic")    = return Italic
  parseJSON (String "underline") = return Underline
  parseJSON (String s)           = return (OtherMark $ TL.fromStrict s)
  parseJSON v                    = typeMismatch "Mark" v

instance Print Value where
  printPlain (Value n) = printPlain n

instance Print Node where
  printPlain (Document _ ns)  = TL.intercalate "\n\n" . fmap printPlain $ ns
  printPlain (Block _ _ _ ns) = printPlain ns
  printPlain (Text ns)        = printPlain ns
  printPlain (Leaf _ ns _)    = printPlain ns
