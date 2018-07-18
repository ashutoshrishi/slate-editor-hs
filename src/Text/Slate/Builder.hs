{-# LANGUAGE DeriveFunctor #-}
module Text.Slate.Builder (
  -- * Builder state monad
    Builder, makeValue
  -- * Construction DSL
  , Editor, block, voidOpenBlock, text, plain
  ) where

import           Control.Monad.Free
import           Control.Monad.Trans.State
import qualified Data.Text.Lazy            as TL
import           Text.Slate.Types

data EditorF a
  = OpenBlock TL.Text Bool a
  | WriteText [Mark] TL.Text a
  deriving Functor

type Editor = Free EditorF

block :: TL.Text -> Editor ()
block ty = liftF (OpenBlock ty False ())

voidOpenBlock :: TL.Text -> Editor ()
voidOpenBlock ty = liftF (OpenBlock ty True ())

plain :: TL.Text -> Editor ()
plain txt = liftF (WriteText [] txt ())

text :: [Mark] -> TL.Text -> Editor ()
text ms txt = liftF (WriteText ms txt ())

type Builder = State [Node]

makeValue :: Editor () -> Value
makeValue editor =
  let blocks = execState (collectBlocks editor) []
  in Value (Document Nothing blocks)

collectBlocks :: Editor () -> Builder ()
collectBlocks (Free (OpenBlock ty void r))  = do
  modify (Block ty void Nothing [] :)
  collectBlocks r
collectBlocks (Free (WriteText ms txt r)) = do
  modify $ appendText (Text [Leaf ms txt])
  collectBlocks r
collectBlocks (Pure a)                   = return a

appendText :: Node -> [Node] -> [Node]
appendText txt@(Text _) (Block ty void d ns : bs) =
  Block ty void d (ns ++ [txt]) : bs
appendText _ ns = ns

