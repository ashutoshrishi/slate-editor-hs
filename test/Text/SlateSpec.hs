module Text.SlateSpec where

import           Data.Text.Lazy.Encoding (encodeUtf8)
import qualified Data.Text.Lazy.IO       as TL
import           Test.Hspec
import           Text.Slate              (Value (..), fromJSON)

spec :: Spec
spec =
  describe "Value parsing" $
    it "decoding a Value from slate JSON" $ do
      json <- encodeUtf8 <$> TL.readFile "test/sample.json"
      fmap (const True) (fromJSON json) `shouldBe` Right True
