module Text.Slate.TypesSpec where

import           Data.Aeson              (decode)
import           Data.Text.Lazy.Encoding (encodeUtf8)
import qualified Data.Text.Lazy.IO       as TL
import           Test.Hspec
import           Text.Slate.Types

spec :: Spec
spec =
  describe "Value parsing" $
    it "decoding a Value from slate JSON" $ do
      json <- encodeUtf8 <$> TL.readFile "test/sample.json"
      verify (decode json) `shouldBe` True


verify :: Maybe Value -> Bool
verify (Just (Value _)) = True
verify _                = False
