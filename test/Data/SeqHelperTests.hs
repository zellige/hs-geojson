module Data.SeqHelperTests where

-- Local
import Arbitrary ()
import qualified Data.SeqHelper as SeqHelper
import qualified Data.Sequence as Sequence
import Test.Tasty
import Test.Tasty.Hspec (Spec, describe, it, shouldBe, testSpec)

-- Tests

tests :: IO TestTree
tests =
  testSpec "Data.SeqHelper.removeNextDuplicate" testRemoveNextDuplicate

testRemoveNextDuplicate :: Spec
testRemoveNextDuplicate =
  describe "removeNextDuplicate" $
    it "Make it clear" $
      SeqHelper.removeNextDuplicate (Sequence.fromList ([1, 1, 2, 3, 3, 3, 1] :: [Int])) `shouldBe` Sequence.fromList ([1, 2, 3, 1] :: [Int])
