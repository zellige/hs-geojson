module Data.SeqHelperTests where

-- Local
import Arbitrary ()
import qualified Data.SeqHelper as SeqHelper
import qualified Data.Sequence as Sequence
import Test.Hspec
  ( Spec,
    context,
    describe,
    it,
    shouldBe,
  )
import Test.Tasty
import Test.Tasty.Hspec
  ( testSpec,
  )
import Test.Tasty.QuickCheck
  ( Property,
    property,
    testProperty,
  )

-- Tests

tests :: IO TestTree
tests =
  testSpec "Data.SeqHelper.removeNextDuplicate" testRemoveNextDuplicate

testRemoveNextDuplicate :: Spec
testRemoveNextDuplicate =
  describe "removeNextDuplicate" $
    it "Make it clear" $
      SeqHelper.removeNextDuplicate (Sequence.fromList ([1, 1, 2, 3, 3, 3, 1] :: [Int])) `shouldBe` Sequence.fromList ([1, 2, 3, 1] :: [Int])
