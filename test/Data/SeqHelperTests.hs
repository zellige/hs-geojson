module Data.SeqHelperTests where

import qualified Data.List.NonEmpty                  as ListNonEmpty
import qualified Data.Sequence                       as Sequence
import qualified Data.Validation                     as Validation
import           Test.Tasty
import           Test.Tasty.Hspec                    (Spec, context, describe,
                                                      it, shouldBe, testSpec)
import           Test.Tasty.QuickCheck               (Property, property,
                                                      testProperty)
-- Local
import           Arbitrary                           ()
import qualified Data.Geospatial.Internal.BasicTypes as BasicTypes
import qualified Data.SeqHelper                      as SeqHelper

-- Tests

tests :: IO TestTree
tests =
  testSpec "Data.SeqHelper.removeNextDuplicate" testRemoveNextDuplicate

testRemoveNextDuplicate :: Spec
testRemoveNextDuplicate =
  describe "removeNextDuplicate" $
    it "Make it clear" $
      SeqHelper.removeNextDuplicate (Sequence.fromList ([1, 1, 2, 3, 3, 3, 1] :: [Int])) `shouldBe` Sequence.fromList ([1, 2, 3, 1] :: [Int])
