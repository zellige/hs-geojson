module Data.LineStringTests where

import           Data.Foldable         (Foldable (..))
import           Data.Validation       (Validation (..))
import           Test.Tasty
import           Test.Tasty.Hspec      (Spec, context, describe, it, shouldBe,
                                        testSpec)
import           Test.Tasty.QuickCheck (Property, property, testProperty)
-- Local
import           Arbitrary             ()
import           Data.LineString


-- Tests

tests :: IO TestTree
tests = do
  specs <- specTests
  pure $ testGroup "Data.LineStringTests" [qcTests, specs]

qcTests :: TestTree
qcTests = testGroup "Data.LineStringTests.QuickCheck"
  [ testProperty "Data.LineString.lineStringLength" testLineStringLength
  , testProperty "Data.LineString.fromLineString" testFromLineString
  , testProperty "Data.LineString.Foldable" testFoldable
  ]

specTests :: IO TestTree
specTests = do
  specs <- sequence
    [ testSpec "Data.LineString.fromList" testFromList
    ]
  pure $ testGroup "Data.LineStringTests.Spec" specs

-- QuickCheck

-- (\xs -> lineStringLength xs == (length (fromLineString xs))) (xs :: LineString Int)
--
testLineStringLength :: LineString Int -> Property
testLineStringLength xs = property $ lineStringLength xs == length (fromLineString xs)

-- (\xs -> length (fromLineString xs) >= 4) (xs :: LineString Int)
--
testFromLineString :: LineString Int -> Property
testFromLineString xs = property $ length (fromLineString xs) >= 2

-- > (\xs -> (foldr (:) [] xs) == (fromLineString xs)) (xs :: LineString Int)
--
-- > (\xs -> (lineStringHead xs) == (foldr'' (\a -> const a) 0 xs)) (xs :: LineString Int)
--
testFoldable :: LineString Int -> Property
testFoldable xs = property $ (foldr (:) [] xs == fromLineString xs) && (lineStringHead xs == foldr' const 0 xs)

-- Spec

-- >>> fromList [] :: Validation ListToLineStringError (LineString Int)
-- Failure List Empty
--
-- >>> fromList [0] :: Validation ListToLineStringError (LineString Int)
-- Failure Singleton List
--
-- >>> fromList [0, 1] :: Validation ListToLineStringError (LineString Int)
-- Success [0,1]
--
-- >>> fromList [0, 1, 2] :: Validation ListToLineStringError (LineString Int)
-- Success [0,1,2]
--
-- >>> fromList [0, 1, 2, 4, 5, 0] :: Validation ListToLineStringError (LineString Int)
-- Success [0,1,2,4,5,0]
--
testFromList :: Spec
testFromList =
  describe "fromList" $ do
    it "creates a LineString out of a list of elements" $ do
      fromList ([0, 1] :: [Int])             `shouldBe` Success (makeLineString 0 1 [])
      fromList ([0, 1, 2] :: [Int])          `shouldBe` Success (makeLineString 0 1 [2])
      fromList ([0, 1, 2, 4, 5, 0] :: [Int]) `shouldBe` Success (makeLineString 0 1 [2, 4, 5, 0])
    context "when provided with invalid input" $
      it "fails" $ do
        fromList ([] :: [Int])  `shouldBe` Failure ListEmpty
        fromList ([0] :: [Int]) `shouldBe` Failure SingletonList

-- TODO
-- (\xs -> safeLast (fromLineString xs) == Just (lineStringHead xs)) (xs :: LineString Int)
--
