module Data.LineStringTests where

import           Data.Foldable         (Foldable (..))
import           Data.Validation       (Validation (..))
import qualified Data.Vector           as Vector
import           Test.Tasty
import           Test.Tasty.Hspec      (Spec, context, describe, it, shouldBe,
                                        testSpec)
import           Test.Tasty.QuickCheck (Property, property, testProperty)
-- Local
import           Arbitrary             ()
import qualified Data.LineString       as LineString


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
    , testSpec "Data.LineString.combineToVector" testCombineToVector
    ]
  pure $ testGroup "Data.LineStringTests.Spec" specs

-- QuickCheck

-- (\xs -> lineStringLength xs == (length (fromLineString xs))) (xs :: LineString Int)
--
testLineStringLength :: LineString.LineString Int -> Property
testLineStringLength xs = property $ LineString.lineStringLength xs == length (LineString.fromLineString xs)

-- (\xs -> length (fromLineString xs) >= 4) (xs :: LineString Int)
--
testFromLineString :: LineString.LineString Int -> Property
testFromLineString xs = property $ length (LineString.fromLineString xs) >= 2

-- > (\xs -> (foldr (:) [] xs) == (fromLineString xs)) (xs :: LineString Int)
--
-- > (\xs -> (lineStringHead xs) == (foldr'' (\a -> const a) 0 xs)) (xs :: LineString Int)
--
testFoldable :: LineString.LineString Int -> Property
testFoldable xs = property $ (foldr (:) [] xs == LineString.fromLineString xs) && (LineString.lineStringHead xs == foldr' const 0 xs)

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
      LineString.fromList ([0, 1] :: [Int])             `shouldBe` Success (LineString.makeLineString 0 1 [])
      LineString.fromList ([0, 1, 2] :: [Int])          `shouldBe` Success (LineString.makeLineString 0 1 [2])
      LineString.fromList ([0, 1, 2, 4, 5, 0] :: [Int]) `shouldBe` Success (LineString.makeLineString 0 1 [2, 4, 5, 0])
    context "when provided with invalid input" $
      it "fails" $ do
        LineString.fromList ([] :: [Int])  `shouldBe` Failure LineString.ListEmpty
        LineString.fromList ([0] :: [Int]) `shouldBe` Failure LineString.SingletonList

fromVector :: Spec
fromVector =
  describe "fromVector" $ do
    it "creates a LineString out of a Vector of elements" $ do
      LineString.fromVector (Vector.fromList [0, 1] :: (Vector.Vector Int))             `shouldBe` Success (LineString.makeLineString 0 1 [])
      LineString.fromVector (Vector.fromList [0, 1, 2] :: (Vector.Vector Int))          `shouldBe` Success (LineString.makeLineString 0 1 [2])
      LineString.fromVector (Vector.fromList [0, 1, 2, 4, 5, 0] :: (Vector.Vector Int)) `shouldBe` Success (LineString.makeLineString 0 1 [2, 4, 5, 0])
    context "when provided with invalid input" $
      it "fails" $ do
        LineString.fromVector (Vector.fromList [] :: (Vector.Vector Int))  `shouldBe` Failure LineString.VectorEmpty
        LineString.fromVector (Vector.fromList [0] :: (Vector.Vector Int)) `shouldBe` Failure LineString.SingletonVector

testCombineToVector :: Spec
testCombineToVector =
  describe "combineToVector" $
    it "combine a LineString using tuples" $ do
      LineString.combineToVector (,) (LineString.makeLineString 0 1 [])           `shouldBe` Vector.fromList ([(0, 1)] :: [(Int, Int)])
      LineString.combineToVector (,) (LineString.makeLineString 0 1 [2])          `shouldBe` Vector.fromList ([(0, 1), (1,2)] :: [(Int, Int)])
      LineString.combineToVector (,) (LineString.makeLineString 0 1 [2, 4, 5, 0]) `shouldBe` Vector.fromList ([(0, 1), (1, 2), (2, 4), (4, 5), (5, 0)] :: [(Int, Int)])

-- TODO
-- (\xs -> safeLast (fromLineString xs) == Just (lineStringHead xs)) (xs :: LineString Int)
-- toVector
