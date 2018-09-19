module Data.LineStringTests where

import           Data.Validation                     (Validation (..))
import qualified Data.Vector.Storable                as VectorStorable
import           Test.Tasty
import           Test.Tasty.Hspec                    (Spec, context, describe,
                                                      it, shouldBe, testSpec)
import           Test.Tasty.QuickCheck               (Property, property,
                                                      testProperty)
-- Local
import           Arbitrary                           ()
import qualified Data.Geospatial.Internal.BasicTypes as BasicTypes
import qualified Data.LineString                     as LineString
-- Tests

tests :: IO TestTree
tests = do
  specs <- specTests
  pure $ testGroup "Data.LineStringTests" [qcTests, specs]

qcTests :: TestTree
qcTests = testGroup "Data.LineStringTests.QuickCheck"
  [ testProperty "Data.LineString.lineStringLength" testLineStringLength
  , testProperty "Data.LineString.fromLineString" testFromLineString
  -- , testProperty "Data.LineString.Foldable" testFoldable
  ]

specTests :: IO TestTree
specTests = do
  specs <- sequence
    [ testSpec "Data.LineString.fromList" testFromList
    , testSpec "Data.LineString.fromVector" testFromVector
    , testSpec "Data.LineString.toVector" testToVector
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
-- testFoldable :: LineString.LineString Int -> Property
-- testFoldable xs = property $ (foldr (:) [] xs == LineString.fromLineString xs) && (LineString.lineStringHead xs == Foldable.foldr' const 0 xs)

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
      LineString.fromList ([0, 1] :: [Int])             `shouldBe` Success (LineString.makeLineString 0 1 VectorStorable.empty)
      LineString.fromList ([0, 1, 2] :: [Int])          `shouldBe` Success (LineString.makeLineString 0 1 (VectorStorable.fromList [2]))
      LineString.fromList ([0, 1, 2, 4, 5, 0] :: [Int]) `shouldBe` Success (LineString.makeLineString 0 1 (VectorStorable.fromList [2, 4, 5, 0]))
    context "when provided with invalid input" $
      it "fails" $ do
        LineString.fromList ([] :: [Int])  `shouldBe` Failure LineString.ListEmpty
        LineString.fromList ([0] :: [Int]) `shouldBe` Failure LineString.SingletonList

testFromVector :: Spec
testFromVector =
  describe "fromVector" $ do
    it "creates a LineString out of a Vector of elements" $ do
      LineString.fromVector (VectorStorable.fromList [0, 1] :: (VectorStorable.Vector Int))             `shouldBe` Success (LineString.makeLineString 0 1 VectorStorable.empty)
      LineString.fromVector (VectorStorable.fromList [0, 1, 2] :: (VectorStorable.Vector Int))          `shouldBe` Success (LineString.makeLineString 0 1 (VectorStorable.fromList [2]))
      LineString.fromVector (VectorStorable.fromList [0, 1, 2, 4, 5, 0] :: (VectorStorable.Vector Int)) `shouldBe` Success (LineString.makeLineString 0 1 (VectorStorable.fromList [2, 4, 5, 0]))
    context "when provided with invalid input" $
      it "fails" $ do
        LineString.fromVector (VectorStorable.fromList [] :: (VectorStorable.Vector Int))  `shouldBe` Failure LineString.VectorEmpty
        LineString.fromVector (VectorStorable.fromList [0] :: (VectorStorable.Vector Int)) `shouldBe` Failure LineString.SingletonVector

testCombineToVector :: Spec
testCombineToVector =
  describe "combineToVector" $
    it "combine a LineString using PointXY" $ do
      LineString.combineToVector BasicTypes.PointXY (LineString.makeLineString 0 1 VectorStorable.empty)                   `shouldBe` VectorStorable.fromList [BasicTypes.PointXY 0 1]
      LineString.combineToVector BasicTypes.PointXY (LineString.makeLineString 0 1 (VectorStorable.fromList [2]))          `shouldBe` VectorStorable.fromList [BasicTypes.PointXY 0 1, BasicTypes.PointXY 1 2]
      LineString.combineToVector BasicTypes.PointXY (LineString.makeLineString 0 1 (VectorStorable.fromList [2, 4, 5, 0])) `shouldBe` VectorStorable.fromList [BasicTypes.PointXY 0 1, BasicTypes.PointXY 1 2, BasicTypes.PointXY 2 4, BasicTypes.PointXY 4 5, BasicTypes.PointXY 5 0]

testToVector :: Spec
testToVector =
  describe "toVector" $
    it "from a LineString to a vector" $ do
      LineString.toVector (LineString.makeLineString 0 1 VectorStorable.empty)                   `shouldBe` VectorStorable.fromList ([0, 1] :: [Int])
      LineString.toVector (LineString.makeLineString 0 1 (VectorStorable.fromList [2]))          `shouldBe` VectorStorable.fromList ([0, 1, 2] :: [Int])
      LineString.toVector (LineString.makeLineString 0 1 (VectorStorable.fromList [2, 4, 5, 0])) `shouldBe` VectorStorable.fromList ([0, 1, 2, 4, 5, 0] :: [Int])

-- TODO
-- (\xs -> safeLast (fromLineString xs) == Just (lineStringHead xs)) (xs :: LineString Int)
