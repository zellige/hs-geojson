module Data.LineStringTests where

-- Local
import Arbitrary ()
import qualified Data.Geospatial.Internal.BasicTypes as BasicTypes
import qualified Data.LineString as LineString
import qualified Data.Sequence as Sequence
import Data.Validation (Validation (..))
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
tests = do
  specs <- specTests
  pure $ testGroup "Data.LineStringTests" [qcTests, specs]

qcTests :: TestTree
qcTests =
  testGroup
    "Data.LineStringTests.QuickCheck"
    [ testProperty "Data.LineString.lineStringLength" testLineStringLength,
      testProperty "Data.LineString.fromLineString" testFromLineString,
      testProperty "Data.LineString.Foldable" testFoldable
    ]

specTests :: IO TestTree
specTests = do
  specs <-
    sequence
      [ testSpec "Data.LineString.fromList" testFromList,
        testSpec "Data.LineString.fromSeq" testFromSequence,
        testSpec "Data.LineString.toSeq" testToSequence,
        testSpec "Data.LineString.combineToSequence" testCombineToSequence
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
-- > (\xs -> (lineStringHead xs) == (foldr (\a -> const a) 0 xs)) (xs :: LineString Int)
--
testFoldable :: LineString.LineString Int -> Property
testFoldable xs = property $ (foldr (:) [] xs == LineString.fromLineString xs) && (LineString.lineStringHead xs == foldr const 0 xs)

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
-- testFromList :: Spec
testFromList =
  describe "fromList" $ do
    it "creates a LineString out of a list of elements" $ do
      LineString.fromList ([0, 1] :: [Int]) `shouldBe` Success (LineString.makeLineString 0 1 Sequence.empty)
      LineString.fromList ([0, 1, 2] :: [Int]) `shouldBe` Success (LineString.makeLineString 0 1 (Sequence.fromList [2]))
      LineString.fromList ([0, 1, 2, 4, 5, 0] :: [Int]) `shouldBe` Success (LineString.makeLineString 0 1 (Sequence.fromList [2, 4, 5, 0]))
    context "when provided with invalid input" $
      it "fails" $ do
        LineString.fromList ([] :: [Int]) `shouldBe` Failure LineString.ListEmpty
        LineString.fromList ([0] :: [Int]) `shouldBe` Failure LineString.SingletonList

-- testFromSequence :: Spec
testFromSequence =
  describe "fromSeq" $ do
    it "creates a LineString out of a Sequence of elements" $ do
      LineString.fromSeq (Sequence.fromList [0, 1] :: (Sequence.Seq Int)) `shouldBe` Success (LineString.makeLineString 0 1 Sequence.empty)
      LineString.fromSeq (Sequence.fromList [0, 1, 2] :: (Sequence.Seq Int)) `shouldBe` Success (LineString.makeLineString 0 1 (Sequence.fromList [2]))
      LineString.fromSeq (Sequence.fromList [0, 1, 2, 4, 5, 0] :: (Sequence.Seq Int)) `shouldBe` Success (LineString.makeLineString 0 1 (Sequence.fromList [2, 4, 5, 0]))
    context "when provided with invalid input" $
      it "fails" $ do
        LineString.fromSeq (Sequence.fromList [] :: (Sequence.Seq Int)) `shouldBe` Failure LineString.SequenceEmpty
        LineString.fromSeq (Sequence.fromList [0] :: (Sequence.Seq Int)) `shouldBe` Failure LineString.SingletonSequence

-- testCombineToSequence :: Spec
testCombineToSequence =
  describe "combineToSeq" $
    it "combine a LineString using PointXY" $ do
      LineString.combineToSeq BasicTypes.PointXY (LineString.makeLineString 0 1 Sequence.empty) `shouldBe` Sequence.fromList [BasicTypes.PointXY 0 1]
      LineString.combineToSeq BasicTypes.PointXY (LineString.makeLineString 0 1 (Sequence.fromList [2])) `shouldBe` Sequence.fromList [BasicTypes.PointXY 0 1, BasicTypes.PointXY 1 2]
      LineString.combineToSeq BasicTypes.PointXY (LineString.makeLineString 0 1 (Sequence.fromList [2, 4, 5, 0])) `shouldBe` Sequence.fromList [BasicTypes.PointXY 0 1, BasicTypes.PointXY 1 2, BasicTypes.PointXY 2 4, BasicTypes.PointXY 4 5, BasicTypes.PointXY 5 0]

-- testToSequence :: Spec
testToSequence =
  describe "toSeq" $
    it "from a LineString to a Sequence" $ do
      LineString.toSeq (LineString.makeLineString 0 1 Sequence.empty) `shouldBe` Sequence.fromList ([0, 1] :: [Int])
      LineString.toSeq (LineString.makeLineString 0 1 (Sequence.fromList [2])) `shouldBe` Sequence.fromList ([0, 1, 2] :: [Int])
      LineString.toSeq (LineString.makeLineString 0 1 (Sequence.fromList [2, 4, 5, 0])) `shouldBe` Sequence.fromList ([0, 1, 2, 4, 5, 0] :: [Int])

-- TODO
-- (\xs -> safeLast (fromLineString xs) == Just (lineStringHead xs)) (xs :: LineString Int)
