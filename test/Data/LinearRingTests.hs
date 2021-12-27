module Data.LinearRingTests where

-- Local
import Arbitrary ()
import qualified Data.Geospatial.Internal.BasicTypes as BasicTypes
import qualified Data.LinearRing as LinearRing
import qualified Data.List.NonEmpty as ListNonEmpty
import qualified Data.Sequence as Sequence
import qualified Data.Validation as Validation
import Test.Tasty
import Test.Tasty.Hspec
  ( Spec,
    context,
    describe,
    it,
    shouldBe,
    testSpec,
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
  pure $ testGroup "Data.LinearRingTests" [qcTests, specs]

qcTests :: TestTree
qcTests =
  testGroup
    "Data.LinearRingTests.QuickCheck"
    [ testProperty "Data.LinearRing.ringLength" testRingLength,
      testProperty "Data.LinearRing.fromLinearRing" testFromLinearRing,
      testProperty "Data.LinearRing.Foldable" testFoldable
    ]

specTests :: IO TestTree
specTests = do
  specs <-
    sequence
      [ testSpec "Data.LinearRing.fromList" testFromList,
        testSpec "Data.LinearRing.fromSeq" testFromSequence,
        testSpec "Data.LinearRing.combineToSequence" testCombineToSequence,
        testSpec "Data.LinearRing.toSequence" testToSequence,
        testSpec "Data.LinearRing.foldMap" testFoldMap
      ]
  pure $ testGroup "Data.LinearRingTests.Spec" specs

-- QuickCheck

-- > (\xs -> ringLength xs == (length (fromLinearRing xs))) (xs :: LinearRing Int)
--
testRingLength :: LinearRing.LinearRing Int -> Property
testRingLength xs = property $ LinearRing.ringLength xs == length (LinearRing.fromLinearRing xs)

-- > (\xs -> length (fromLinearRing xs) >= 4) (xs :: LinearRing Int)
--
testFromLinearRing :: LinearRing.LinearRing Int -> Property
testFromLinearRing xs = property $ length (LinearRing.fromLinearRing xs) >= 4

-- > (\xs -> (foldr (:) [] xs) == (fromLinearRing xs)) (xs :: LinearRing Int)
--
-- > (\xs -> (ringHead xs) == (foldr (\a -> const a) 0 xs)) (xs :: LinearRing Int)
--
testFoldable :: LinearRing.LinearRing Int -> Property
testFoldable xs = property $ (foldr (:) [] xs == LinearRing.fromLinearRing xs) && (LinearRing.ringHead xs == foldr const 0 xs)

-- Spec

-- >>> fromList [] :: AccValidation (NonEmpty (ListToLinearRingError Int)) (LinearRing Int)
-- AccFailure (List too short: (length = 0) :| [])
--
-- >>> fromList [0] :: AccValidation (NonEmpty (ListToLinearRingError Int)) (LinearRing Int)
-- AccFailure (List too short: (length = 1) :| [])
--
-- >>> fromList [0, 1] :: AccValidation (NonEmpty (ListToLinearRingError Int)) (LinearRing Int)
-- AccFailure (List too short: (length = 2) :| [])
--
-- >>> fromList [0, 1, 2] :: AccValidation (NonEmpty (ListToLinearRingError Int)) (LinearRing Int)
-- AccFailure (List too short: (length = 3) :| [])
--
-- >>> fromList [0, 1, 2, 3] :: AccValidation (NonEmpty (ListToLinearRingError Int)) (LinearRing Int)
-- AccSuccess [0,1,2,0]
--
-- >>> fromList [0, 1, 2, 4, 0] :: AccValidation (NonEmpty (ListToLinearRingError Int)) (LinearRing Int)
-- AccSuccess [0,1,2,4,0]
--
-- >>> fromList [0, 1, 2, 4, 5, 0] :: AccValidation (NonEmpty (ListToLinearRingError Int)) (LinearRing Int)
-- AccSuccess [0,1,2,4,5,0]

-- >>> fromList [0, 1, 2, 4, 5, 6] :: AccValidation (NonEmpty (ListToLinearRingError Int)) (LinearRing Int)
-- AccSuccess [0,1,2,4,5,0]
--
testFromList :: Spec
testFromList =
  describe "fromList" $ do
    it "creates a LinearRing out of a list of elements" $ do
      LinearRing.fromList ([0, 1, 2, 3] :: [Int]) `shouldBe` Validation.Success (LinearRing.makeLinearRing 0 1 2 Sequence.empty)
      LinearRing.fromList ([0, 1, 2, 4, 0] :: [Int]) `shouldBe` Validation.Success (LinearRing.makeLinearRing 0 1 2 (Sequence.fromList [4]))
      LinearRing.fromList ([0, 1, 2, 4, 5, 0] :: [Int]) `shouldBe` Validation.Success (LinearRing.makeLinearRing 0 1 2 (Sequence.fromList [4, 5]))
      LinearRing.fromList ([0, 1, 2, 4, 5, 6] :: [Int]) `shouldBe` Validation.Success (LinearRing.makeLinearRing 0 1 2 (Sequence.fromList [4, 5]))
    context "when provided with invalid input" $
      it "fails" $ do
        LinearRing.fromList [] `shouldBe` Validation.Failure (LinearRing.ListTooShort 0 ListNonEmpty.:| [] :: ListNonEmpty.NonEmpty (LinearRing.ListToLinearRingError Int))
        LinearRing.fromList [0] `shouldBe` Validation.Failure (LinearRing.ListTooShort 1 ListNonEmpty.:| [] :: ListNonEmpty.NonEmpty (LinearRing.ListToLinearRingError Int))
        LinearRing.fromList [0, 1] `shouldBe` Validation.Failure (LinearRing.ListTooShort 2 ListNonEmpty.:| [] :: ListNonEmpty.NonEmpty (LinearRing.ListToLinearRingError Int))
        LinearRing.fromList [0, 1, 2] `shouldBe` Validation.Failure (LinearRing.ListTooShort 3 ListNonEmpty.:| [] :: ListNonEmpty.NonEmpty (LinearRing.ListToLinearRingError Int))

testFromSequence :: Spec
testFromSequence =
  describe "fromSeq" $ do
    it "creates a LinearRing out of a Sequence of elements" $ do
      LinearRing.fromSeq (Sequence.fromList ([0, 1, 0] :: [Int])) `shouldBe` Validation.Success (LinearRing.makeLinearRing 0 1 0 Sequence.empty)
      LinearRing.fromSeq (Sequence.fromList ([0, 1, 2, 0] :: [Int])) `shouldBe` Validation.Success (LinearRing.makeLinearRing 0 1 2 (Sequence.fromList [0]))
      LinearRing.fromSeq (Sequence.fromList ([0, 1, 2, 4, 0] :: [Int])) `shouldBe` Validation.Success (LinearRing.makeLinearRing 0 1 2 (Sequence.fromList [4, 0]))
      LinearRing.fromSeq (Sequence.fromList ([0, 1, 2, 4, 5, 0] :: [Int])) `shouldBe` Validation.Success (LinearRing.makeLinearRing 0 1 2 (Sequence.fromList [4, 5, 0]))
    context "when provided with invalid input" $
      it "fails" $ do
        LinearRing.fromSeq (Sequence.fromList []) `shouldBe` Validation.Failure (LinearRing.SequenceTooShort 0 ListNonEmpty.:| [] :: ListNonEmpty.NonEmpty (LinearRing.SequenceToLinearRingError Int))
        LinearRing.fromSeq (Sequence.fromList [0]) `shouldBe` Validation.Failure (LinearRing.SequenceTooShort 1 ListNonEmpty.:| [] :: ListNonEmpty.NonEmpty (LinearRing.SequenceToLinearRingError Int))
        LinearRing.fromSeq (Sequence.fromList [0, 1]) `shouldBe` Validation.Failure (LinearRing.SequenceTooShort 2 ListNonEmpty.:| [] :: ListNonEmpty.NonEmpty (LinearRing.SequenceToLinearRingError Int))
        LinearRing.fromSeq (Sequence.fromList [0, 1, 2]) `shouldBe` Validation.Failure (LinearRing.FirstNotEqualToLast 0 2 ListNonEmpty.:| [] :: ListNonEmpty.NonEmpty (LinearRing.SequenceToLinearRingError Int))

testCombineToSequence :: Spec
testCombineToSequence =
  describe "combineToSeq" $
    it "combine a LinearRing using tuples" $ do
      LinearRing.combineToSeq BasicTypes.PointXY (LinearRing.makeLinearRing 0 1 2 Sequence.empty) `shouldBe` Sequence.fromList [BasicTypes.PointXY 0 1, BasicTypes.PointXY 1 2]
      LinearRing.combineToSeq BasicTypes.PointXY (LinearRing.makeLinearRing 0 1 2 (Sequence.fromList [4])) `shouldBe` Sequence.fromList [BasicTypes.PointXY 0 1, BasicTypes.PointXY 1 2, BasicTypes.PointXY 2 4]
      LinearRing.combineToSeq BasicTypes.PointXY (LinearRing.makeLinearRing 0 1 2 (Sequence.fromList [4, 5])) `shouldBe` Sequence.fromList [BasicTypes.PointXY 0 1, BasicTypes.PointXY 1 2, BasicTypes.PointXY 2 4, BasicTypes.PointXY 4 5]

testToSequence :: Spec
testToSequence =
  describe "toSeq" $
    it "from a LinearRing to a Sequence" $ do
      LinearRing.toSeq (LinearRing.makeLinearRing 0 1 0 Sequence.empty) `shouldBe` Sequence.fromList ([0, 1, 0] :: [Int])
      LinearRing.toSeq (LinearRing.makeLinearRing 0 1 2 (Sequence.fromList [0])) `shouldBe` Sequence.fromList ([0, 1, 2, 0] :: [Int])
      LinearRing.toSeq (LinearRing.makeLinearRing 0 1 2 (Sequence.fromList [4, 0])) `shouldBe` Sequence.fromList ([0, 1, 2, 4, 0] :: [Int])

testFoldMap :: Spec
testFoldMap =
  describe "foldMap" $
    it "foldMap of a LinearRing" $
      foldMap (\x -> Sequence.singleton (x + 1)) (LinearRing.makeLinearRing 0 1 2 Sequence.empty) `shouldBe` Sequence.fromList ([1, 2, 3, 1] :: [Int])

-- TODO
-- > (\xs -> safeLast (fromLinearRing xs) == Just (ringHead xs)) (xs :: LinearRing Int)
--
-- > (\x xs -> length (foldrDropLast (:) [] (x : xs)) == length xs) (x :: Int) (xs :: [Int])
--
