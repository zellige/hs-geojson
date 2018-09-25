module Data.LinearRingTests where

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
import qualified Data.LinearRing                     as LinearRing

-- Tests

tests :: IO TestTree
tests = do
  specs <- specTests
  pure $ testGroup "Data.LinearRingTests" [qcTests, specs]

qcTests :: TestTree
qcTests = testGroup "Data.LinearRingTests.QuickCheck"
  [ testProperty "Data.LinearRing.ringLength" testRingLength
  , testProperty "Data.LinearRing.fromLinearRing" testFromLinearRing
  , testProperty "Data.LinearRing.Foldable" testFoldable
  ]

specTests :: IO TestTree
specTests = do
  specs <- sequence
    [ testSpec "Data.LinearRing.fromList" testFromList
    , testSpec "Data.LinearRing.fromVector" testFromVector
    , testSpec "Data.LinearRing.combineToVector" testCombineToVector
    , testSpec "Data.LinearRing.testToVector" testToVector
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
testFoldable xs = property $ (LinearRing.foldr (:) [] xs == LinearRing.fromLinearRing xs) && (LinearRing.ringHead xs == LinearRing.foldr const 0 xs)

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
      LinearRing.fromList ([0, 1, 2, 3] :: [Int])       `shouldBe` Validation.Success (LinearRing.makeLinearRing 0 1 2 Sequence.empty)
      LinearRing.fromList ([0, 1, 2, 4, 0] :: [Int])    `shouldBe` Validation.Success (LinearRing.makeLinearRing 0 1 2 (Sequence.fromList [4]))
      LinearRing.fromList ([0, 1, 2, 4, 5, 0] :: [Int]) `shouldBe` Validation.Success (LinearRing.makeLinearRing 0 1 2 (Sequence.fromList [4,5]))
      LinearRing.fromList ([0, 1, 2, 4, 5, 6] :: [Int]) `shouldBe` Validation.Success (LinearRing.makeLinearRing 0 1 2 (Sequence.fromList [4,5]))
    context "when provided with invalid input" $
      it "fails" $ do
        LinearRing.fromList []        `shouldBe` Validation.Failure (LinearRing.ListTooShort 0 ListNonEmpty.:| [] :: ListNonEmpty.NonEmpty (LinearRing.ListToLinearRingError Int))
        LinearRing.fromList [0]       `shouldBe` Validation.Failure (LinearRing.ListTooShort 1 ListNonEmpty.:| [] :: ListNonEmpty.NonEmpty (LinearRing.ListToLinearRingError Int))
        LinearRing.fromList [0, 1]    `shouldBe` Validation.Failure (LinearRing.ListTooShort 2 ListNonEmpty.:| [] :: ListNonEmpty.NonEmpty (LinearRing.ListToLinearRingError Int))
        LinearRing.fromList [0, 1, 2] `shouldBe` Validation.Failure (LinearRing.ListTooShort 3 ListNonEmpty.:| [] :: ListNonEmpty.NonEmpty (LinearRing.ListToLinearRingError Int))

testFromVector :: Spec
testFromVector =
  describe "fromVector" $ do
    it "creates a LinearRing out of a vector of elements" $ do
      LinearRing.fromVector (Sequence.fromList ([0, 1, 0] :: [Int]))         `shouldBe` Validation.Success (LinearRing.makeLinearRing 0 1 0 Sequence.empty)
      LinearRing.fromVector (Sequence.fromList ([0, 1, 2, 0] :: [Int]))      `shouldBe` Validation.Success (LinearRing.makeLinearRing 0 1 2 (Sequence.fromList [0]))
      LinearRing.fromVector (Sequence.fromList ([0, 1, 2, 4, 0] :: [Int]))   `shouldBe` Validation.Success (LinearRing.makeLinearRing 0 1 2 (Sequence.fromList [4, 0]))
      LinearRing.fromVector (Sequence.fromList ([0, 1, 2, 4, 5, 0]:: [Int])) `shouldBe` Validation.Success (LinearRing.makeLinearRing 0 1 2 (Sequence.fromList [4, 5, 0]))
    context "when provided with invalid input" $
      it "fails" $ do
        LinearRing.fromVector (Sequence.fromList [])        `shouldBe` Validation.Failure (LinearRing.VectorTooShort 0 ListNonEmpty.:| [] :: ListNonEmpty.NonEmpty (LinearRing.VectorToLinearRingError Int))
        LinearRing.fromVector (Sequence.fromList [0])       `shouldBe` Validation.Failure (LinearRing.VectorTooShort 1 ListNonEmpty.:| [] :: ListNonEmpty.NonEmpty (LinearRing.VectorToLinearRingError Int))
        LinearRing.fromVector (Sequence.fromList [0, 1])    `shouldBe` Validation.Failure (LinearRing.VectorTooShort 2 ListNonEmpty.:| [] :: ListNonEmpty.NonEmpty (LinearRing.VectorToLinearRingError Int))
        LinearRing.fromVector (Sequence.fromList [0, 1, 2]) `shouldBe` Validation.Failure (LinearRing.FirstNotEqualToLast 0 2 ListNonEmpty.:| [] :: ListNonEmpty.NonEmpty (LinearRing.VectorToLinearRingError Int))

testCombineToVector :: Spec
testCombineToVector =
  describe "combineToVector" $
    it "combine a LinearRing using tuples" $ do
      LinearRing.combineToVector BasicTypes.PointXY (LinearRing.makeLinearRing 0 1 2 Sequence.empty)             `shouldBe` Sequence.fromList [BasicTypes.PointXY 0 1, BasicTypes.PointXY 1 2]
      LinearRing.combineToVector BasicTypes.PointXY (LinearRing.makeLinearRing 0 1 2 (Sequence.fromList [4]))    `shouldBe` Sequence.fromList [BasicTypes.PointXY 0 1, BasicTypes.PointXY 1 2, BasicTypes.PointXY 2 4]
      LinearRing.combineToVector BasicTypes.PointXY (LinearRing.makeLinearRing 0 1 2 (Sequence.fromList [4,5]))  `shouldBe` Sequence.fromList [BasicTypes.PointXY 0 1, BasicTypes.PointXY 1 2, BasicTypes.PointXY 2 4, BasicTypes.PointXY 4 5]

testToVector :: Spec
testToVector =
  describe "toVector" $
    it "from a LinearRing to a vector" $ do
      LinearRing.toVector (LinearRing.makeLinearRing 0 1 0 Sequence.empty)             `shouldBe` Sequence.fromList ([0, 1, 0] :: [Int])
      LinearRing.toVector (LinearRing.makeLinearRing 0 1 2 (Sequence.fromList [0]))    `shouldBe` Sequence.fromList ([0, 1, 2, 0] :: [Int])
      LinearRing.toVector (LinearRing.makeLinearRing 0 1 2 (Sequence.fromList [4,0]))  `shouldBe` Sequence.fromList ([0, 1, 2, 4, 0] :: [Int])


-- TODO
-- > (\xs -> safeLast (fromLinearRing xs) == Just (ringHead xs)) (xs :: LinearRing Int)
--
-- > (\x xs -> length (foldrDropLast (:) [] (x : xs)) == length xs) (x :: Int) (xs :: [Int])
--
