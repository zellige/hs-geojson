module Data.LinearRingTests where

import           Data.Foldable         (Foldable (..))
import           Data.List.NonEmpty    (NonEmpty (..))
import           Data.Validation       (AccValidation (..))
import           Test.Tasty
import           Test.Tasty.Hspec      (Spec, context, describe, it, shouldBe,
                                        testSpec)
import           Test.Tasty.QuickCheck (Property, property, testProperty)
-- Local
import           Arbitrary             ()
import           Data.LinearRing


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
    ]
  pure $ testGroup "Data.LinearRingTests.Spec" specs

-- QuickCheck

-- > (\xs -> ringLength xs == (length (fromLinearRing xs))) (xs :: LinearRing Int)
--
testRingLength :: LinearRing Int -> Property
testRingLength xs = property $ ringLength xs == length (fromLinearRing xs)

-- > (\xs -> length (fromLinearRing xs) >= 4) (xs :: LinearRing Int)
--
testFromLinearRing :: LinearRing Int -> Property
testFromLinearRing xs = property $ length (fromLinearRing xs) >= 4

-- > (\xs -> (foldr (:) [] xs) == (fromLinearRing xs)) (xs :: LinearRing Int)
--
-- > (\xs -> (ringHead xs) == (foldr'' (\a -> const a) 0 xs)) (xs :: LinearRing Int)
--
testFoldable :: LinearRing Int -> Property
testFoldable xs = property $ (foldr (:) [] xs == fromLinearRing xs) && (ringHead xs == foldr' const 0 xs)

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
      fromList ([0, 1, 2, 3] :: [Int])       `shouldBe` AccSuccess (makeLinearRing 0 1 2 [])
      fromList ([0, 1, 2, 4, 0] :: [Int])    `shouldBe` AccSuccess (makeLinearRing 0 1 2 [4])
      fromList ([0, 1, 2, 4, 5, 0] :: [Int]) `shouldBe` AccSuccess (makeLinearRing 0 1 2 [4, 5])
      fromList ([0, 1, 2, 4, 5, 6] :: [Int]) `shouldBe` AccSuccess (makeLinearRing 0 1 2 [4, 5])
    context "when provided with invalid input" $
      it "fails" $ do
        fromList []        `shouldBe` AccFailure (ListTooShort 0 :| [] :: NonEmpty (ListToLinearRingError Int))
        fromList [0]       `shouldBe` AccFailure (ListTooShort 1 :| [] :: NonEmpty (ListToLinearRingError Int))
        fromList [0, 1]    `shouldBe` AccFailure (ListTooShort 2 :| [] :: NonEmpty (ListToLinearRingError Int))
        fromList [0, 1, 2] `shouldBe` AccFailure (ListTooShort 3 :| [] :: NonEmpty (ListToLinearRingError Int))

-- TODO
-- > (\xs -> safeLast (fromLinearRing xs) == Just (ringHead xs)) (xs :: LinearRing Int)
--
-- > (\x xs -> length (foldrDropLast (:) [] (x : xs)) == length xs) (x :: Int) (xs :: [Int])
--
