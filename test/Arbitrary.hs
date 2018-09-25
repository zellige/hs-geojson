{-# OPTIONS_GHC -fno-warn-orphans #-}

module Arbitrary where

import           Test.Tasty.QuickCheck (Arbitrary, arbitrary)

-- Local
import           Data.LinearRing       (LinearRing, makeLinearRing)
import           Data.LineString       (LineString, makeLineString)


instance (Arbitrary a, Eq a, Show a) => Arbitrary (LinearRing a) where
  arbitrary = makeLinearRing <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Arbitrary a, Eq a, Show a) => Arbitrary (LineString a) where
  arbitrary = makeLineString <$> arbitrary <*> arbitrary <*> arbitrary
