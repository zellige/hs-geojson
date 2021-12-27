{-# OPTIONS_GHC -fno-warn-orphans #-}

module Arbitrary where

-- Local

import Data.LineString (LineString, makeLineString)
import Data.LinearRing (LinearRing, makeLinearRing)
import Test.Tasty.QuickCheck (Arbitrary, arbitrary)

instance (Arbitrary a, Eq a, Show a) => Arbitrary (LinearRing a) where
  arbitrary = makeLinearRing <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Arbitrary a, Eq a, Show a) => Arbitrary (LineString a) where
  arbitrary = makeLineString <$> arbitrary <*> arbitrary <*> arbitrary
