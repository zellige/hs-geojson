{-# OPTIONS_GHC -fno-warn-orphans #-}

module Arbitrary where

import qualified Data.Vector           as Vector
import qualified Data.Vector.Storable  as VectorStorable
import           Test.Tasty.QuickCheck (Arbitrary, arbitrary)

-- Local
import           Data.LinearRing       (LinearRing, makeLinearRing)
import           Data.LineString       (LineString, makeLineString)


instance (Arbitrary a, Eq a, Show a) => Arbitrary (LinearRing a) where
  arbitrary = makeLinearRing <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Arbitrary a, Eq a, Show a, VectorStorable.Storable a) => Arbitrary (LineString a) where
  arbitrary = makeLineString <$> arbitrary <*> arbitrary <*> arbitrary

instance (Arbitrary a, VectorStorable.Storable a) => Arbitrary (VectorStorable.Vector a) where
  arbitrary = fmap VectorStorable.fromList arbitrary

instance (Arbitrary a) => Arbitrary (Vector.Vector a) where
  arbitrary = fmap Vector.fromList arbitrary
