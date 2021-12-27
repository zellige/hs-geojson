{-# LANGUAGE OverloadedStrings #-}

module Data.Geospatial.Internal.BasicTypesTests where

-- Local
import Data.Geospatial.Internal.BasicTypes
import qualified Data.Vector.Storable as VectorStorable
import Test.Tasty
import Test.Tasty.Hspec
  ( Spec,
    describe,
    it,
    shouldBe,
    testSpec,
  )

-- Tests

tests :: IO TestTree
tests = do
  specs <- specTests
  pure $ testGroup "Data.Geospatial.Internal.BasicTypesTests" [specs]

specTests :: IO TestTree
specTests = do
  specs <-
    sequence
      [ testSpec "Data.Geospatial.Internal.BasicTypes Storable" testStorable
      ]
  pure $ testGroup "Data.Geospatial.Internal.BasicTypesTests.Spec" specs

-- Spec

empty :: GeoPositionWithoutCRS
empty = GeoEmpty

pointXy :: GeoPositionWithoutCRS
pointXy = GeoPointXY (PointXY 1.0 2.0)

pointXyz :: GeoPositionWithoutCRS
pointXyz = GeoPointXYZ (PointXYZ 3.0 4.0 5.0)

pointXyzm :: GeoPositionWithoutCRS
pointXyzm = GeoPointXYZM (PointXYZM 6.0 7.0 8.0 9.0)

items :: VectorStorable.Vector GeoPositionWithoutCRS
items = VectorStorable.fromList [empty, pointXy, pointXyz, pointXyzm]

testStorable :: Spec
testStorable =
  describe "storable" $
    it "read storable instances" $ do
      items VectorStorable.! 0 `shouldBe` empty
      items VectorStorable.! 1 `shouldBe` pointXy
      items VectorStorable.! 2 `shouldBe` pointXyz
      items VectorStorable.! 3 `shouldBe` pointXyzm
