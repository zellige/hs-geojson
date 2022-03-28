module Data.Geospatial.Internal.GeoFeatureCollectionTests where

import qualified Data.Aeson as A
-- Local
import Fixture
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

-- Tests

tests :: IO TestTree
tests = do
  specs <- specTests
  pure $ testGroup " Data.Geospatial.Internal.GeoFeatureCollectionTests" [specs]

specTests :: IO TestTree
specTests = do
  specs <-
    sequence
      [ testSpec "Data.Geospatial.Internal.GeoFeatureCollection.fromJSON" testFromJSON,
        testSpec "Data.Geospatial.Internal.GeoFeatureCollection.toJSON" testToJSON
      ]
  pure $ testGroup "Data.Geospatial.Internal.GeoFeatureCollectionTests.Spec" specs

-- Spec

-- >>> (A.decode . BS.pack) bigAssFeatureCollectionJSON == Just bigAssFeatureCollection
-- True
--
-- >>> (A.decode . BS.pack)  bigAssFeatureCollectionWithNoBBoxJSON == Just bigAssFeatureCollectionWithNoBBox
-- True
--
-- >>> (A.decode . BS.pack)  emptyFeatureCollectionWithBBoxJSON == Just emptyFeatureCollectionWithBBox
-- True
--
-- >>> (A.decode . BS.pack)  emptyFeatureCollectionJSON == Just emptyFeatureCollection
-- True
--
testFromJSON :: Spec
testFromJSON =
  describe "fromJSON" $
    it "decode FeatureCollection Objects from GeoJSON" $ do
      A.decode bigAssFeatureCollectionJSON `shouldBe` Just bigAssFeatureCollection
      A.decode bigAssFeatureCollectionWithNoBBoxJSON `shouldBe` Just bigAssFeatureCollectionWithNoBBox
      A.decode emptyFeatureCollectionWithBBoxJSON `shouldBe` Just emptyFeatureCollectionWithBBox
      A.decode emptyFeatureCollectionJSON `shouldBe` Just emptyFeatureCollection

-- >>> (A.decode . A.encode) bigAssFeatureCollection == Just bigAssFeatureCollection
-- True
--
-- >>> (A.decode . A.encode) bigAssFeatureCollectionWithNoBBox == Just bigAssFeatureCollectionWithNoBBox
-- True
--
-- >>> (A.decode . A.encode) emptyFeatureCollectionWithBBox == Just emptyFeatureCollectionWithBBox
-- True
--
-- >>> (A.decode . A.encode) emptyFeatureCollection == Just emptyFeatureCollection
-- True
--
testToJSON :: Spec
testToJSON =
  describe "toJSON" $
    it "encode FeatureCollection Objects to GeoJSON" $ do
      (A.decode . A.encode) bigAssFeatureCollection `shouldBe` Just bigAssFeatureCollection
      (A.decode . A.encode) bigAssFeatureCollectionWithNoBBox `shouldBe` Just bigAssFeatureCollectionWithNoBBox
      (A.decode . A.encode) emptyFeatureCollectionWithBBox `shouldBe` Just emptyFeatureCollectionWithBBox
      (A.decode . A.encode) emptyFeatureCollection `shouldBe` Just emptyFeatureCollection
