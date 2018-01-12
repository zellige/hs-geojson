module Data.Geospatial.Internal.GeoFeatureTests where

import qualified Data.Aeson       as A
import           Test.Tasty
import           Test.Tasty.Hspec (Spec, describe, it, shouldBe, testSpec)
-- Local
import           Fixture


-- Tests

tests :: IO TestTree
tests = do
  specs <- specTests
  pure $ testGroup "Data.Geospatial.Internal.GeoFeatureTests" [specs]

specTests :: IO TestTree
specTests = do
  specs <- sequence
    [ testSpec "Data.Geospatial.Internal.GeoFeature.fromJSON" testFromJSON
    , testSpec "Data.Geospatial.Internal.GeoFeature.toJSON" testToJSON
    ]
  pure $ testGroup "Data.Geospatial.Internal.GeoFeatureTests.Spec" specs

-- Spec

-- >>> (A.decode . BS.pack) bigFeatureJSON == Just bigFeature
-- True
--
-- >>> (A.decode . BS.pack) featureWithNoPropertiesJSON == Just featureWithNoProperties
-- True
--
-- >>> (A.decode . BS.pack) featureWithNoIdJSON == Just featureWithNoId
-- True
--
-- >>> (A.decode . BS.pack) featureWithNoBBoxJSON == Just featureWithNoBBox
-- True
--
-- >>> (A.decode . BS.pack) featureWithNoGeometryJSON == Just featureWithNoGeometry
-- True
--
testFromJSON :: Spec
testFromJSON =
  describe "fromJSON" $
    it "decode Feature Objects from GeoJSON" $ do
      A.decode bigFeatureJSON              `shouldBe` Just bigFeature
      A.decode featureWithNoPropertiesJSON `shouldBe` Just featureWithNoProperties
      A.decode featureWithNoIdJSON         `shouldBe` Just featureWithNoId
      A.decode featureWithNoBBoxJSON       `shouldBe` Just featureWithNoBBox
      A.decode featureWithNoGeometryJSON   `shouldBe` Just featureWithNoGeometry

-- >>> (A.decode . A.encode) bigFeature == Just bigFeature
-- True
--
-- >>> (A.decode . A.encode) featureWithNoProperties == Just featureWithNoProperties
-- True
--
-- >>> (A.decode . A.encode) featureWithNoId == Just featureWithNoId
-- True
--
-- >>> (A.decode . A.encode) featureWithNoBBox == Just featureWithNoBBox
-- True
--
-- >>> (A.decode . A.encode) featureWithNoGeometry == Just featureWithNoGeometry
-- True
--
testToJSON :: Spec
testToJSON =
  describe "toJSON" $
    it "encode Feature Objects to GeoJSON" $ do
      (A.decode . A.encode) bigFeature              `shouldBe` Just bigFeature
      (A.decode . A.encode) featureWithNoProperties `shouldBe` Just featureWithNoProperties
      (A.decode . A.encode) featureWithNoId         `shouldBe` Just featureWithNoId
      (A.decode . A.encode) featureWithNoBBox       `shouldBe` Just featureWithNoBBox
      (A.decode . A.encode) featureWithNoGeometry   `shouldBe` Just featureWithNoGeometry
