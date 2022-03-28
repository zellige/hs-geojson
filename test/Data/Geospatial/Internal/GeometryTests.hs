{-# LANGUAGE OverloadedStrings #-}

module Data.Geospatial.Internal.GeometryTests where

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as BS
-- Local
import Data.Geospatial.Internal.Geometry
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
  pure $ testGroup "Data.Geospatial.Internal.GeometryTests" [specs]

specTests :: IO TestTree
specTests = do
  specs <-
    sequence
      [ testSpec "Data.Geospatial.Internal.Geometry.fromJSON" testFromJSON,
        testSpec "Data.Geospatial.Internal.Geometry.toJSON" testToJSON
      ]
  pure $ testGroup "Data.Geospatial.Internal.GeometryTests.Spec" specs

-- Spec

-- >>> decode' lShapedPolyJSON == Just lShapedPoly
-- True
--
-- >>> decode' emptyPolyJSON == Just emptyPoly
-- True
--
-- >>> decode' emptyMultiPolyJSON == Just emptyMultiPoly
-- True
--
-- >>> decode' singleLineMultiLineJSON == Just singleLineMultiLine
-- True
--
-- >>> decode' multiLineJSON == Just multiLine
-- True
--
-- >>> decode' emptyCollectionJSON == Just emptyCollection
-- True
--
-- >>> decode' bigassCollectionJSON == Just bigassCollection
-- True

--
-- decode' "null" :: Maybe GeospatialGeometry
-- Just NoGeometry
--
testFromJSON :: Spec
testFromJSON =
  describe "fromJSON" $
    it "decode Feature Objects from GeoJSON" $ do
      A.decode lShapedPolyJSON `shouldBe` Just lShapedPoly
      A.decode emptyPolyJSON `shouldBe` Just emptyPoly
      A.decode emptyMultiPolyJSON `shouldBe` Just emptyMultiPoly
      A.decode singleLineMultiLineJSON `shouldBe` Just singleLineMultiLine
      A.decode multiLineJSON `shouldBe` Just multiLine
      A.decode emptyCollectionJSON `shouldBe` Just emptyCollection
      A.decode bigassCollectionJSON `shouldBe` Just bigassCollection
      (A.decode . BS.pack) "null" `shouldBe` Just NoGeometry

-- >>> A.encode NoGeometry
-- "null"
--
-- >>> (A.decode . A.encode) lShapedPoly == Just lShapedPoly
-- True
--
-- >>> (A.decode . A.encode) emptyPoly == Just emptyPoly
-- True
--
-- >>> (A.decode . A.encode) emptyMultiPoly == Just emptyMultiPoly
-- True
--
-- >>> (A.decode . A.encode) singleLineMultiLine == Just singleLineMultiLine
-- True
--
-- >>> (A.decode . A.encode) multiLine == Just multiLine
-- True
--
-- >>> (A.decode . A.encode) emptyCollection == Just emptyCollection
-- True
--
-- >>> (A.decode . A.encode) bigassCollection == Just bigassCollection
-- True
--
testToJSON :: Spec
testToJSON =
  describe "toJSON" $
    it "encode Feature Objects to GeoJSON" $ do
      A.encode NoGeometry `shouldBe` "null"
      (A.decode . A.encode) lShapedPoly `shouldBe` Just lShapedPoly
      (A.decode . A.encode) emptyPoly `shouldBe` Just emptyPoly
      (A.decode . A.encode) emptyMultiPoly `shouldBe` Just emptyMultiPoly
      (A.decode . A.encode) singleLineMultiLine `shouldBe` Just singleLineMultiLine
      (A.decode . A.encode) multiLine `shouldBe` Just multiLine
      (A.decode . A.encode) emptyCollection `shouldBe` Just emptyCollection
      (A.decode . A.encode) bigassCollection `shouldBe` Just bigassCollection
