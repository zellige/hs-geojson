{-# LANGUAGE OverloadedStrings #-}

module Data.Geospatial.Internal.CRSTests where

import qualified Data.Aeson                   as A
import qualified Data.ByteString.Lazy.Char8   as BS
import           Test.Tasty
import           Test.Tasty.Hspec             (Spec, context, describe, it,
                                               shouldBe, testSpec)
-- Local
import           Data.Geospatial.Internal.CRS
import           Fixture


-- Tests

tests :: IO TestTree
tests = do
  specs <- specTests
  pure $ testGroup "Data.Geospatial.Internal.CRSTests" [specs]

specTests :: IO TestTree
specTests = do
  specs <- sequence
    [ testSpec "Data.Geospatial.Internal.CRS.fromJSON" testFromJSON
    , testSpec "Data.Geospatial.Internal.CRS.toJSON" testToJSON
    ]
  pure $ testGroup "Data.Geospatial.Internal.CRSTests.Spec" specs

-- Spec

-- >>> (A.decode . BS.pack) testLinkCRSJSON == Just testLinkCRS
-- True
--
-- >>> (A.decode . BS.pack) testNamedCRSJSON == Just testNamedCRS
-- True
--
-- >>> (A.decode . BS.pack) testEPSGJSON == Just testEPSG
-- True
--
-- (A.decode . BS.pack) "null" == Just NoCRS
-- True
--
testFromJSON :: Spec
testFromJSON =
  describe "fromJSON" $ do
    it "decode CRS Objects from GeoJSON" $ do
      A.decode testLinkCRSJSON  `shouldBe` Just testLinkCRS
      A.decode testNamedCRSJSON `shouldBe` Just testNamedCRS
      A.decode testEPSGJSON     `shouldBe` Just testEPSG
    context "when provided with invalid input" $
      it "fails" $
        (A.decode . BS.pack) "null" `shouldBe` Just NoCRS

-- >>> (A.decode . A.encode) testLinkCRS == Just testLinkCRS
-- True
--
-- >>> (A.decode . A.encode) testNamedCRS == Just testNamedCRS
-- True
--
-- >>> (A.decode . A.encode) testEPSG == Just testEPSG
-- True
--
-- >>> A.encode NoCRS
-- "null"
--
testToJSON :: Spec
testToJSON =
  describe "toJSON" $ do
    it "encode CRS Objects to GeoJSON" $ do
      (A.decode . A.encode) testLinkCRS  `shouldBe` Just testLinkCRS
      (A.decode . A.encode) testNamedCRS `shouldBe` Just testNamedCRS
      (A.decode . A.encode) testEPSG     `shouldBe` Just testEPSG
    context "when provided with invalid input" $
      it "fails" $
      A.encode NoCRS `shouldBe` "null"
