module Main (main) where

-- Local
import qualified Data.Geospatial.Internal.CRSTests as CRS
import qualified Data.Geospatial.Internal.GeoFeatureCollectionTests as FC
import qualified Data.Geospatial.Internal.GeoFeatureTests as F
import qualified Data.Geospatial.Internal.GeometryTests as G
import qualified Data.LineStringTests as LS
import qualified Data.LinearRingTests as LR
import qualified Data.SeqHelperTests as SH
import Test.Tasty

main :: IO ()
main = do
  tests <-
    sequence
      [ LR.tests,
        LS.tests,
        SH.tests,
        CRS.tests,
        FC.tests,
        F.tests,
        G.tests
      ]
  defaultMain (testGroup "tests" tests)
