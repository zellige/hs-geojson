{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
-------------------------------------------------------------------
-- |
-- Module       : Data.Geospatial.GeoFeature
-- Copyright    : (C) 2014 Dom De Re
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-- See Section 2.2 /Feature Objects/ of the GeoJSON spec.
-- Parameterised on the property type
--
-------------------------------------------------------------------
module Data.Geospatial.GeoFeature (
    -- * Types
        GeoFeature(..)
    -- * Lenses
    ,   bbox
    ,   geometry
    ,   properties
    ,   featureId
    ) where

import Data.Geospatial.BasicTypes
import Data.Geospatial.Geometry
import Data.Geospatial.Geometry.JSON
import Data.Geospatial.GeoPosition

import Control.Applicative ( (<$>), (<*>) )
import Control.Lens ( makeLenses )
import Control.Monad ( mzero )
import Data.Aeson ( FromJSON(..), ToJSON(..), Value(..), Object, (.:) )
import Data.Text ( Text )
import Text.JSON ( JSON(..), makeObj, valFromObj )

-- $setup
--
-- >>> import qualified Data.Aeson as A
-- >>> import qualified Data.ByteString.Lazy.Char8 as BS
-- >>> import qualified Text.JSON as J
--
-- Test Bounding Box Data
-- >>> let lshapedPolyVertices = [[120.0, -15.0], [127.0, -15.0], [127.0, -25.0], [124.0, -25.0], [124.0, -18.0], [120.0, -18.0]] :: [GeoPositionWithoutCRS]
-- >>> let emptyVertices = [] :: [GeoPositionWithoutCRS]
-- >>> let testLatLonBBox = [-32, 147.5, -29.5, 151.0] :: BoundingBoxWithoutCRS
-- >>> let testLatLonBBoxJSON = "[-32,147.5,-29.5,151]"
-- >>> let testEmptyBBox = [] :: BoundingBoxWithoutCRS
-- >>> let testEmptyBBoxJSON = "[]"
--
-- End Test Bounding Box Data
-- Test Geometry Data
-- Polys
-- >>> let lShapedPolyJSON = "{\"type\":\"Polygon\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]}"
--
-- Upside down L Shaped Poly
--
-- (120, -15)                (127, -15)
-- *---------------------------*
-- |                           |
-- |                           |
-- |             (124, -18)    |
-- *---------------*           |
-- (120, -18)      |           |
--                 |           |
--                 |           |
--                 |           |
--                 |           |
--                 |           |
--                 |           |
--                 *-----------*
--               (124, -25)  (127, -25)
--
-- >>> let lShapedGeoPoly = GeoPolygon lshapedPolyVertices
-- >>> let lShapedPoly = Polygon lShapedGeoPoly
-- >>> let emptyPolyJSON = "{\"type\":\"Polygon\",\"coordinates\":[]}"
-- >>> let emptyGeoPoly = GeoPolygon emptyVertices
-- >>> let emptyPoly = Polygon emptyGeoPoly
--
-- Multi Polys
-- >>> let emptyMultiPolyJSON = "{\"type\":\"MultiPolygon\",\"coordinates\":[]}"
-- >>> let emptyMultiGeoPoly = GeoMultiPolygon []
-- >>> let emptyMultiPoly = MultiPolygon emptyMultiGeoPoly
-- >>> let singlePolyMultiPolyJSON = "{\"type\":\"MultiPolygon\",\"coordinates\":[{\"type\":\"Polygon\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]}]}"
-- >>> let singlePolyGeoMultiPoly = GeoMultiPolygon [lShapedGeoPoly]
-- >>> let singlePolyMultiPoly = MultiPolygon singlePolyGeoMultiPoly
-- >>> let multiPolyJSON = "{\"type\":\"MultiPolygon\",\"coordinates\":[{\"type\":\"Polygon\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]},{\"type\":\"Polygon\",\"coordinates\":[]}]}"
-- >>> let geoMultiPoly = GeoMultiPolygon [lShapedGeoPoly, emptyGeoPoly]
-- >>> let multiPoly = MultiPolygon geoMultiPoly
--
-- Line Data
-- >>> let lShapedLineJSON = "{\"type\":\"Line\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]}"
-- >>> let lShapedGeoLine = GeoLine lshapedPolyVertices
-- >>> let lShapedLine = Line lShapedGeoLine
-- >>> let emptyLineJSON = "{\"type\":\"Line\",\"coordinates\":[]}"
-- >>> let emptyGeoLine = GeoLine emptyVertices
-- >>> let emptyLine = Line emptyGeoLine
--
-- Multi Lines
-- >>> let emptyMultiLineJSON = "{\"type\":\"MultiLine\",\"coordinates\":[]}"
-- >>> let emptyMultiGeoLine = GeoMultiLine []
-- >>> let emptyMultiLine = MultiLine emptyMultiGeoLine
-- >>> let singleLineMultiLineJSON = "{\"type\":\"MultiLine\",\"coordinates\":[{\"type\":\"Line\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]}]}"
-- >>> let singleLineGeoMultiLine = GeoMultiLine [lShapedGeoLine]
-- >>> let singleLineMultiLine = MultiLine singleLineGeoMultiLine
-- >>> let multiLineJSON = "{\"type\":\"MultiLine\",\"coordinates\":[{\"type\":\"Line\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]},{\"type\":\"Line\",\"coordinates\":[]}]}"
-- >>> let geoMultiLine = GeoMultiLine [lShapedGeoLine, emptyGeoLine]
-- >>> let multiLine = MultiLine geoMultiLine
-- >>> let emptyCollectionJSON = "{\"type\":\"GeometryCollection\",\"geometries\":[]}"
-- >>> let emptyCollection = Collection []
-- >>> let bigassCollectionJSON = "{\"type\":\"GeometryCollection\",\"geometries\":[{\"type\":\"MultiLine\",\"coordinates\":[{\"type\":\"Line\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]}]},{\"type\":\"MultiLine\",\"coordinates\":[]},{\"type\":\"Line\",\"coordinates\":[]},{\"type\":\"MultiLine\",\"coordinates\":[{\"type\":\"Line\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]},{\"type\":\"Line\",\"coordinates\":[]}]},{\"type\":\"Line\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]},{\"type\":\"MultiPolygon\",\"coordinates\":[{\"type\":\"Polygon\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]},{\"type\":\"Polygon\",\"coordinates\":[]}]},{\"type\":\"MultiPolygon\",\"coordinates\":[{\"type\":\"Polygon\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]}]},{\"type\":\"Polygon\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]},{\"type\":\"MultiPolygon\",\"coordinates\":[]},{\"type\":\"Polygon\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]}]}"
-- >>> let bigassCollection = Collection [singleLineMultiLine, emptyMultiLine, emptyLine, multiLine, lShapedLine, multiPoly, singlePolyMultiPoly, lShapedPoly, emptyMultiPoly, lShapedPoly]
--
-- End Test Geometry Data
--
-- Test Properties
-- >>> let testProperties = makeObj [("depth", showJSON (5 :: Int)), ("comment", showJSON "Bore run over by dump truck")]
--
-- Test Features
-- >>> let bigFeatureJSON = "{\"type\":\"Feature\",\"properties\":{\"depth\":5,\"comment\":\"Bore run over by dump truck\"},\"geometry\":{\"type\":\"GeometryCollection\",\"geometries\":[{\"type\":\"MultiLine\",\"coordinates\":[{\"type\":\"Line\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]}]},{\"type\":\"MultiLine\",\"coordinates\":[]},{\"type\":\"Line\",\"coordinates\":[]},{\"type\":\"MultiLine\",\"coordinates\":[{\"type\":\"Line\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]},{\"type\":\"Line\",\"coordinates\":[]}]},{\"type\":\"Line\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]},{\"type\":\"MultiPolygon\",\"coordinates\":[{\"type\":\"Polygon\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]},{\"type\":\"Polygon\",\"coordinates\":[]}]},{\"type\":\"MultiPolygon\",\"coordinates\":[{\"type\":\"Polygon\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]}]},{\"type\":\"Polygon\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]},{\"type\":\"MultiPolygon\",\"coordinates\":[]},{\"type\":\"Polygon\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]}]},\"bbox\":[-32,147.5,-29.5,151],\"id\":\"GW001\"}"
-- >>> let bigFeature = GeoFeature (Just testLatLonBBox) bigassCollection testProperties (Just "GW001")
-- >>> let featureWithNoPropertiesJSON = "{\"type\":\"Feature\",\"properties\":null,\"geometry\":{\"type\":\"GeometryCollection\",\"geometries\":[{\"type\":\"MultiLine\",\"coordinates\":[{\"type\":\"Line\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]}]},{\"type\":\"MultiLine\",\"coordinates\":[]},{\"type\":\"Line\",\"coordinates\":[]},{\"type\":\"MultiLine\",\"coordinates\":[{\"type\":\"Line\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]},{\"type\":\"Line\",\"coordinates\":[]}]},{\"type\":\"Line\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]},{\"type\":\"MultiPolygon\",\"coordinates\":[{\"type\":\"Polygon\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]},{\"type\":\"Polygon\",\"coordinates\":[]}]},{\"type\":\"MultiPolygon\",\"coordinates\":[{\"type\":\"Polygon\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]}]},{\"type\":\"Polygon\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]},{\"type\":\"MultiPolygon\",\"coordinates\":[]},{\"type\":\"Polygon\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]}]},\"bbox\":[-32,147.5,-29.5,151],\"id\":\"GW001\"}"
-- >>> let featureWithNoProperties = let GeoFeature bbox geometry _ featureId = bigFeature in GeoFeature bbox geometry JSNull featureId
-- >>> let featureWithNoGeometryJSON = "{\"type\":\"Feature\",\"properties\":{\"depth\":5,\"comment\":\"Bore run over by dump truck\"},\"geometry\":null,\"bbox\":[-32,147.5,-29.5,151],\"id\":\"GW001\"}"
-- >>> let featureWithNoGeometry = let GeoFeature bbox _ props featureId = bigFeature in GeoFeature bbox NoGeometry props featureId
-- >>> let featureWithNoIdJSON = "{\"type\":\"Feature\",\"properties\":{\"depth\":5,\"comment\":\"Bore run over by dump truck\"},\"geometry\":{\"type\":\"GeometryCollection\",\"geometries\":[{\"type\":\"MultiLine\",\"coordinates\":[{\"type\":\"Line\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]}]},{\"type\":\"MultiLine\",\"coordinates\":[]},{\"type\":\"Line\",\"coordinates\":[]},{\"type\":\"MultiLine\",\"coordinates\":[{\"type\":\"Line\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]},{\"type\":\"Line\",\"coordinates\":[]}]},{\"type\":\"Line\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]},{\"type\":\"MultiPolygon\",\"coordinates\":[{\"type\":\"Polygon\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]},{\"type\":\"Polygon\",\"coordinates\":[]}]},{\"type\":\"MultiPolygon\",\"coordinates\":[{\"type\":\"Polygon\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]}]},{\"type\":\"Polygon\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]},{\"type\":\"MultiPolygon\",\"coordinates\":[]},{\"type\":\"Polygon\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]}]},\"bbox\":[-32,147.5,-29.5,151]}"
-- >>> let featureWithNoId = let GeoFeature bbox geometry props _ = bigFeature in GeoFeature bbox geometry props Nothing
-- >>> let featureWithNoBBoxJSON = "{\"type\":\"Feature\",\"properties\":{\"depth\":5,\"comment\":\"Bore run over by dump truck\"},\"geometry\":{\"type\":\"GeometryCollection\",\"geometries\":[{\"type\":\"MultiLine\",\"coordinates\":[{\"type\":\"Line\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]}]},{\"type\":\"MultiLine\",\"coordinates\":[]},{\"type\":\"Line\",\"coordinates\":[]},{\"type\":\"MultiLine\",\"coordinates\":[{\"type\":\"Line\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]},{\"type\":\"Line\",\"coordinates\":[]}]},{\"type\":\"Line\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]},{\"type\":\"MultiPolygon\",\"coordinates\":[{\"type\":\"Polygon\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]},{\"type\":\"Polygon\",\"coordinates\":[]}]},{\"type\":\"MultiPolygon\",\"coordinates\":[{\"type\":\"Polygon\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]}]},{\"type\":\"Polygon\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]},{\"type\":\"MultiPolygon\",\"coordinates\":[]},{\"type\":\"Polygon\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]}]},\"id\":\"GW001\"}"
-- >>> let featureWithNoBBox = let GeoFeature _ geometry props featureId = bigFeature in GeoFeature Nothing geometry props featureId
-- >>> let bigAssFeatureCollectionJSON = "{\"type\":\"FeatureCollection\",\"features\":[{\"type\":\"Feature\",\"properties\":{\"depth\":5,\"comment\":\"Bore run over by dump truck\"},\"geometry\":{\"type\":\"GeometryCollection\",\"geometries\":[{\"type\":\"MultiLine\",\"coordinates\":[{\"type\":\"Line\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]}]},{\"type\":\"MultiLine\",\"coordinates\":[]},{\"type\":\"Line\",\"coordinates\":[]},{\"type\":\"MultiLine\",\"coordinates\":[{\"type\":\"Line\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]},{\"type\":\"Line\",\"coordinates\":[]}]},{\"type\":\"Line\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]},{\"type\":\"MultiPolygon\",\"coordinates\":[{\"type\":\"Polygon\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]},{\"type\":\"Polygon\",\"coordinates\":[]}]},{\"type\":\"MultiPolygon\",\"coordinates\":[{\"type\":\"Polygon\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]}]},{\"type\":\"Polygon\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]},{\"type\":\"MultiPolygon\",\"coordinates\":[]},{\"type\":\"Polygon\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]}]},\"id\":\"GW001\"},{\"type\":\"Feature\",\"properties\":{\"depth\":5,\"comment\":\"Bore run over by dump truck\"},\"geometry\":null,\"bbox\":[-32,147.5,-29.5,151],\"id\":\"GW001\"},{\"type\":\"Feature\",\"properties\":{\"depth\":5,\"comment\":\"Bore run over by dump truck\"},\"geometry\":{\"type\":\"GeometryCollection\",\"geometries\":[{\"type\":\"MultiLine\",\"coordinates\":[{\"type\":\"Line\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]}]},{\"type\":\"MultiLine\",\"coordinates\":[]},{\"type\":\"Line\",\"coordinates\":[]},{\"type\":\"MultiLine\",\"coordinates\":[{\"type\":\"Line\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]},{\"type\":\"Line\",\"coordinates\":[]}]},{\"type\":\"Line\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]},{\"type\":\"MultiPolygon\",\"coordinates\":[{\"type\":\"Polygon\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]},{\"type\":\"Polygon\",\"coordinates\":[]}]},{\"type\":\"MultiPolygon\",\"coordinates\":[{\"type\":\"Polygon\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]}]},{\"type\":\"Polygon\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]},{\"type\":\"MultiPolygon\",\"coordinates\":[]},{\"type\":\"Polygon\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]}]},\"id\":\"GW001\"},{\"type\":\"Feature\",\"properties\":{\"depth\":5,\"comment\":\"Bore run over by dump truck\"},\"geometry\":{\"type\":\"GeometryCollection\",\"geometries\":[{\"type\":\"MultiLine\",\"coordinates\":[{\"type\":\"Line\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]}]},{\"type\":\"MultiLine\",\"coordinates\":[]},{\"type\":\"Line\",\"coordinates\":[]},{\"type\":\"MultiLine\",\"coordinates\":[{\"type\":\"Line\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]},{\"type\":\"Line\",\"coordinates\":[]}]},{\"type\":\"Line\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]},{\"type\":\"MultiPolygon\",\"coordinates\":[{\"type\":\"Polygon\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]},{\"type\":\"Polygon\",\"coordinates\":[]}]},{\"type\":\"MultiPolygon\",\"coordinates\":[{\"type\":\"Polygon\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]}]},{\"type\":\"Polygon\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]},{\"type\":\"MultiPolygon\",\"coordinates\":[]},{\"type\":\"Polygon\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]}]},\"bbox\":[-32,147.5,-29.5,151]},{\"type\":\"Feature\",\"properties\":null,\"geometry\":{\"type\":\"GeometryCollection\",\"geometries\":[{\"type\":\"MultiLine\",\"coordinates\":[{\"type\":\"Line\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]}]},{\"type\":\"MultiLine\",\"coordinates\":[]},{\"type\":\"Line\",\"coordinates\":[]},{\"type\":\"MultiLine\",\"coordinates\":[{\"type\":\"Line\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]},{\"type\":\"Line\",\"coordinates\":[]}]},{\"type\":\"Line\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]},{\"type\":\"MultiPolygon\",\"coordinates\":[{\"type\":\"Polygon\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]},{\"type\":\"Polygon\",\"coordinates\":[]}]},{\"type\":\"MultiPolygon\",\"coordinates\":[{\"type\":\"Polygon\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]}]},{\"type\":\"Polygon\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]},{\"type\":\"MultiPolygon\",\"coordinates\":[]},{\"type\":\"Polygon\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]}]},\"bbox\":[-32,147.5,-29.5,151],\"id\":\"GW001\"},{\"type\":\"Feature\",\"properties\":{\"depth\":5,\"comment\":\"Bore run over by dump truck\"},\"geometry\":{\"type\":\"GeometryCollection\",\"geometries\":[{\"type\":\"MultiLine\",\"coordinates\":[{\"type\":\"Line\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]}]},{\"type\":\"MultiLine\",\"coordinates\":[]},{\"type\":\"Line\",\"coordinates\":[]},{\"type\":\"MultiLine\",\"coordinates\":[{\"type\":\"Line\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]},{\"type\":\"Line\",\"coordinates\":[]}]},{\"type\":\"Line\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]},{\"type\":\"MultiPolygon\",\"coordinates\":[{\"type\":\"Polygon\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]},{\"type\":\"Polygon\",\"coordinates\":[]}]},{\"type\":\"MultiPolygon\",\"coordinates\":[{\"type\":\"Polygon\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]}]},{\"type\":\"Polygon\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]},{\"type\":\"MultiPolygon\",\"coordinates\":[]},{\"type\":\"Polygon\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]}]},\"bbox\":[-32,147.5,-29.5,151],\"id\":\"GW001\"}],\"bbox\":[-32,147.5,-29.5,151]}"
-- >>> let bigAssFeatureCollection = GeoFeatureCollection (Just testLatLonBBox) [featureWithNoBBox, featureWithNoGeometry, featureWithNoBBox, featureWithNoId, featureWithNoProperties, bigFeature]
-- >>> let bigAssFeatureCollectionWithNoBBoxJSON = "{\"type\":\"FeatureCollection\",\"features\":[{\"type\":\"Feature\",\"properties\":{\"depth\":5,\"comment\":\"Bore run over by dump truck\"},\"geometry\":{\"type\":\"GeometryCollection\",\"geometries\":[{\"type\":\"MultiLine\",\"coordinates\":[{\"type\":\"Line\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]}]},{\"type\":\"MultiLine\",\"coordinates\":[]},{\"type\":\"Line\",\"coordinates\":[]},{\"type\":\"MultiLine\",\"coordinates\":[{\"type\":\"Line\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]},{\"type\":\"Line\",\"coordinates\":[]}]},{\"type\":\"Line\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]},{\"type\":\"MultiPolygon\",\"coordinates\":[{\"type\":\"Polygon\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]},{\"type\":\"Polygon\",\"coordinates\":[]}]},{\"type\":\"MultiPolygon\",\"coordinates\":[{\"type\":\"Polygon\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]}]},{\"type\":\"Polygon\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]},{\"type\":\"MultiPolygon\",\"coordinates\":[]},{\"type\":\"Polygon\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]}]},\"id\":\"GW001\"},{\"type\":\"Feature\",\"properties\":{\"depth\":5,\"comment\":\"Bore run over by dump truck\"},\"geometry\":null,\"bbox\":[-32,147.5,-29.5,151],\"id\":\"GW001\"},{\"type\":\"Feature\",\"properties\":{\"depth\":5,\"comment\":\"Bore run over by dump truck\"},\"geometry\":{\"type\":\"GeometryCollection\",\"geometries\":[{\"type\":\"MultiLine\",\"coordinates\":[{\"type\":\"Line\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]}]},{\"type\":\"MultiLine\",\"coordinates\":[]},{\"type\":\"Line\",\"coordinates\":[]},{\"type\":\"MultiLine\",\"coordinates\":[{\"type\":\"Line\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]},{\"type\":\"Line\",\"coordinates\":[]}]},{\"type\":\"Line\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]},{\"type\":\"MultiPolygon\",\"coordinates\":[{\"type\":\"Polygon\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]},{\"type\":\"Polygon\",\"coordinates\":[]}]},{\"type\":\"MultiPolygon\",\"coordinates\":[{\"type\":\"Polygon\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]}]},{\"type\":\"Polygon\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]},{\"type\":\"MultiPolygon\",\"coordinates\":[]},{\"type\":\"Polygon\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]}]},\"id\":\"GW001\"},{\"type\":\"Feature\",\"properties\":{\"depth\":5,\"comment\":\"Bore run over by dump truck\"},\"geometry\":{\"type\":\"GeometryCollection\",\"geometries\":[{\"type\":\"MultiLine\",\"coordinates\":[{\"type\":\"Line\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]}]},{\"type\":\"MultiLine\",\"coordinates\":[]},{\"type\":\"Line\",\"coordinates\":[]},{\"type\":\"MultiLine\",\"coordinates\":[{\"type\":\"Line\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]},{\"type\":\"Line\",\"coordinates\":[]}]},{\"type\":\"Line\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]},{\"type\":\"MultiPolygon\",\"coordinates\":[{\"type\":\"Polygon\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]},{\"type\":\"Polygon\",\"coordinates\":[]}]},{\"type\":\"MultiPolygon\",\"coordinates\":[{\"type\":\"Polygon\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]}]},{\"type\":\"Polygon\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]},{\"type\":\"MultiPolygon\",\"coordinates\":[]},{\"type\":\"Polygon\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]}]},\"bbox\":[-32,147.5,-29.5,151]},{\"type\":\"Feature\",\"properties\":null,\"geometry\":{\"type\":\"GeometryCollection\",\"geometries\":[{\"type\":\"MultiLine\",\"coordinates\":[{\"type\":\"Line\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]}]},{\"type\":\"MultiLine\",\"coordinates\":[]},{\"type\":\"Line\",\"coordinates\":[]},{\"type\":\"MultiLine\",\"coordinates\":[{\"type\":\"Line\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]},{\"type\":\"Line\",\"coordinates\":[]}]},{\"type\":\"Line\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]},{\"type\":\"MultiPolygon\",\"coordinates\":[{\"type\":\"Polygon\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]},{\"type\":\"Polygon\",\"coordinates\":[]}]},{\"type\":\"MultiPolygon\",\"coordinates\":[{\"type\":\"Polygon\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]}]},{\"type\":\"Polygon\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]},{\"type\":\"MultiPolygon\",\"coordinates\":[]},{\"type\":\"Polygon\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]}]},\"bbox\":[-32,147.5,-29.5,151],\"id\":\"GW001\"},{\"type\":\"Feature\",\"properties\":{\"depth\":5,\"comment\":\"Bore run over by dump truck\"},\"geometry\":{\"type\":\"GeometryCollection\",\"geometries\":[{\"type\":\"MultiLine\",\"coordinates\":[{\"type\":\"Line\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]}]},{\"type\":\"MultiLine\",\"coordinates\":[]},{\"type\":\"Line\",\"coordinates\":[]},{\"type\":\"MultiLine\",\"coordinates\":[{\"type\":\"Line\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]},{\"type\":\"Line\",\"coordinates\":[]}]},{\"type\":\"Line\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]},{\"type\":\"MultiPolygon\",\"coordinates\":[{\"type\":\"Polygon\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]},{\"type\":\"Polygon\",\"coordinates\":[]}]},{\"type\":\"MultiPolygon\",\"coordinates\":[{\"type\":\"Polygon\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]}]},{\"type\":\"Polygon\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]},{\"type\":\"MultiPolygon\",\"coordinates\":[]},{\"type\":\"Polygon\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]}]},\"bbox\":[-32,147.5,-29.5,151],\"id\":\"GW001\"}]}"
-- >>> let bigAssFeatureCollectionWithNoBBox = let GeoFeatureCollection _ features = bigAssFeatureCollection in GeoFeatureCollection Nothing features
-- >>> let emptyFeatureCollectionJSON = "{\"type\":\"FeatureCollection\",\"features\":[]}"
-- >>> let emptyFeatureCollection = GeoFeatureCollection Nothing []
-- >>> let emptyFeatureCollectionWithBBoxJSON = "{\"type\":\"FeatureCollection\",\"features\":[],\"bbox\":[-32,147.5,-29.5,151]}"
-- >>> let emptyFeatureCollectionWithBBox = GeoFeatureCollection (Just testLatLonBBox) []
--

-- | See Section 2.2 /Feature Objects/ of the GeoJSON spec.
-- Parameterised on the property type
data GeoFeature a = GeoFeature {
    _bbox :: Maybe BoundingBoxWithoutCRS,
    _geometry :: GeospatialGeometry,
    _properties :: a,
    _featureId :: Maybe FeatureID } deriving (Show, Eq)



makeLenses ''GeoFeature

-- instances

-- | encodes and decodes Feature objects to and from GeoJSON
--
-- >>> encode bigFeature == bigFeatureJSON
-- True
--
-- >>> decode bigFeatureJSON == Ok bigFeature
-- True
--
-- >>> encode featureWithNoProperties == featureWithNoPropertiesJSON
-- True
--
-- >>> decode featureWithNoPropertiesJSON == Ok featureWithNoProperties
-- True
--
-- >>> encode featureWithNoId == featureWithNoIdJSON
-- True
--
-- >>> decode featureWithNoIdJSON == Ok featureWithNoId
-- True
--
-- >>> encode featureWithNoBBox == featureWithNoBBoxJSON
-- True
--
-- >>> decode featureWithNoBBoxJSON == Ok featureWithNoBBox
-- True
--
-- >>> encode featureWithNoGeometry == featureWithNoGeometryJSON
-- True
--
-- >>> decode featureWithNoGeometryJSON == Ok featureWithNoGeometry
-- True
--
instance (JSON a) => JSON (GeoFeature a) where
    readJSON json = do
        obj <- readJSON json
        objType <- valFromObj "type" obj
        if objType /= "Feature"
            then
                fail $ "Invalid GeoJSON type for a Feature: " ++ objType
            else
                GeoFeature <$> optValFromObj "bbox" obj
                    <*> valFromObj "geometry" obj
                    <*> valFromObj "properties" obj
                    <*> optValFromObj "id" obj

    showJSON (GeoFeature bbox' geom props featureId') = makeObj $ baseAttributes ++ optAttributes "bbox" bbox' ++ optAttributes "id" featureId'
        where
            baseAttributes = [("type", showJSON ("Feature" :: Text)), ("properties", showJSON props), ("geometry", showJSON geom)]


instance (FromJSON a) => FromJSON (GeoFeature a) where
--  parseJSON :: Value -> Parse a
    parseJSON (Object obj) = do
        objType <- obj .: ("type" :: Text)
        if objType /= ("Feature" :: Text)
            then
                mzero
            else
                GeoFeature
                    <$> obj .: ("bbox" :: Text)
                    <*> obj .: ("geometry" :: Text)
                    <*> obj .: ("properties" :: Text)
                    <*> obj .: ("id" :: Text)
