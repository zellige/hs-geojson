{-# OPTIONS_GHC -fno-warn-orphans #-}
module Text.GeoJSON () where

import Data.Functor ( Functor(..) )
import Control.Applicative
import Control.Lens ( makeLenses )
import Text.JSON

import Data.Geospatial
import Data.Geospatial.Geometry.JSON

-- | This module only exports the JSON Instances for the types from Data.Geospatial.
-- .
-- Hence you will have to refer to the `Data.Geospatial` documentation to find the documentation
-- related to the Instances contained in this module.

-- $setup
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
-- Test CRS Data
-- >>> let testLinkCRSJSON = "{\"type\":\"link\",\"properties\":{\"href\":\"www.google.com.au\",\"type\":\"proj4\"}}"
-- >>> let testLinkCRS = LinkedCRS "www.google.com.au" "proj4"
-- >>> let testEPSGJSON = "{\"type\":\"epsg\",\"properties\":{\"code\":4326}}"
-- >>> let testEPSG = EPSG 4326
-- >>> let testNamedCRSJSON = "{\"type\":\"name\",\"properties\":{\"name\":\"urn:ogc:def:crs:OGC:1.3:CRS84\"}}"
-- >>> let testNamedCRS = NamedCRS "urn:ogc:def:crs:OGC:1.3:CRS84"
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



-- helper functions:

geometryFromJSON :: String -> JSValue -> Result GeospatialGeometry
geometryFromJSON "Point" obj                                = Point <$> readJSON obj
geometryFromJSON "MultiPoint" obj                           = MultiPoint <$> readJSON obj
geometryFromJSON "Polygon" obj                              = Polygon <$> readJSON obj
geometryFromJSON "MultiPolygon" obj                         = MultiPolygon <$> readJSON obj
geometryFromJSON "Line" obj                                 = Line <$> readJSON obj
geometryFromJSON "MultiLine" obj                            = MultiLine <$> readJSON obj
geometryFromJSON "GeometryCollection" (JSObject jsonObj)    = Collection <$> (valFromObj "geometries" jsonObj >>= readJSON)
geometryFromJSON "GeometryCollection" _                     = Error "Invalid value type for 'geometries' attribute.."
geometryFromJSON typeString _                               = Error $ "Invalid Geometry Type: " ++ typeString

crsPropertyFromObj :: (JSON a) => String -> JSObject JSValue -> Result a
crsPropertyFromObj name obj = do
    props <- valFromObj "properties" obj
    valFromObj name props

crsObjectFromJSON :: String -> JSObject JSValue -> Result CRSObject
crsObjectFromJSON "name" obj    = NamedCRS <$> crsPropertyFromObj "name" obj
crsObjectFromJSON "epsg" obj    = EPSG <$> crsPropertyFromObj "code" obj
crsObjectFromJSON "link" obj    = LinkedCRS <$> crsPropertyFromObj "href" obj <*> crsPropertyFromObj "type" obj
crsObjectFromJSON typeString _  = Error $ "Invalid or unimplemented CRS Object Type: " ++ typeString

-- end helper functions

-- newtype to avoid orphan instances.

-- Conversion of Geospatial data types into GeoJSON,

instance JSON GeoMultiPoint where
    readJSON = readGeometryGeoJSON "MultiPoint" GeoMultiPoint

    showJSON (GeoMultiPoint points) = makeGeometryGeoJSON "MultiPoint" points

instance JSON GeoPolygon where
    readJSON = readGeometryGeoJSON "Polygon" GeoPolygon

    showJSON (GeoPolygon vertices) = makeGeometryGeoJSON "Polygon" vertices

instance JSON GeoMultiPolygon where
    readJSON = readGeometryGeoJSON "MultiPolygon" GeoMultiPolygon

    showJSON (GeoMultiPolygon polys) = makeGeometryGeoJSON "MultiPolygon" polys

instance JSON GeoLine where
    readJSON = readGeometryGeoJSON "Line" GeoLine

    showJSON (GeoLine line) = makeGeometryGeoJSON "Line" line

instance JSON GeoMultiLine where
    readJSON = readGeometryGeoJSON "MultiLine" GeoMultiLine

    showJSON (GeoMultiLine lines') = makeGeometryGeoJSON "MultiLine" lines'

-- |
-- encodes and decodes Geometry Objects to and from GeoJSON
-- (refer to source to see the values for the test values)
--
-- >>> encode NoGeometry
-- "null"
--
-- >>> decode "null" :: Result GeospatialGeometry
-- Ok NoGeometry
--
-- >>> encode lShapedPoly == lShapedPolyJSON
-- True
--
-- >>> decode lShapedPolyJSON == Ok lShapedPoly
-- True
--
-- >>> encode emptyPoly == emptyPolyJSON
-- True
--
-- >>> decode emptyPolyJSON == Ok emptyPoly
-- True
--
-- >>> encode emptyMultiPoly == emptyMultiPolyJSON
-- True
--
-- >>> decode emptyMultiPolyJSON == Ok emptyMultiPoly
-- True
--
-- >>> encode singleLineMultiLine == singleLineMultiLineJSON
-- True
--
-- >>> decode singleLineMultiLineJSON == Ok singleLineMultiLine
-- True
--
-- >>> encode multiLine == multiLineJSON
-- True
--
-- >>> decode multiLineJSON == Ok multiLine
-- True
--
-- >>> encode emptyCollection == emptyCollectionJSON
-- True
--
-- >>> decode emptyCollectionJSON == Ok emptyCollection
-- True
--
-- >>> encode bigassCollection == bigassCollectionJSON
-- True
--
-- >>> decode bigassCollectionJSON == Ok bigassCollection
-- True
--
instance JSON GeospatialGeometry where
    readJSON JSNull = Ok NoGeometry
    readJSON json   = do
        geometryObj <- readJSON json
        geometryType <- valFromObj "type" geometryObj
        geometryFromJSON geometryType (JSObject geometryObj)

    showJSON (NoGeometry)               = JSNull
    showJSON (Point point)              = showJSON point
    showJSON (MultiPoint points)        = showJSON points
    showJSON (Polygon vertices)         = showJSON vertices
    showJSON (MultiPolygon vertices)    = showJSON vertices
    showJSON (Line vertices)            = showJSON vertices
    showJSON (MultiLine vertices)       = showJSON vertices
    showJSON (Collection geometries)    = makeObj [("type", showJSON "GeometryCollection"), ("geometries", showJSON geometries)]

-- |
-- encode and decodes CRS Objects to and from GeoJSON
--
-- >>> encode testLinkCRS == testLinkCRSJSON
-- True
--
-- >>> decode testLinkCRSJSON == Ok testLinkCRS
-- True
--
-- >>> encode testNamedCRS == testNamedCRSJSON
-- True
--
-- >>> decode testNamedCRSJSON == Ok testNamedCRS
-- True
--
-- >>> encode testEPSG == testEPSGJSON
-- True
--
-- >>> decode testEPSGJSON == Ok testEPSG
-- True
--
-- >>> encode NoCRS
-- "null"
--
-- >>> decode "null" == Ok NoCRS
-- True
--
instance JSON CRSObject where
    readJSON JSNull = Ok NoCRS
    readJSON json   = do
        crsObject <- readJSON json
        crsType <- valFromObj "type" crsObject
        crsObjectFromJSON crsType crsObject

    showJSON (NamedCRS name)                    = makeObj [("type", showJSON "name"), ("properties", showJSON (makeObj [("name", showJSON name)]))]
    showJSON (EPSG code)                        = makeObj [("type", showJSON "epsg"), ("properties", showJSON (makeObj [("code", showJSON code)]))]
    showJSON (LinkedCRS href format )           = makeObj [("type", showJSON "link"), ("properties", showJSON (makeObj [("href", showJSON href), ("type", showJSON format)]))]
    showJSON NoCRS                              = JSNull

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
instance JSON GeoFeature where
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
            baseAttributes = [("type", showJSON "Feature"), ("properties", showJSON props), ("geometry", showJSON geom)]

-- | Encodes and Decodes FeatureCollection objects to and from GeoJSON
--
-- >>> encode bigAssFeatureCollection == bigAssFeatureCollectionJSON
-- True
--
-- >>> decode bigAssFeatureCollectionJSON == Ok bigAssFeatureCollection
-- True
--
-- >>> encode bigAssFeatureCollectionWithNoBBox == bigAssFeatureCollectionWithNoBBoxJSON
-- True
--
-- >>> decode bigAssFeatureCollectionWithNoBBoxJSON == Ok bigAssFeatureCollectionWithNoBBox
-- True
--
-- >>> encode emptyFeatureCollectionWithBBox == emptyFeatureCollectionWithBBoxJSON
-- True
--
-- >>> decode emptyFeatureCollectionWithBBoxJSON == Ok emptyFeatureCollectionWithBBox
-- True
--
-- >>> encode emptyFeatureCollection == emptyFeatureCollectionJSON
-- True
--
-- >>> decode emptyFeatureCollectionJSON == Ok emptyFeatureCollection
-- True
--
instance JSON GeoFeatureCollection where
    readJSON json = do
        obj <- readJSON json
        objType <- valFromObj "type" obj
        if objType /= "FeatureCollection"
            then
                fail $ "Invalid GeoJSON type for a Feature Collection: " ++ objType
            else
                GeoFeatureCollection <$> optValFromObj "bbox" obj <*> valFromObj "features" obj

    showJSON (GeoFeatureCollection bbox' features) = makeObj $ baseAttributes ++ optAttributes "bbox" bbox'
        where
            baseAttributes              = [("type", showJSON "FeatureCollection"), ("features", showJSON features)]

