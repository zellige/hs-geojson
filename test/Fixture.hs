{-# LANGUAGE OverloadedStrings #-}

module Fixture where

import qualified Data.Aeson                                        as A
import qualified Data.ByteString.Lazy.Char8                        as BS
import qualified Data.Text                                         as T
-- Local
import           Data.Geospatial.Internal.BasicTypes               (BoundingBoxWithoutCRS,
                                                                    FeatureID (..),
                                                                    GeoPositionWithoutCRS)
import           Data.Geospatial.Internal.CRS                      (CRSObject (..))
import           Data.Geospatial.Internal.GeoFeature               (GeoFeature (..))
import           Data.Geospatial.Internal.GeoFeatureCollection     (GeoFeatureCollection (..))
import           Data.Geospatial.Internal.Geometry                 (GeospatialGeometry (..))
import           Data.Geospatial.Internal.Geometry.GeoLine         (GeoLine (..))
import           Data.Geospatial.Internal.Geometry.GeoMultiLine    (GeoMultiLine (..),
                                                                    mergeGeoLines)
import           Data.Geospatial.Internal.Geometry.GeoMultiPolygon (GeoMultiPolygon (..),
                                                                    mergeGeoPolygons)
import           Data.Geospatial.Internal.Geometry.GeoPolygon      (GeoPolygon (..))
import           Data.LinearRing                                   (LinearRing, makeLinearRing)
import           Data.LineString                                   (LineString, makeLineString)


-- CRS Data

testLinkCRSJSON :: BS.ByteString
testLinkCRSJSON = "{\"type\":\"link\",\"properties\":{\"href\":\"www.google.com.au\",\"type\":\"proj4\"}}"

testLinkCRS :: CRSObject
testLinkCRS = LinkedCRS "www.google.com.au" "proj4"

testEPSGJSON :: BS.ByteString
testEPSGJSON = "{\"type\":\"epsg\",\"properties\":{\"code\":4326}}"

testEPSG :: CRSObject
testEPSG = EPSG 4326

testNamedCRSJSON :: BS.ByteString
testNamedCRSJSON = "{\"type\":\"name\",\"properties\":{\"name\":\"urn:ogc:def:crs:OGC:1.3:CRS84\"}}"

testNamedCRS :: CRSObject
testNamedCRS = NamedCRS "urn:ogc:def:crs:OGC:1.3:CRS84"

-- Bounding Box Data

lshapedPolyVertices :: [LinearRing GeoPositionWithoutCRS]
lshapedPolyVertices = [ makeLinearRing [120.0, -15.0] [127.0, -15.0] [127.0, -25.0] [[124.0, -25.0], [124.0, -18.0], [120.0, -18.0]] ]

lshapedPolyLineVertices :: LineString GeoPositionWithoutCRS
lshapedPolyLineVertices = makeLineString [120.0, -15.0] [127.0, -15.0] [[127.0, -25.0], [124.0, -25.0], [124.0, -18.0], [120.0, -18.0]]


emptyVertices :: [LinearRing GeoPositionWithoutCRS]
emptyVertices = []

emptyLineVertices :: [GeoPositionWithoutCRS]
emptyLineVertices = []


testLatLonBBox :: BoundingBoxWithoutCRS
testLatLonBBox = [-32, 147.5, -29.5, 151.0]

testLatLonBBoxJSON :: BS.ByteString
testLatLonBBoxJSON = "[-32,147.5,-29.5,151]"


testEmptyBBox :: BoundingBoxWithoutCRS
testEmptyBBox = []

testEmptyBBoxJSON :: BS.ByteString
testEmptyBBoxJSON = "[]"

-- Geometry Data
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

-- Polys
lShapedPolyJSON :: BS.ByteString
lShapedPolyJSON = "{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]],\"type\":\"Polygon\"}"

lShapedGeoPoly :: GeoPolygon
lShapedGeoPoly = GeoPolygon lshapedPolyVertices

lShapedPoly :: GeospatialGeometry
lShapedPoly = Polygon lShapedGeoPoly


emptyPolyJSON :: BS.ByteString
emptyPolyJSON = "{\"type\":\"Polygon\",\"coordinates\":[]}"

emptyGeoPoly :: GeoPolygon
emptyGeoPoly = GeoPolygon emptyVertices

emptyPoly :: GeospatialGeometry
emptyPoly = Polygon emptyGeoPoly

-- Multi Polys
emptyMultiPolyJSON :: BS.ByteString
emptyMultiPolyJSON = "{\"type\":\"MultiPolygon\",\"coordinates\":[]}"

emptyMultiGeoPoly :: GeoMultiPolygon
emptyMultiGeoPoly = GeoMultiPolygon []

emptyMultiPoly :: GeospatialGeometry
emptyMultiPoly = MultiPolygon emptyMultiGeoPoly


singlePolyMultiPolyJSON :: BS.ByteString
singlePolyMultiPolyJSON = "{\"type\":\"MultiPolygon\",\"coordinates\":[[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]]]}"

singlePolyGeoMultiPoly :: GeoMultiPolygon
singlePolyGeoMultiPoly = mergeGeoPolygons [lShapedGeoPoly]

singlePolyMultiPoly :: GeospatialGeometry
singlePolyMultiPoly = MultiPolygon singlePolyGeoMultiPoly


multiPolyJSON :: BS.ByteString
multiPolyJSON = "{\"type\":\"MultiPolygon\",\"coordinates\":[[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]],[]]}"

geoMultiPoly :: GeoMultiPolygon
geoMultiPoly = mergeGeoPolygons [lShapedGeoPoly, emptyGeoPoly]

multiPoly :: GeospatialGeometry
multiPoly = MultiPolygon geoMultiPoly

-- Lines
lShapedLineJSON :: BS.ByteString
lShapedLineJSON = "{\"type\":\"LineString\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]}"

lShapedGeoLine :: GeoLine
lShapedGeoLine = GeoLine lshapedPolyLineVertices

lShapedLine :: GeospatialGeometry
lShapedLine = Line lShapedGeoLine

-- Multi Lines
emptyMultiLineJSON :: BS.ByteString
emptyMultiLineJSON = "{\"type\":\"MultiLineString\",\"coordinates\":[]}"

emptyMultiGeoLine :: GeoMultiLine
emptyMultiGeoLine = GeoMultiLine []

emptyMultiLine :: GeospatialGeometry
emptyMultiLine = MultiLine emptyMultiGeoLine


singleLineMultiLineJSON :: BS.ByteString
singleLineMultiLineJSON = "{\"type\":\"MultiLineString\",\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]]}"

singleLineGeoMultiLine :: GeoMultiLine
singleLineGeoMultiLine = mergeGeoLines [lShapedGeoLine]

singleLineMultiLine :: GeospatialGeometry
singleLineMultiLine = MultiLine singleLineGeoMultiLine


multiLineJSON :: BS.ByteString
multiLineJSON = "{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]],[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]],\"type\":\"MultiLineString\"}"

geoMultiLine :: GeoMultiLine
geoMultiLine = mergeGeoLines [lShapedGeoLine, lShapedGeoLine]

multiLine :: GeospatialGeometry
multiLine = MultiLine geoMultiLine

-- Collection
emptyCollectionJSON :: BS.ByteString
emptyCollectionJSON = "{\"type\":\"GeometryCollection\",\"geometries\":[]}"

emptyCollection :: GeospatialGeometry
emptyCollection = Collection []


bigassCollectionJSON :: BS.ByteString
bigassCollectionJSON = "{\"geometries\":[{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]],\"type\":\"MultiLineString\"},{\"coordinates\":[],\"type\":\"MultiLineString\"},{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]],[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]],\"type\":\"MultiLineString\"},{\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]],\"type\":\"LineString\"},{\"coordinates\":[[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]],[]],\"type\":\"MultiPolygon\"},{\"coordinates\":[[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]]],\"type\":\"MultiPolygon\"},{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]],\"type\":\"Polygon\"},{\"coordinates\":[],\"type\":\"MultiPolygon\"},{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]],\"type\":\"Polygon\"}],\"type\":\"GeometryCollection\"}"

bigassCollection :: GeospatialGeometry
bigassCollection = Collection [singleLineMultiLine, emptyMultiLine, multiLine, lShapedLine, multiPoly, singlePolyMultiPoly, lShapedPoly, emptyMultiPoly, lShapedPoly]

-- Properties Data

testProperties :: A.Value
testProperties = A.object [T.pack "depth" A..= (5 :: Int), T.pack "comment" A..= T.pack "Bore run over by dump truck"]

-- Feature Data

featureId :: Maybe FeatureID
featureId = Just $ FeatureIDText "GW001"

bbox :: Maybe BoundingBoxWithoutCRS
bbox = Just testLatLonBBox


bigFeatureJSON :: BS.ByteString
bigFeatureJSON = "{\"bbox\":[-32,147.5,-29.5,151],\"geometry\":{\"geometries\":[{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]],\"type\":\"MultiLineString\"},{\"coordinates\":[],\"type\":\"MultiLineString\"},{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]],[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]],\"type\":\"MultiLineString\"},{\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]],\"type\":\"LineString\"},{\"coordinates\":[[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]],[]],\"type\":\"MultiPolygon\"},{\"coordinates\":[[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]]],\"type\":\"MultiPolygon\"},{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]],\"type\":\"Polygon\"},{\"coordinates\":[],\"type\":\"MultiPolygon\"},{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]],\"type\":\"Polygon\"}],\"type\":\"GeometryCollection\"},\"id\":\"GW001\",\"type\":\"Feature\",\"properties\":{\"depth\":5,\"comment\":\"Bore run over by dump truck\"}}"

bigFeature :: GeoFeature A.Value
bigFeature = GeoFeature bbox bigassCollection testProperties featureId


featureWithNoPropertiesJSON :: BS.ByteString
featureWithNoPropertiesJSON = "{\"bbox\":[-32,147.5,-29.5,151],\"geometry\":{\"geometries\":[{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]],\"type\":\"MultiLineString\"},{\"coordinates\":[],\"type\":\"MultiLineString\"},{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]],[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]],\"type\":\"MultiLineString\"},{\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]],\"type\":\"LineString\"},{\"coordinates\":[[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]],[]],\"type\":\"MultiPolygon\"},{\"coordinates\":[[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]]],\"type\":\"MultiPolygon\"},{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]],\"type\":\"Polygon\"},{\"coordinates\":[],\"type\":\"MultiPolygon\"},{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]],\"type\":\"Polygon\"}],\"type\":\"GeometryCollection\"},\"id\":\"GW001\",\"type\":\"Feature\",\"properties\":null}"

featureWithNoProperties :: GeoFeature A.Value
featureWithNoProperties = GeoFeature bbox bigassCollection A.Null featureId


featureWithNoGeometryJSON :: BS.ByteString
featureWithNoGeometryJSON = "{\"type\":\"Feature\",\"properties\":{\"depth\":5,\"comment\":\"Bore run over by dump truck\"},\"geometry\":null,\"bbox\":[-32,147.5,-29.5,151],\"id\":\"GW001\"}"

featureWithNoGeometry :: GeoFeature A.Value
featureWithNoGeometry = GeoFeature bbox NoGeometry testProperties featureId


featureWithNoIdJSON :: BS.ByteString
featureWithNoIdJSON = "{\"bbox\":[-32,147.5,-29.5,151],\"geometry\":{\"geometries\":[{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]],\"type\":\"MultiLineString\"},{\"coordinates\":[],\"type\":\"MultiLineString\"},{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]],[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]],\"type\":\"MultiLineString\"},{\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]],\"type\":\"LineString\"},{\"coordinates\":[[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]],[]],\"type\":\"MultiPolygon\"},{\"coordinates\":[[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]]],\"type\":\"MultiPolygon\"},{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]],\"type\":\"Polygon\"},{\"coordinates\":[],\"type\":\"MultiPolygon\"},{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]],\"type\":\"Polygon\"}],\"type\":\"GeometryCollection\"},\"type\":\"Feature\",\"properties\":{\"depth\":5,\"comment\":\"Bore run over by dump truck\"}}"

featureWithNoId :: GeoFeature A.Value
featureWithNoId = GeoFeature bbox bigassCollection testProperties Nothing


featureWithNoBBoxJSON :: BS.ByteString
featureWithNoBBoxJSON = "{\"geometry\":{\"geometries\":[{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]],\"type\":\"MultiLineString\"},{\"coordinates\":[],\"type\":\"MultiLineString\"},{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]],[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]],\"type\":\"MultiLineString\"},{\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]],\"type\":\"LineString\"},{\"coordinates\":[[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]],[]],\"type\":\"MultiPolygon\"},{\"coordinates\":[[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]]],\"type\":\"MultiPolygon\"},{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]],\"type\":\"Polygon\"},{\"coordinates\":[],\"type\":\"MultiPolygon\"},{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]],\"type\":\"Polygon\"}],\"type\":\"GeometryCollection\"},\"id\":\"GW001\",\"type\":\"Feature\",\"properties\":{\"depth\":5,\"comment\":\"Bore run over by dump truck\"}}"

featureWithNoBBox :: GeoFeature A.Value
featureWithNoBBox = GeoFeature Nothing bigassCollection testProperties featureId

-- FeatureCollection Data

features :: [GeoFeature A.Value]
features = [featureWithNoBBox, featureWithNoGeometry, featureWithNoBBox, featureWithNoId, featureWithNoProperties, bigFeature]


bigAssFeatureCollectionJSON :: BS.ByteString
bigAssFeatureCollectionJSON = "{\"bbox\":[-32,147.5,-29.5,151],\"features\":[{\"geometry\":{\"geometries\":[{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]],\"type\":\"MultiLineString\"},{\"coordinates\":[],\"type\":\"MultiLineString\"},{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]],[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]],\"type\":\"MultiLineString\"},{\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]],\"type\":\"LineString\"},{\"coordinates\":[[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]],[]],\"type\":\"MultiPolygon\"},{\"coordinates\":[[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]]],\"type\":\"MultiPolygon\"},{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]],\"type\":\"Polygon\"},{\"coordinates\":[],\"type\":\"MultiPolygon\"},{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]],\"type\":\"Polygon\"}],\"type\":\"GeometryCollection\"},\"id\":\"GW001\",\"type\":\"Feature\",\"properties\":{\"depth\":5,\"comment\":\"Bore run over by dump truck\"}},{\"bbox\":[-32,147.5,-29.5,151],\"geometry\":null,\"id\":\"GW001\",\"type\":\"Feature\",\"properties\":{\"depth\":5,\"comment\":\"Bore run over by dump truck\"}},{\"geometry\":{\"geometries\":[{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]],\"type\":\"MultiLineString\"},{\"coordinates\":[],\"type\":\"MultiLineString\"},{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]],[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]],\"type\":\"MultiLineString\"},{\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]],\"type\":\"LineString\"},{\"coordinates\":[[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]],[]],\"type\":\"MultiPolygon\"},{\"coordinates\":[[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]]],\"type\":\"MultiPolygon\"},{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]],\"type\":\"Polygon\"},{\"coordinates\":[],\"type\":\"MultiPolygon\"},{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]],\"type\":\"Polygon\"}],\"type\":\"GeometryCollection\"},\"id\":\"GW001\",\"type\":\"Feature\",\"properties\":{\"depth\":5,\"comment\":\"Bore run over by dump truck\"}},{\"bbox\":[-32,147.5,-29.5,151],\"geometry\":{\"geometries\":[{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]],\"type\":\"MultiLineString\"},{\"coordinates\":[],\"type\":\"MultiLineString\"},{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]],[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]],\"type\":\"MultiLineString\"},{\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]],\"type\":\"LineString\"},{\"coordinates\":[[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]],[]],\"type\":\"MultiPolygon\"},{\"coordinates\":[[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]]],\"type\":\"MultiPolygon\"},{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]],\"type\":\"Polygon\"},{\"coordinates\":[],\"type\":\"MultiPolygon\"},{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]],\"type\":\"Polygon\"}],\"type\":\"GeometryCollection\"},\"type\":\"Feature\",\"properties\":{\"depth\":5,\"comment\":\"Bore run over by dump truck\"}},{\"bbox\":[-32,147.5,-29.5,151],\"geometry\":{\"geometries\":[{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]],\"type\":\"MultiLineString\"},{\"coordinates\":[],\"type\":\"MultiLineString\"},{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]],[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]],\"type\":\"MultiLineString\"},{\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]],\"type\":\"LineString\"},{\"coordinates\":[[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]],[]],\"type\":\"MultiPolygon\"},{\"coordinates\":[[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]]],\"type\":\"MultiPolygon\"},{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]],\"type\":\"Polygon\"},{\"coordinates\":[],\"type\":\"MultiPolygon\"},{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]],\"type\":\"Polygon\"}],\"type\":\"GeometryCollection\"},\"id\":\"GW001\",\"type\":\"Feature\",\"properties\":null},{\"bbox\":[-32,147.5,-29.5,151],\"geometry\":{\"geometries\":[{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]],\"type\":\"MultiLineString\"},{\"coordinates\":[],\"type\":\"MultiLineString\"},{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]],[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]],\"type\":\"MultiLineString\"},{\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]],\"type\":\"LineString\"},{\"coordinates\":[[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]],[]],\"type\":\"MultiPolygon\"},{\"coordinates\":[[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]]],\"type\":\"MultiPolygon\"},{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]],\"type\":\"Polygon\"},{\"coordinates\":[],\"type\":\"MultiPolygon\"},{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]],\"type\":\"Polygon\"}],\"type\":\"GeometryCollection\"},\"id\":\"GW001\",\"type\":\"Feature\",\"properties\":{\"depth\":5,\"comment\":\"Bore run over by dump truck\"}}],\"type\":\"FeatureCollection\"}"

bigAssFeatureCollection :: GeoFeatureCollection A.Value
bigAssFeatureCollection = GeoFeatureCollection bbox features


bigAssFeatureCollectionWithNoBBoxJSON :: BS.ByteString
bigAssFeatureCollectionWithNoBBoxJSON = "{\"features\":[{\"geometry\":{\"geometries\":[{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]],\"type\":\"MultiLineString\"},{\"coordinates\":[],\"type\":\"MultiLineString\"},{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]],[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]],\"type\":\"MultiLineString\"},{\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]],\"type\":\"LineString\"},{\"coordinates\":[[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]],[]],\"type\":\"MultiPolygon\"},{\"coordinates\":[[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]]],\"type\":\"MultiPolygon\"},{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]],\"type\":\"Polygon\"},{\"coordinates\":[],\"type\":\"MultiPolygon\"},{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]],\"type\":\"Polygon\"}],\"type\":\"GeometryCollection\"},\"id\":\"GW001\",\"type\":\"Feature\",\"properties\":{\"depth\":5,\"comment\":\"Bore run over by dump truck\"}},{\"bbox\":[-32,147.5,-29.5,151],\"geometry\":null,\"id\":\"GW001\",\"type\":\"Feature\",\"properties\":{\"depth\":5,\"comment\":\"Bore run over by dump truck\"}},{\"geometry\":{\"geometries\":[{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]],\"type\":\"MultiLineString\"},{\"coordinates\":[],\"type\":\"MultiLineString\"},{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]],[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]],\"type\":\"MultiLineString\"},{\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]],\"type\":\"LineString\"},{\"coordinates\":[[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]],[]],\"type\":\"MultiPolygon\"},{\"coordinates\":[[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]]],\"type\":\"MultiPolygon\"},{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]],\"type\":\"Polygon\"},{\"coordinates\":[],\"type\":\"MultiPolygon\"},{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]],\"type\":\"Polygon\"}],\"type\":\"GeometryCollection\"},\"id\":\"GW001\",\"type\":\"Feature\",\"properties\":{\"depth\":5,\"comment\":\"Bore run over by dump truck\"}},{\"bbox\":[-32,147.5,-29.5,151],\"geometry\":{\"geometries\":[{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]],\"type\":\"MultiLineString\"},{\"coordinates\":[],\"type\":\"MultiLineString\"},{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]],[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]],\"type\":\"MultiLineString\"},{\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]],\"type\":\"LineString\"},{\"coordinates\":[[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]],[]],\"type\":\"MultiPolygon\"},{\"coordinates\":[[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]]],\"type\":\"MultiPolygon\"},{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]],\"type\":\"Polygon\"},{\"coordinates\":[],\"type\":\"MultiPolygon\"},{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]],\"type\":\"Polygon\"}],\"type\":\"GeometryCollection\"},\"type\":\"Feature\",\"properties\":{\"depth\":5,\"comment\":\"Bore run over by dump truck\"}},{\"bbox\":[-32,147.5,-29.5,151],\"geometry\":{\"geometries\":[{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]],\"type\":\"MultiLineString\"},{\"coordinates\":[],\"type\":\"MultiLineString\"},{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]],[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]],\"type\":\"MultiLineString\"},{\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]],\"type\":\"LineString\"},{\"coordinates\":[[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]],[]],\"type\":\"MultiPolygon\"},{\"coordinates\":[[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]]],\"type\":\"MultiPolygon\"},{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]],\"type\":\"Polygon\"},{\"coordinates\":[],\"type\":\"MultiPolygon\"},{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]],\"type\":\"Polygon\"}],\"type\":\"GeometryCollection\"},\"id\":\"GW001\",\"type\":\"Feature\",\"properties\":null},{\"bbox\":[-32,147.5,-29.5,151],\"geometry\":{\"geometries\":[{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]],\"type\":\"MultiLineString\"},{\"coordinates\":[],\"type\":\"MultiLineString\"},{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]],[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]],\"type\":\"MultiLineString\"},{\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]],\"type\":\"LineString\"},{\"coordinates\":[[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]],[]],\"type\":\"MultiPolygon\"},{\"coordinates\":[[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]]],\"type\":\"MultiPolygon\"},{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]],\"type\":\"Polygon\"},{\"coordinates\":[],\"type\":\"MultiPolygon\"},{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]],\"type\":\"Polygon\"}],\"type\":\"GeometryCollection\"},\"id\":\"GW001\",\"type\":\"Feature\",\"properties\":{\"depth\":5,\"comment\":\"Bore run over by dump truck\"}}],\"type\":\"FeatureCollection\"}"

bigAssFeatureCollectionWithNoBBox :: GeoFeatureCollection A.Value
bigAssFeatureCollectionWithNoBBox = GeoFeatureCollection Nothing features


emptyFeatureCollectionJSON :: BS.ByteString
emptyFeatureCollectionJSON = "{\"type\":\"FeatureCollection\",\"features\":[]}"

emptyFeatureCollection :: GeoFeatureCollection A.Value
emptyFeatureCollection = GeoFeatureCollection Nothing []


emptyFeatureCollectionWithBBoxJSON :: BS.ByteString
emptyFeatureCollectionWithBBoxJSON = "{\"type\":\"FeatureCollection\",\"features\":[],\"bbox\":[-32,147.5,-29.5,151]}"

emptyFeatureCollectionWithBBox :: GeoFeatureCollection A.Value
emptyFeatureCollectionWithBBox = GeoFeatureCollection bbox []
