{-# LANGUAGE OverloadedStrings #-}

module Fixture where

import qualified Data.Aeson                                        as Aeson
import qualified Data.ByteString.Lazy.Char8                        as BS
import qualified Data.Text                                         as T
-- Local
import qualified Data.Geospatial.Internal.BasicTypes               as BasicTypes
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
import qualified Data.LinearRing                                   as LinearRing
import qualified Data.LineString                                   as LineString
import qualified Data.Vector                                       as Vector
import qualified Data.Vector.Storable                              as VectorStorable

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

mkGeoPosition :: [Double] -> BasicTypes.GeoPositionWithoutCRS
mkGeoPosition x = BasicTypes.GeoPositionWithoutCRS $ VectorStorable.fromList x

mkBoundingBox :: [Double] -> BasicTypes.BoundingBoxWithoutCRS
mkBoundingBox x = BasicTypes.BoundingBoxWithoutCRS $ VectorStorable.fromList x

-- Bounding Box Data

lshapedPolyVertices :: Vector.Vector (LinearRing.LinearRing BasicTypes.GeoPositionWithoutCRS)
lshapedPolyVertices =  Vector.fromList [LinearRing.makeLinearRing (mkGeoPosition [120.0, -15.0]) (mkGeoPosition [127.0, -15.0]) (mkGeoPosition [127.0, -25.0]) [mkGeoPosition [124.0, -25.0], mkGeoPosition [124.0, -18.0], mkGeoPosition [120.0, -18.0]]]

lshapedPolyLineVertices :: LineString.LineString BasicTypes.GeoPositionWithoutCRS
lshapedPolyLineVertices = LineString.makeLineString (mkGeoPosition [120.0, -15.0]) (mkGeoPosition [127.0, -15.0]) [mkGeoPosition [127.0, -25.0], mkGeoPosition [124.0, -25.0], mkGeoPosition [124.0, -18.0], mkGeoPosition [120.0, -18.0]]

emptyVertices :: Vector.Vector (LinearRing.LinearRing BasicTypes.GeoPositionWithoutCRS)
emptyVertices = Vector.empty

emptyLineVertices :: [BasicTypes.GeoPositionWithoutCRS]
emptyLineVertices = []

testLatLonBBox :: BasicTypes.BoundingBoxWithoutCRS
testLatLonBBox = mkBoundingBox [-32, 147.5, -29.5, 151.0]

testLatLonBBoxJSON :: BS.ByteString
testLatLonBBoxJSON = "[-32,147.5,-29.5,151]"

testEmptyBBox :: BasicTypes.BoundingBoxWithoutCRS
testEmptyBBox = mkBoundingBox []

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
emptyMultiGeoPoly = GeoMultiPolygon Vector.empty

emptyMultiPoly :: GeospatialGeometry
emptyMultiPoly = MultiPolygon emptyMultiGeoPoly

singlePolyMultiPolyJSON :: BS.ByteString
singlePolyMultiPolyJSON = "{\"type\":\"MultiPolygon\",\"coordinates\":[[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]]]}"

singlePolyGeoMultiPoly :: GeoMultiPolygon
singlePolyGeoMultiPoly = mergeGeoPolygons (Vector.fromList [lShapedGeoPoly])

singlePolyMultiPoly :: GeospatialGeometry
singlePolyMultiPoly = MultiPolygon singlePolyGeoMultiPoly

multiPolyJSON :: BS.ByteString
multiPolyJSON = "{\"type\":\"MultiPolygon\",\"coordinates\":[[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]],[]]}"

geoMultiPoly :: GeoMultiPolygon
geoMultiPoly = mergeGeoPolygons (Vector.fromList [lShapedGeoPoly, emptyGeoPoly])

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
emptyMultiGeoLine = GeoMultiLine Vector.empty

emptyMultiLine :: GeospatialGeometry
emptyMultiLine = MultiLine emptyMultiGeoLine

singleLineMultiLineJSON :: BS.ByteString
singleLineMultiLineJSON = "{\"type\":\"MultiLineString\",\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]]}"

singleLineGeoMultiLine :: GeoMultiLine
singleLineGeoMultiLine = mergeGeoLines (Vector.fromList [lShapedGeoLine])

singleLineMultiLine :: GeospatialGeometry
singleLineMultiLine = MultiLine singleLineGeoMultiLine

multiLineJSON :: BS.ByteString
multiLineJSON = "{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]],[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]],\"type\":\"MultiLineString\"}"

geoMultiLine :: GeoMultiLine
geoMultiLine = mergeGeoLines (Vector.fromList [lShapedGeoLine, lShapedGeoLine])

multiLine :: GeospatialGeometry
multiLine = MultiLine geoMultiLine

-- Collection
emptyCollectionJSON :: BS.ByteString
emptyCollectionJSON = "{\"type\":\"GeometryCollection\",\"geometries\":[]}"

emptyCollection :: GeospatialGeometry
emptyCollection = Collection Vector.empty

bigassCollectionJSON :: BS.ByteString
bigassCollectionJSON = "{\"geometries\":[{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]],\"type\":\"MultiLineString\"},{\"coordinates\":[],\"type\":\"MultiLineString\"},{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]],[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]],\"type\":\"MultiLineString\"},{\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]],\"type\":\"LineString\"},{\"coordinates\":[[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]],[]],\"type\":\"MultiPolygon\"},{\"coordinates\":[[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]]],\"type\":\"MultiPolygon\"},{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]],\"type\":\"Polygon\"},{\"coordinates\":[],\"type\":\"MultiPolygon\"},{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]],\"type\":\"Polygon\"}],\"type\":\"GeometryCollection\"}"

bigassCollection :: GeospatialGeometry
bigassCollection = Collection $ Vector.fromList [singleLineMultiLine, emptyMultiLine, multiLine, lShapedLine, multiPoly, singlePolyMultiPoly, lShapedPoly, emptyMultiPoly, lShapedPoly]

-- Properties Data

testProperties :: Aeson.Value
testProperties = Aeson.object [T.pack "depth" Aeson..= (5 :: Int), T.pack "comment" Aeson..= T.pack "Bore run over by dump truck"]

-- Feature Data

featureId :: Maybe BasicTypes.FeatureID
featureId = Just $ BasicTypes.FeatureIDText "GW001"

bbox :: Maybe BasicTypes.BoundingBoxWithoutCRS
bbox = Just testLatLonBBox


bigFeatureJSON :: BS.ByteString
bigFeatureJSON = "{\"bbox\":[-32,147.5,-29.5,151],\"geometry\":{\"geometries\":[{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]],\"type\":\"MultiLineString\"},{\"coordinates\":[],\"type\":\"MultiLineString\"},{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]],[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]],\"type\":\"MultiLineString\"},{\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]],\"type\":\"LineString\"},{\"coordinates\":[[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]],[]],\"type\":\"MultiPolygon\"},{\"coordinates\":[[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]]],\"type\":\"MultiPolygon\"},{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]],\"type\":\"Polygon\"},{\"coordinates\":[],\"type\":\"MultiPolygon\"},{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]],\"type\":\"Polygon\"}],\"type\":\"GeometryCollection\"},\"id\":\"GW001\",\"type\":\"Feature\",\"properties\":{\"depth\":5,\"comment\":\"Bore run over by dump truck\"}}"

bigFeature :: GeoFeature Aeson.Value
bigFeature = GeoFeature bbox bigassCollection testProperties featureId


featureWithNoPropertiesJSON :: BS.ByteString
featureWithNoPropertiesJSON = "{\"bbox\":[-32,147.5,-29.5,151],\"geometry\":{\"geometries\":[{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]],\"type\":\"MultiLineString\"},{\"coordinates\":[],\"type\":\"MultiLineString\"},{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]],[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]],\"type\":\"MultiLineString\"},{\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]],\"type\":\"LineString\"},{\"coordinates\":[[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]],[]],\"type\":\"MultiPolygon\"},{\"coordinates\":[[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]]],\"type\":\"MultiPolygon\"},{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]],\"type\":\"Polygon\"},{\"coordinates\":[],\"type\":\"MultiPolygon\"},{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]],\"type\":\"Polygon\"}],\"type\":\"GeometryCollection\"},\"id\":\"GW001\",\"type\":\"Feature\",\"properties\":null}"

featureWithNoProperties :: GeoFeature Aeson.Value
featureWithNoProperties = GeoFeature bbox bigassCollection Aeson.Null featureId


featureWithNoGeometryJSON :: BS.ByteString
featureWithNoGeometryJSON = "{\"type\":\"Feature\",\"properties\":{\"depth\":5,\"comment\":\"Bore run over by dump truck\"},\"geometry\":null,\"bbox\":[-32,147.5,-29.5,151],\"id\":\"GW001\"}"

featureWithNoGeometry :: GeoFeature Aeson.Value
featureWithNoGeometry = GeoFeature bbox NoGeometry testProperties featureId


featureWithNoIdJSON :: BS.ByteString
featureWithNoIdJSON = "{\"bbox\":[-32,147.5,-29.5,151],\"geometry\":{\"geometries\":[{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]],\"type\":\"MultiLineString\"},{\"coordinates\":[],\"type\":\"MultiLineString\"},{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]],[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]],\"type\":\"MultiLineString\"},{\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]],\"type\":\"LineString\"},{\"coordinates\":[[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]],[]],\"type\":\"MultiPolygon\"},{\"coordinates\":[[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]]],\"type\":\"MultiPolygon\"},{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]],\"type\":\"Polygon\"},{\"coordinates\":[],\"type\":\"MultiPolygon\"},{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]],\"type\":\"Polygon\"}],\"type\":\"GeometryCollection\"},\"type\":\"Feature\",\"properties\":{\"depth\":5,\"comment\":\"Bore run over by dump truck\"}}"

featureWithNoId :: GeoFeature Aeson.Value
featureWithNoId = GeoFeature bbox bigassCollection testProperties Nothing


featureWithNoBBoxJSON :: BS.ByteString
featureWithNoBBoxJSON = "{\"geometry\":{\"geometries\":[{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]],\"type\":\"MultiLineString\"},{\"coordinates\":[],\"type\":\"MultiLineString\"},{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]],[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]],\"type\":\"MultiLineString\"},{\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]],\"type\":\"LineString\"},{\"coordinates\":[[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]],[]],\"type\":\"MultiPolygon\"},{\"coordinates\":[[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]]],\"type\":\"MultiPolygon\"},{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]],\"type\":\"Polygon\"},{\"coordinates\":[],\"type\":\"MultiPolygon\"},{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]],\"type\":\"Polygon\"}],\"type\":\"GeometryCollection\"},\"id\":\"GW001\",\"type\":\"Feature\",\"properties\":{\"depth\":5,\"comment\":\"Bore run over by dump truck\"}}"

featureWithNoBBox :: GeoFeature Aeson.Value
featureWithNoBBox = GeoFeature Nothing bigassCollection testProperties featureId

-- FeatureCollection Data

features :: Vector.Vector (GeoFeature Aeson.Value)
features = Vector.fromList [featureWithNoBBox, featureWithNoGeometry, featureWithNoBBox, featureWithNoId, featureWithNoProperties, bigFeature]


bigAssFeatureCollectionJSON :: BS.ByteString
bigAssFeatureCollectionJSON = "{\"bbox\":[-32,147.5,-29.5,151],\"features\":[{\"geometry\":{\"geometries\":[{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]],\"type\":\"MultiLineString\"},{\"coordinates\":[],\"type\":\"MultiLineString\"},{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]],[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]],\"type\":\"MultiLineString\"},{\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]],\"type\":\"LineString\"},{\"coordinates\":[[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]],[]],\"type\":\"MultiPolygon\"},{\"coordinates\":[[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]]],\"type\":\"MultiPolygon\"},{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]],\"type\":\"Polygon\"},{\"coordinates\":[],\"type\":\"MultiPolygon\"},{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]],\"type\":\"Polygon\"}],\"type\":\"GeometryCollection\"},\"id\":\"GW001\",\"type\":\"Feature\",\"properties\":{\"depth\":5,\"comment\":\"Bore run over by dump truck\"}},{\"bbox\":[-32,147.5,-29.5,151],\"geometry\":null,\"id\":\"GW001\",\"type\":\"Feature\",\"properties\":{\"depth\":5,\"comment\":\"Bore run over by dump truck\"}},{\"geometry\":{\"geometries\":[{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]],\"type\":\"MultiLineString\"},{\"coordinates\":[],\"type\":\"MultiLineString\"},{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]],[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]],\"type\":\"MultiLineString\"},{\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]],\"type\":\"LineString\"},{\"coordinates\":[[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]],[]],\"type\":\"MultiPolygon\"},{\"coordinates\":[[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]]],\"type\":\"MultiPolygon\"},{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]],\"type\":\"Polygon\"},{\"coordinates\":[],\"type\":\"MultiPolygon\"},{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]],\"type\":\"Polygon\"}],\"type\":\"GeometryCollection\"},\"id\":\"GW001\",\"type\":\"Feature\",\"properties\":{\"depth\":5,\"comment\":\"Bore run over by dump truck\"}},{\"bbox\":[-32,147.5,-29.5,151],\"geometry\":{\"geometries\":[{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]],\"type\":\"MultiLineString\"},{\"coordinates\":[],\"type\":\"MultiLineString\"},{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]],[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]],\"type\":\"MultiLineString\"},{\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]],\"type\":\"LineString\"},{\"coordinates\":[[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]],[]],\"type\":\"MultiPolygon\"},{\"coordinates\":[[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]]],\"type\":\"MultiPolygon\"},{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]],\"type\":\"Polygon\"},{\"coordinates\":[],\"type\":\"MultiPolygon\"},{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]],\"type\":\"Polygon\"}],\"type\":\"GeometryCollection\"},\"type\":\"Feature\",\"properties\":{\"depth\":5,\"comment\":\"Bore run over by dump truck\"}},{\"bbox\":[-32,147.5,-29.5,151],\"geometry\":{\"geometries\":[{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]],\"type\":\"MultiLineString\"},{\"coordinates\":[],\"type\":\"MultiLineString\"},{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]],[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]],\"type\":\"MultiLineString\"},{\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]],\"type\":\"LineString\"},{\"coordinates\":[[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]],[]],\"type\":\"MultiPolygon\"},{\"coordinates\":[[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]]],\"type\":\"MultiPolygon\"},{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]],\"type\":\"Polygon\"},{\"coordinates\":[],\"type\":\"MultiPolygon\"},{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]],\"type\":\"Polygon\"}],\"type\":\"GeometryCollection\"},\"id\":\"GW001\",\"type\":\"Feature\",\"properties\":null},{\"bbox\":[-32,147.5,-29.5,151],\"geometry\":{\"geometries\":[{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]],\"type\":\"MultiLineString\"},{\"coordinates\":[],\"type\":\"MultiLineString\"},{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]],[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]],\"type\":\"MultiLineString\"},{\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]],\"type\":\"LineString\"},{\"coordinates\":[[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]],[]],\"type\":\"MultiPolygon\"},{\"coordinates\":[[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]]],\"type\":\"MultiPolygon\"},{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]],\"type\":\"Polygon\"},{\"coordinates\":[],\"type\":\"MultiPolygon\"},{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]],\"type\":\"Polygon\"}],\"type\":\"GeometryCollection\"},\"id\":\"GW001\",\"type\":\"Feature\",\"properties\":{\"depth\":5,\"comment\":\"Bore run over by dump truck\"}}],\"type\":\"FeatureCollection\"}"

bigAssFeatureCollection :: GeoFeatureCollection Aeson.Value
bigAssFeatureCollection = GeoFeatureCollection bbox features


bigAssFeatureCollectionWithNoBBoxJSON :: BS.ByteString
bigAssFeatureCollectionWithNoBBoxJSON = "{\"features\":[{\"geometry\":{\"geometries\":[{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]],\"type\":\"MultiLineString\"},{\"coordinates\":[],\"type\":\"MultiLineString\"},{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]],[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]],\"type\":\"MultiLineString\"},{\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]],\"type\":\"LineString\"},{\"coordinates\":[[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]],[]],\"type\":\"MultiPolygon\"},{\"coordinates\":[[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]]],\"type\":\"MultiPolygon\"},{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]],\"type\":\"Polygon\"},{\"coordinates\":[],\"type\":\"MultiPolygon\"},{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]],\"type\":\"Polygon\"}],\"type\":\"GeometryCollection\"},\"id\":\"GW001\",\"type\":\"Feature\",\"properties\":{\"depth\":5,\"comment\":\"Bore run over by dump truck\"}},{\"bbox\":[-32,147.5,-29.5,151],\"geometry\":null,\"id\":\"GW001\",\"type\":\"Feature\",\"properties\":{\"depth\":5,\"comment\":\"Bore run over by dump truck\"}},{\"geometry\":{\"geometries\":[{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]],\"type\":\"MultiLineString\"},{\"coordinates\":[],\"type\":\"MultiLineString\"},{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]],[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]],\"type\":\"MultiLineString\"},{\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]],\"type\":\"LineString\"},{\"coordinates\":[[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]],[]],\"type\":\"MultiPolygon\"},{\"coordinates\":[[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]]],\"type\":\"MultiPolygon\"},{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]],\"type\":\"Polygon\"},{\"coordinates\":[],\"type\":\"MultiPolygon\"},{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]],\"type\":\"Polygon\"}],\"type\":\"GeometryCollection\"},\"id\":\"GW001\",\"type\":\"Feature\",\"properties\":{\"depth\":5,\"comment\":\"Bore run over by dump truck\"}},{\"bbox\":[-32,147.5,-29.5,151],\"geometry\":{\"geometries\":[{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]],\"type\":\"MultiLineString\"},{\"coordinates\":[],\"type\":\"MultiLineString\"},{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]],[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]],\"type\":\"MultiLineString\"},{\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]],\"type\":\"LineString\"},{\"coordinates\":[[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]],[]],\"type\":\"MultiPolygon\"},{\"coordinates\":[[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]]],\"type\":\"MultiPolygon\"},{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]],\"type\":\"Polygon\"},{\"coordinates\":[],\"type\":\"MultiPolygon\"},{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]],\"type\":\"Polygon\"}],\"type\":\"GeometryCollection\"},\"type\":\"Feature\",\"properties\":{\"depth\":5,\"comment\":\"Bore run over by dump truck\"}},{\"bbox\":[-32,147.5,-29.5,151],\"geometry\":{\"geometries\":[{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]],\"type\":\"MultiLineString\"},{\"coordinates\":[],\"type\":\"MultiLineString\"},{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]],[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]],\"type\":\"MultiLineString\"},{\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]],\"type\":\"LineString\"},{\"coordinates\":[[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]],[]],\"type\":\"MultiPolygon\"},{\"coordinates\":[[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]]],\"type\":\"MultiPolygon\"},{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]],\"type\":\"Polygon\"},{\"coordinates\":[],\"type\":\"MultiPolygon\"},{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]],\"type\":\"Polygon\"}],\"type\":\"GeometryCollection\"},\"id\":\"GW001\",\"type\":\"Feature\",\"properties\":null},{\"bbox\":[-32,147.5,-29.5,151],\"geometry\":{\"geometries\":[{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]],\"type\":\"MultiLineString\"},{\"coordinates\":[],\"type\":\"MultiLineString\"},{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]],[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]],\"type\":\"MultiLineString\"},{\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]],\"type\":\"LineString\"},{\"coordinates\":[[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]],[]],\"type\":\"MultiPolygon\"},{\"coordinates\":[[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]]],\"type\":\"MultiPolygon\"},{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]],\"type\":\"Polygon\"},{\"coordinates\":[],\"type\":\"MultiPolygon\"},{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]],\"type\":\"Polygon\"}],\"type\":\"GeometryCollection\"},\"id\":\"GW001\",\"type\":\"Feature\",\"properties\":{\"depth\":5,\"comment\":\"Bore run over by dump truck\"}}],\"type\":\"FeatureCollection\"}"

bigAssFeatureCollectionWithNoBBox :: GeoFeatureCollection Aeson.Value
bigAssFeatureCollectionWithNoBBox = GeoFeatureCollection Nothing features


emptyFeatureCollectionJSON :: BS.ByteString
emptyFeatureCollectionJSON = "{\"type\":\"FeatureCollection\",\"features\":[]}"

emptyFeatureCollection :: GeoFeatureCollection Aeson.Value
emptyFeatureCollection = GeoFeatureCollection Nothing Vector.empty


emptyFeatureCollectionWithBBoxJSON :: BS.ByteString
emptyFeatureCollectionWithBBoxJSON = "{\"type\":\"FeatureCollection\",\"features\":[],\"bbox\":[-32,147.5,-29.5,151]}"

emptyFeatureCollectionWithBBox :: GeoFeatureCollection Aeson.Value
emptyFeatureCollectionWithBBox = GeoFeatureCollection bbox Vector.empty
