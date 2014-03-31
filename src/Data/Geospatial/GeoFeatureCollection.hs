{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, TemplateHaskell #-}
-------------------------------------------------------------------
-- |
-- Module       : Data.Geospatial.GeoFeature
-- Copyright    : (C) 2014 Dom De Re
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-- See Section 2.3 /Feature Collection Objects/ of the GeoJSON spec
--
-------------------------------------------------------------------
module Data.Geospatial.GeoFeatureCollection (
    -- * Types
        GeoFeatureCollection(..)
    -- * Lenses
    ,   boundingbox
    ,   geofeatures
    ) where

import Data.Geospatial.BasicTypes
import Data.Geospatial.GeoFeature
import Data.Geospatial.Geometry
import Data.Geospatial.Geometry.Aeson
import Data.Geospatial.GeoPosition

import Prelude ( Show, Eq(..), ($) )
import Control.Applicative ( (<$>), (<*>) )
import Control.Lens ( makeLenses )
import Control.Monad ( mzero, return )
import Data.Aeson ( FromJSON(..), ToJSON(..), Value(..), Object, (.:), (.:?), (.=), object )
import Data.List ( (++) )
import Data.Maybe ( Maybe(..) )
import Data.Text ( Text )

-- $setup
--
-- >>> import qualified Data.Aeson as A
-- >>> import qualified Data.ByteString.Lazy.Char8 as BS
-- >>> import Data.Function ( (.) )
-- >>> import Data.Int ( Int )
-- >>> import Data.Maybe ( Maybe(..) )
-- >>> import Data.String
-- >>> import qualified Data.Text as T
--
-- >>> let decode' = A.decode . BS.pack; decode' :: (FromJSON a) => String -> Maybe a
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
-- >>> let testProperties = object [(T.pack "depth") .= (5 :: Int), (T.pack "comment") .= (T.pack "Bore run over by dump truck")]
--
-- Test Features
-- >>> let bigFeatureJSON = "{\"type\":\"Feature\",\"properties\":{\"depth\":5,\"comment\":\"Bore run over by dump truck\"},\"geometry\":{\"type\":\"GeometryCollection\",\"geometries\":[{\"type\":\"MultiLine\",\"coordinates\":[{\"type\":\"Line\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]}]},{\"type\":\"MultiLine\",\"coordinates\":[]},{\"type\":\"Line\",\"coordinates\":[]},{\"type\":\"MultiLine\",\"coordinates\":[{\"type\":\"Line\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]},{\"type\":\"Line\",\"coordinates\":[]}]},{\"type\":\"Line\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]},{\"type\":\"MultiPolygon\",\"coordinates\":[{\"type\":\"Polygon\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]},{\"type\":\"Polygon\",\"coordinates\":[]}]},{\"type\":\"MultiPolygon\",\"coordinates\":[{\"type\":\"Polygon\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]}]},{\"type\":\"Polygon\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]},{\"type\":\"MultiPolygon\",\"coordinates\":[]},{\"type\":\"Polygon\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]}]},\"bbox\":[-32,147.5,-29.5,151],\"id\":\"GW001\"}"
-- >>> let bigFeature = GeoFeature (Just testLatLonBBox) bigassCollection testProperties (Just "GW001")
-- >>> let featureWithNoPropertiesJSON = "{\"type\":\"Feature\",\"properties\":null,\"geometry\":{\"type\":\"GeometryCollection\",\"geometries\":[{\"type\":\"MultiLine\",\"coordinates\":[{\"type\":\"Line\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]}]},{\"type\":\"MultiLine\",\"coordinates\":[]},{\"type\":\"Line\",\"coordinates\":[]},{\"type\":\"MultiLine\",\"coordinates\":[{\"type\":\"Line\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]},{\"type\":\"Line\",\"coordinates\":[]}]},{\"type\":\"Line\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]},{\"type\":\"MultiPolygon\",\"coordinates\":[{\"type\":\"Polygon\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]},{\"type\":\"Polygon\",\"coordinates\":[]}]},{\"type\":\"MultiPolygon\",\"coordinates\":[{\"type\":\"Polygon\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]}]},{\"type\":\"Polygon\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]},{\"type\":\"MultiPolygon\",\"coordinates\":[]},{\"type\":\"Polygon\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]}]},\"bbox\":[-32,147.5,-29.5,151],\"id\":\"GW001\"}"
-- >>> let featureWithNoProperties = let GeoFeature bbox geometry _ featureId = bigFeature in GeoFeature bbox geometry Null featureId
-- >>> let featureWithNoGeometryJSON = "{\"type\":\"Feature\",\"properties\":{\"depth\":5,\"comment\":\"Bore run over by dump truck\"},\"geometry\":null,\"bbox\":[-32,147.5,-29.5,151],\"id\":\"GW001\"}"
-- >>> let featureWithNoGeometry = let GeoFeature bbox _ props featureId = bigFeature in GeoFeature bbox NoGeometry props featureId
-- >>> let featureWithNoIdJSON = "{\"type\":\"Feature\",\"properties\":{\"depth\":5,\"comment\":\"Bore run over by dump truck\"},\"geometry\":{\"type\":\"GeometryCollection\",\"geometries\":[{\"type\":\"MultiLine\",\"coordinates\":[{\"type\":\"Line\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]}]},{\"type\":\"MultiLine\",\"coordinates\":[]},{\"type\":\"Line\",\"coordinates\":[]},{\"type\":\"MultiLine\",\"coordinates\":[{\"type\":\"Line\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]},{\"type\":\"Line\",\"coordinates\":[]}]},{\"type\":\"Line\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]},{\"type\":\"MultiPolygon\",\"coordinates\":[{\"type\":\"Polygon\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]},{\"type\":\"Polygon\",\"coordinates\":[]}]},{\"type\":\"MultiPolygon\",\"coordinates\":[{\"type\":\"Polygon\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]}]},{\"type\":\"Polygon\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]},{\"type\":\"MultiPolygon\",\"coordinates\":[]},{\"type\":\"Polygon\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]}]},\"bbox\":[-32,147.5,-29.5,151]}"
-- >>> let featureWithNoId = let GeoFeature bbox geometry props _ = bigFeature in GeoFeature bbox geometry props Nothing
-- >>> let featureWithNoBBoxJSON = "{\"type\":\"Feature\",\"properties\":{\"depth\":5,\"comment\":\"Bore run over by dump truck\"},\"geometry\":{\"type\":\"GeometryCollection\",\"geometries\":[{\"type\":\"MultiLine\",\"coordinates\":[{\"type\":\"Line\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]}]},{\"type\":\"MultiLine\",\"coordinates\":[]},{\"type\":\"Line\",\"coordinates\":[]},{\"type\":\"MultiLine\",\"coordinates\":[{\"type\":\"Line\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]},{\"type\":\"Line\",\"coordinates\":[]}]},{\"type\":\"Line\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]},{\"type\":\"MultiPolygon\",\"coordinates\":[{\"type\":\"Polygon\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]},{\"type\":\"Polygon\",\"coordinates\":[]}]},{\"type\":\"MultiPolygon\",\"coordinates\":[{\"type\":\"Polygon\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]}]},{\"type\":\"Polygon\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]},{\"type\":\"MultiPolygon\",\"coordinates\":[]},{\"type\":\"Polygon\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]}]},\"id\":\"GW001\"}"
-- >>> let featureWithNoBBox = let GeoFeature _ geometry props featureId = bigFeature in GeoFeature Nothing geometry props featureId
-- >>> let bigAssFeatureCollectionJSON = "{\"bbox\":[-32,147.5,-29.5,151],\"features\":[{\"geometry\":{\"geometries\":[{\"coordinates\":[{\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]],\"type\":\"Line\"}],\"type\":\"MultiLine\"},{\"coordinates\":[],\"type\":\"MultiLine\"},{\"coordinates\":[],\"type\":\"Line\"},{\"coordinates\":[{\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]],\"type\":\"Line\"},{\"coordinates\":[],\"type\":\"Line\"}],\"type\":\"MultiLine\"},{\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]],\"type\":\"Line\"},{\"coordinates\":[{\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]],\"type\":\"Polygon\"},{\"coordinates\":[],\"type\":\"Polygon\"}],\"type\":\"MultiPolygon\"},{\"coordinates\":[{\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]],\"type\":\"Polygon\"}],\"type\":\"MultiPolygon\"},{\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]],\"type\":\"Polygon\"},{\"coordinates\":[],\"type\":\"MultiPolygon\"},{\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]],\"type\":\"Polygon\"}],\"type\":\"GeometryCollection\"},\"id\":\"GW001\",\"type\":\"Feature\",\"properties\":{\"depth\":5,\"comment\":\"Bore run over by dump truck\"}},{\"bbox\":[-32,147.5,-29.5,151],\"geometry\":null,\"id\":\"GW001\",\"type\":\"Feature\",\"properties\":{\"depth\":5,\"comment\":\"Bore run over by dump truck\"}},{\"geometry\":{\"geometries\":[{\"coordinates\":[{\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]],\"type\":\"Line\"}],\"type\":\"MultiLine\"},{\"coordinates\":[],\"type\":\"MultiLine\"},{\"coordinates\":[],\"type\":\"Line\"},{\"coordinates\":[{\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]],\"type\":\"Line\"},{\"coordinates\":[],\"type\":\"Line\"}],\"type\":\"MultiLine\"},{\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]],\"type\":\"Line\"},{\"coordinates\":[{\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]],\"type\":\"Polygon\"},{\"coordinates\":[],\"type\":\"Polygon\"}],\"type\":\"MultiPolygon\"},{\"coordinates\":[{\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]],\"type\":\"Polygon\"}],\"type\":\"MultiPolygon\"},{\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]],\"type\":\"Polygon\"},{\"coordinates\":[],\"type\":\"MultiPolygon\"},{\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]],\"type\":\"Polygon\"}],\"type\":\"GeometryCollection\"},\"id\":\"GW001\",\"type\":\"Feature\",\"properties\":{\"depth\":5,\"comment\":\"Bore run over by dump truck\"}},{\"bbox\":[-32,147.5,-29.5,151],\"geometry\":{\"geometries\":[{\"coordinates\":[{\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]],\"type\":\"Line\"}],\"type\":\"MultiLine\"},{\"coordinates\":[],\"type\":\"MultiLine\"},{\"coordinates\":[],\"type\":\"Line\"},{\"coordinates\":[{\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]],\"type\":\"Line\"},{\"coordinates\":[],\"type\":\"Line\"}],\"type\":\"MultiLine\"},{\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]],\"type\":\"Line\"},{\"coordinates\":[{\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]],\"type\":\"Polygon\"},{\"coordinates\":[],\"type\":\"Polygon\"}],\"type\":\"MultiPolygon\"},{\"coordinates\":[{\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]],\"type\":\"Polygon\"}],\"type\":\"MultiPolygon\"},{\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]],\"type\":\"Polygon\"},{\"coordinates\":[],\"type\":\"MultiPolygon\"},{\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]],\"type\":\"Polygon\"}],\"type\":\"GeometryCollection\"},\"type\":\"Feature\",\"properties\":{\"depth\":5,\"comment\":\"Bore run over by dump truck\"}},{\"bbox\":[-32,147.5,-29.5,151],\"geometry\":{\"geometries\":[{\"coordinates\":[{\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]],\"type\":\"Line\"}],\"type\":\"MultiLine\"},{\"coordinates\":[],\"type\":\"MultiLine\"},{\"coordinates\":[],\"type\":\"Line\"},{\"coordinates\":[{\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]],\"type\":\"Line\"},{\"coordinates\":[],\"type\":\"Line\"}],\"type\":\"MultiLine\"},{\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]],\"type\":\"Line\"},{\"coordinates\":[{\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]],\"type\":\"Polygon\"},{\"coordinates\":[],\"type\":\"Polygon\"}],\"type\":\"MultiPolygon\"},{\"coordinates\":[{\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]],\"type\":\"Polygon\"}],\"type\":\"MultiPolygon\"},{\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]],\"type\":\"Polygon\"},{\"coordinates\":[],\"type\":\"MultiPolygon\"},{\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]],\"type\":\"Polygon\"}],\"type\":\"GeometryCollection\"},\"id\":\"GW001\",\"type\":\"Feature\",\"properties\":null},{\"bbox\":[-32,147.5,-29.5,151],\"geometry\":{\"geometries\":[{\"coordinates\":[{\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]],\"type\":\"Line\"}],\"type\":\"MultiLine\"},{\"coordinates\":[],\"type\":\"MultiLine\"},{\"coordinates\":[],\"type\":\"Line\"},{\"coordinates\":[{\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]],\"type\":\"Line\"},{\"coordinates\":[],\"type\":\"Line\"}],\"type\":\"MultiLine\"},{\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]],\"type\":\"Line\"},{\"coordinates\":[{\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]],\"type\":\"Polygon\"},{\"coordinates\":[],\"type\":\"Polygon\"}],\"type\":\"MultiPolygon\"},{\"coordinates\":[{\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]],\"type\":\"Polygon\"}],\"type\":\"MultiPolygon\"},{\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]],\"type\":\"Polygon\"},{\"coordinates\":[],\"type\":\"MultiPolygon\"},{\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]],\"type\":\"Polygon\"}],\"type\":\"GeometryCollection\"},\"id\":\"GW001\",\"type\":\"Feature\",\"properties\":{\"depth\":5,\"comment\":\"Bore run over by dump truck\"}}],\"type\":\"FeatureCollection\"}"
-- >>> let bigAssFeatureCollection = GeoFeatureCollection (Just testLatLonBBox) [featureWithNoBBox, featureWithNoGeometry, featureWithNoBBox, featureWithNoId, featureWithNoProperties, bigFeature]
-- >>> let bigAssFeatureCollectionWithNoBBoxJSON = "{\"type\":\"FeatureCollection\",\"features\":[{\"type\":\"Feature\",\"properties\":{\"depth\":5,\"comment\":\"Bore run over by dump truck\"},\"geometry\":{\"type\":\"GeometryCollection\",\"geometries\":[{\"type\":\"MultiLine\",\"coordinates\":[{\"type\":\"Line\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]}]},{\"type\":\"MultiLine\",\"coordinates\":[]},{\"type\":\"Line\",\"coordinates\":[]},{\"type\":\"MultiLine\",\"coordinates\":[{\"type\":\"Line\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]},{\"type\":\"Line\",\"coordinates\":[]}]},{\"type\":\"Line\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]},{\"type\":\"MultiPolygon\",\"coordinates\":[{\"type\":\"Polygon\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]},{\"type\":\"Polygon\",\"coordinates\":[]}]},{\"type\":\"MultiPolygon\",\"coordinates\":[{\"type\":\"Polygon\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]}]},{\"type\":\"Polygon\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]},{\"type\":\"MultiPolygon\",\"coordinates\":[]},{\"type\":\"Polygon\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]}]},\"id\":\"GW001\"},{\"type\":\"Feature\",\"properties\":{\"depth\":5,\"comment\":\"Bore run over by dump truck\"},\"geometry\":null,\"bbox\":[-32,147.5,-29.5,151],\"id\":\"GW001\"},{\"type\":\"Feature\",\"properties\":{\"depth\":5,\"comment\":\"Bore run over by dump truck\"},\"geometry\":{\"type\":\"GeometryCollection\",\"geometries\":[{\"type\":\"MultiLine\",\"coordinates\":[{\"type\":\"Line\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]}]},{\"type\":\"MultiLine\",\"coordinates\":[]},{\"type\":\"Line\",\"coordinates\":[]},{\"type\":\"MultiLine\",\"coordinates\":[{\"type\":\"Line\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]},{\"type\":\"Line\",\"coordinates\":[]}]},{\"type\":\"Line\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]},{\"type\":\"MultiPolygon\",\"coordinates\":[{\"type\":\"Polygon\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]},{\"type\":\"Polygon\",\"coordinates\":[]}]},{\"type\":\"MultiPolygon\",\"coordinates\":[{\"type\":\"Polygon\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]}]},{\"type\":\"Polygon\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]},{\"type\":\"MultiPolygon\",\"coordinates\":[]},{\"type\":\"Polygon\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]}]},\"id\":\"GW001\"},{\"type\":\"Feature\",\"properties\":{\"depth\":5,\"comment\":\"Bore run over by dump truck\"},\"geometry\":{\"type\":\"GeometryCollection\",\"geometries\":[{\"type\":\"MultiLine\",\"coordinates\":[{\"type\":\"Line\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]}]},{\"type\":\"MultiLine\",\"coordinates\":[]},{\"type\":\"Line\",\"coordinates\":[]},{\"type\":\"MultiLine\",\"coordinates\":[{\"type\":\"Line\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]},{\"type\":\"Line\",\"coordinates\":[]}]},{\"type\":\"Line\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]},{\"type\":\"MultiPolygon\",\"coordinates\":[{\"type\":\"Polygon\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]},{\"type\":\"Polygon\",\"coordinates\":[]}]},{\"type\":\"MultiPolygon\",\"coordinates\":[{\"type\":\"Polygon\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]}]},{\"type\":\"Polygon\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]},{\"type\":\"MultiPolygon\",\"coordinates\":[]},{\"type\":\"Polygon\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]}]},\"bbox\":[-32,147.5,-29.5,151]},{\"type\":\"Feature\",\"properties\":null,\"geometry\":{\"type\":\"GeometryCollection\",\"geometries\":[{\"type\":\"MultiLine\",\"coordinates\":[{\"type\":\"Line\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]}]},{\"type\":\"MultiLine\",\"coordinates\":[]},{\"type\":\"Line\",\"coordinates\":[]},{\"type\":\"MultiLine\",\"coordinates\":[{\"type\":\"Line\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]},{\"type\":\"Line\",\"coordinates\":[]}]},{\"type\":\"Line\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]},{\"type\":\"MultiPolygon\",\"coordinates\":[{\"type\":\"Polygon\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]},{\"type\":\"Polygon\",\"coordinates\":[]}]},{\"type\":\"MultiPolygon\",\"coordinates\":[{\"type\":\"Polygon\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]}]},{\"type\":\"Polygon\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]},{\"type\":\"MultiPolygon\",\"coordinates\":[]},{\"type\":\"Polygon\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]}]},\"bbox\":[-32,147.5,-29.5,151],\"id\":\"GW001\"},{\"type\":\"Feature\",\"properties\":{\"depth\":5,\"comment\":\"Bore run over by dump truck\"},\"geometry\":{\"type\":\"GeometryCollection\",\"geometries\":[{\"type\":\"MultiLine\",\"coordinates\":[{\"type\":\"Line\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]}]},{\"type\":\"MultiLine\",\"coordinates\":[]},{\"type\":\"Line\",\"coordinates\":[]},{\"type\":\"MultiLine\",\"coordinates\":[{\"type\":\"Line\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]},{\"type\":\"Line\",\"coordinates\":[]}]},{\"type\":\"Line\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]},{\"type\":\"MultiPolygon\",\"coordinates\":[{\"type\":\"Polygon\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]},{\"type\":\"Polygon\",\"coordinates\":[]}]},{\"type\":\"MultiPolygon\",\"coordinates\":[{\"type\":\"Polygon\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]}]},{\"type\":\"Polygon\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]},{\"type\":\"MultiPolygon\",\"coordinates\":[]},{\"type\":\"Polygon\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]}]},\"bbox\":[-32,147.5,-29.5,151],\"id\":\"GW001\"}]}"
-- >>> let bigAssFeatureCollectionWithNoBBox = let GeoFeatureCollection _ features = bigAssFeatureCollection in GeoFeatureCollection Nothing features
-- >>> let emptyFeatureCollectionJSON = "{\"type\":\"FeatureCollection\",\"features\":[]}"
-- >>> let emptyFeatureCollection = GeoFeatureCollection Nothing [] :: GeoFeatureCollection Value
-- >>> let emptyFeatureCollectionWithBBoxJSON = "{\"type\":\"FeatureCollection\",\"features\":[],\"bbox\":[-32,147.5,-29.5,151]}"
-- >>> let emptyFeatureCollectionWithBBox = GeoFeatureCollection (Just testLatLonBBox) []; emptyFeatureCollectionWithBBox :: GeoFeatureCollection Value
--

-- | See Section 2.3 /Feature Collection Objects/ of the GeoJSON spec
--
data GeoFeatureCollection a = GeoFeatureCollection
    {   _boundingbox :: Maybe BoundingBoxWithoutCRS
    ,   _geofeatures :: [GeoFeature a]
    } deriving (Show, Eq)

makeLenses ''GeoFeatureCollection

-- instances

-- | Decodes FeatureCollection objects to and from GeoJSON
--
-- >>> decode' bigAssFeatureCollectionJSON == Just bigAssFeatureCollection
-- True
--
-- >>> decode' bigAssFeatureCollectionWithNoBBoxJSON == Just bigAssFeatureCollectionWithNoBBox
-- True
--
-- >>> decode' emptyFeatureCollectionWithBBoxJSON == Just emptyFeatureCollectionWithBBox
-- True
--
-- >>> decode' emptyFeatureCollectionJSON == Just emptyFeatureCollection
-- True
--
instance (FromJSON a) => FromJSON (GeoFeatureCollection a) where
--  parseJSON :: Value -> Parse a
    parseJSON (Object obj) = do
        objType <- obj .: ("type" :: Text)
        if objType /= ("FeatureCollection" :: Text)
            then
                mzero
            else
                GeoFeatureCollection
                    <$> obj .:? ("bbox" :: Text)
                    <*> obj .: ("features" :: Text)
    parseJSON _ = mzero

-- | Encodes FeatureCollection objects to and from GeoJSON
--
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
instance (ToJSON a) => ToJSON (GeoFeatureCollection a) where
--  toJSON :: a -> Value
    toJSON (GeoFeatureCollection bbox' features) = object $ baseAttributes ++ optAttributes "bbox" bbox'
        where
            baseAttributes = ["type" .= ("FeatureCollection" :: Text), "features" .= features]
