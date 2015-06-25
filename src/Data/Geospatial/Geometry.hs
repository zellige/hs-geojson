{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
-------------------------------------------------------------------
-- |
-- Module       : Data.Geospatial.Geometry
-- Copyright    : (C) 2014 Dom De Re
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-- See section 2.1 "Geometry Objects" in the GeoJSON Spec.
--
-------------------------------------------------------------------
module Data.Geospatial.Geometry (
    -- * Types
        GeoPoint(..)
    ,   GeoMultiPoint(..), splitGeoMultiPoint, mergeGeoPoints
    ,   GeoPolygon(..)
    ,   GeoMultiPolygon(..), splitGeoMultiPolygon, mergeGeoPolygons
    ,   GeoLine(..)
    ,   GeoMultiLine(..), splitGeoMultiLine, mergeGeoLines
    ,   GeospatialGeometry(..)
    -- * Lenses
    ,   unGeoPoint
    ,   unGeoMultiPoint
    ,   unGeoPolygon
    ,   unGeoMultiPolygon
    ,   unGeoLine
    ,   unGeoMultiLine
    -- * Prisms
    ,   _NoGeometry
    ,   _Point
    ,   _MultiPoint
    ,   _Polygon
    ,   _MultiPolygon
    ,   _Line
    ,   _MultiLine
    ,   _Collection
    ) where

import Data.Geospatial.Geometry.Aeson
import Data.Geospatial.Geometry.GeoLine
import Data.Geospatial.Geometry.GeoMultiLine
import Data.Geospatial.Geometry.GeoMultiPoint
import Data.Geospatial.Geometry.GeoMultiPolygon
import Data.Geospatial.Geometry.GeoPoint
import Data.Geospatial.Geometry.GeoPolygon

import Control.Applicative ( (<$>) )
import Control.Lens ( makePrisms )
import Control.Monad ( mzero )
import Data.Aeson
    (   FromJSON(..)
    ,   ToJSON(..)
    ,   Value(..)
    ,   Object
    ,   (.:)
    ,   (.=)
    ,   object
    )
import Data.Aeson.Types ( Parser )
import Data.Text ( Text )

-- $setup
--
-- >>> import Data.Geospatial.BasicTypes
-- >>> import Data.LinearRing
-- >>> import Data.LineString
-- >>> import Data.Geospatial.Geometry.GeoMultiPolygon (mergeGeoPolygons)
-- >>> import Data.Geospatial.Geometry.GeoMultiLine (mergeGeoLines)
--
-- >>> import Control.Monad ( return )
-- >>> import qualified Data.Aeson as A
-- >>> import qualified Data.ByteString.Lazy.Char8 as BS
--
-- >>> let lshapedPolyVertices = return (makeLinearRing [120.0, -15.0] [127.0, -15.0] [127.0, -25.0] [[124.0, -25.0], [124.0, -18.0], [120.0, -18.0]]) :: [LinearRing GeoPositionWithoutCRS]
-- >>> let lshapedPolyLineVertices = makeLineString [120.0, -15.0] [127.0, -15.0] [[127.0, -25.0], [124.0, -25.0], [124.0, -18.0], [120.0, -18.0]] :: LineString GeoPositionWithoutCRS
-- >>> let emptyVertices = [] :: [LinearRing GeoPositionWithoutCRS]
--
-- >>> let decode' = A.decode . BS.pack; decode' :: (FromJSON a) => String -> Maybe a
--
-- Test Geometry Data
-- Polys
-- >>> let lShapedPolyJSON = "{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]],\"type\":\"Polygon\"}"
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
-- >>> let singlePolyMultiPolyJSON = "{\"type\":\"MultiPolygon\",\"coordinates\":[[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]]]}"
-- >>> let singlePolyGeoMultiPoly = mergeGeoPolygons [lShapedGeoPoly]
-- >>> let singlePolyMultiPoly = MultiPolygon singlePolyGeoMultiPoly
-- >>> let multiPolyJSON = "{\"type\":\"MultiPolygon\",\"coordinates\":[[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]],[]]}"
-- >>> let geoMultiPoly = mergeGeoPolygons [lShapedGeoPoly, emptyGeoPoly]
-- >>> let multiPoly = MultiPolygon geoMultiPoly
--
-- Line Data
-- >>> let lShapedLineJSON = "{\"type\":\"Line\",\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]}"
-- >>> let lShapedGeoLine = GeoLine lshapedPolyLineVertices
-- >>> let lShapedLine = Line lShapedGeoLine
--
-- Multi Lines
-- >>> let emptyMultiLineJSON = "{\"type\":\"MultiLineString\",\"coordinates\":[]}"
-- >>> let emptyMultiGeoLine = GeoMultiLine []
-- >>> let emptyMultiLine = MultiLine emptyMultiGeoLine
-- >>> let singleLineMultiLineJSON = "{\"type\":\"MultiLineString\",\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]]}"
-- >>> let singleLineGeoMultiLine = mergeGeoLines [lShapedGeoLine]
-- >>> let singleLineMultiLine = MultiLine singleLineGeoMultiLine
-- >>> let multiLineJSON = "{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]],[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]],\"type\":\"MultiLineString\"}"
-- >>> let geoMultiLine = mergeGeoLines [lShapedGeoLine, lShapedGeoLine]
-- >>> let multiLine = MultiLine geoMultiLine
-- >>> let emptyCollectionJSON = "{\"type\":\"GeometryCollection\",\"geometries\":[]}"
-- >>> let emptyCollection = Collection []
-- >>> let bigassCollectionJSON = "{\"geometries\":[{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]],\"type\":\"MultiLineString\"},{\"coordinates\":[],\"type\":\"MultiLineString\"},{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]],[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]]],\"type\":\"MultiLineString\"},{\"coordinates\":[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18]],\"type\":\"LineString\"},{\"coordinates\":[[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]],[]],\"type\":\"MultiPolygon\"},{\"coordinates\":[[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]]],\"type\":\"MultiPolygon\"},{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]],\"type\":\"Polygon\"},{\"coordinates\":[],\"type\":\"MultiPolygon\"},{\"coordinates\":[[[120,-15],[127,-15],[127,-25],[124,-25],[124,-18],[120,-18],[120,-15]]],\"type\":\"Polygon\"}],\"type\":\"GeometryCollection\"}"
-- >>> let bigassCollection = Collection [singleLineMultiLine, emptyMultiLine, multiLine, lShapedLine, multiPoly, singlePolyMultiPoly, lShapedPoly, emptyMultiPoly, lShapedPoly]
--
-- End Test Geometry Data
--
--

-- | See section 2.1 /Geometry Objects/ in the GeoJSON Spec.
data GeospatialGeometry =
        NoGeometry
    |   Point GeoPoint
    |   MultiPoint GeoMultiPoint
    |   Polygon GeoPolygon
    |   MultiPolygon GeoMultiPolygon
    |   Line GeoLine
    |   MultiLine GeoMultiLine
    |   Collection [GeospatialGeometry] deriving (Show, Eq)

makePrisms ''GeospatialGeometry

geometryFromAeson :: String -> Value -> Parser GeospatialGeometry
geometryFromAeson "Point" obj                           = Point <$> parseJSON obj
geometryFromAeson "MultiPoint" obj                      = MultiPoint <$> parseJSON obj
geometryFromAeson "Polygon" obj                         = Polygon <$> parseJSON obj
geometryFromAeson "MultiPolygon" obj                    = MultiPolygon <$> parseJSON obj
geometryFromAeson "LineString" obj                      = Line <$> parseJSON obj
geometryFromAeson "MultiLineString" obj                 = MultiLine <$> parseJSON obj
geometryFromAeson "GeometryCollection" (Object jsonObj) = Collection <$> (jsonObj .: ("geometries" :: Text))
geometryFromAeson "GeometryCollection" _                = mzero
geometryFromAeson _ _                                   = mzero


-- |
-- encodes and Geometry Objects to GeoJSON
-- (refer to source to see the values for the test values)
--
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
instance ToJSON GeospatialGeometry where
--  toJSON :: a -> Value
    toJSON NoGeometry               = Null
    toJSON (Point point)            = toJSON point
    toJSON (MultiPoint points)      = toJSON points
    toJSON (Polygon vertices)       = toJSON vertices
    toJSON (MultiPolygon vertices)  = toJSON vertices
    toJSON (Line vertices)          = toJSON vertices
    toJSON (MultiLine vertices)     = toJSON vertices
    toJSON (Collection geometries)  = object
        [   "type" .= ("GeometryCollection" :: Text)
        ,   "geometries" .= geometries
        ]

-- |
-- encodes and decodes Geometry Objects to and from GeoJSON
-- (refer to source to see the values for the test values)
--
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
-- Aeson doesnt decode "null" into `Null` unfortunately
--
-- decode' "null" :: Maybe GeospatialGeometry
-- Just NoGeometry
--
instance FromJSON GeospatialGeometry where
--  parseJSON :: Value -> Parser a
    parseJSON Null = return NoGeometry
    parseJSON (Object obj) = do
        geometryType <- obj .: ("type" :: Text)
        geometryFromAeson geometryType (Object obj)
    parseJSON _ = mzero
