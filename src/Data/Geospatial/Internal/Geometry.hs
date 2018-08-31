{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
-------------------------------------------------------------------
-- |
-- Module       : Data.Geospatial.Internal.Geometry
-- Copyright    : (C) 2014-2018 HS-GeoJSON Project
-- License      : BSD-style (see the file LICENSE.md)
-- Maintainer   : Andrew Newman
--
-- See section 2.1 "Geometry Objects" in the GeoJSON Spec.
--
-------------------------------------------------------------------
module Data.Geospatial.Internal.Geometry (
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

import           Data.Geospatial.Internal.Geometry.GeoLine
import           Data.Geospatial.Internal.Geometry.GeoMultiLine
import           Data.Geospatial.Internal.Geometry.GeoMultiPoint
import           Data.Geospatial.Internal.Geometry.GeoMultiPolygon
import           Data.Geospatial.Internal.Geometry.GeoPoint
import           Data.Geospatial.Internal.Geometry.GeoPolygon

import           Control.Applicative                               ((<$>))
import           Control.Lens                                      (makePrisms)
import           Control.Monad                                     (mzero)
import           Data.Aeson                                        (FromJSON (..),
                                                                    ToJSON (..),
                                                                    Value (..),
                                                                    object,
                                                                    (.:), (.=))
import           Data.Aeson.Types                                  (Parser)
import           Data.Text                                         (Text)
import qualified Data.Vector                                       as Vector


-- | See section 2.1 /Geometry Objects/ in the GeoJSON Spec.
data GeospatialGeometry =
        NoGeometry
    |   Point GeoPoint
    |   MultiPoint GeoMultiPoint
    |   Polygon GeoPolygon
    |   MultiPolygon GeoMultiPolygon
    |   Line GeoLine
    |   MultiLine GeoMultiLine
    |   Collection (Vector.Vector GeospatialGeometry) deriving (Show, Eq)

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
-- encodes Geometry Objects to GeoJSON
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
-- decodes Geometry Objects from GeoJSON
--
-- Aeson doesnt decode "null" into `Null` unfortunately
--
instance FromJSON GeospatialGeometry where
--  parseJSON :: Value -> Parser a
    parseJSON Null = return NoGeometry
    parseJSON (Object obj) = do
        geometryType <- obj .: ("type" :: Text)
        geometryFromAeson geometryType (Object obj)
    parseJSON _ = mzero
