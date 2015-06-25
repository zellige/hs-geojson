-------------------------------------------------------------------
-- |
-- Module       : Data.Geospatial
-- Copyright    : (C) 2014 Dom De Re
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-- Refer to the GeoJSON Spec http://www.geojson.org/geojson-spec.html
--
-------------------------------------------------------------------
module Data.Geospatial (
    -- * Types
        Latitude
    ,   Longitude
    ,   Easting
    ,   Northing
    ,   Altitude
    ,   GeoPositionWithoutCRS
    ,   GeoPosition(..)
    ,   GeoPoint(..)
    ,   GeoMultiPoint(..), splitGeoMultiPoint, mergeGeoPoints
    ,   GeoPolygon(..)
    ,   GeoMultiPolygon(..), splitGeoMultiPolygon, mergeGeoPolygons
    ,   GeoLine(..)
    ,   GeoMultiLine(..), splitGeoMultiLine, mergeGeoLines
    ,   GeospatialGeometry(..)
    ,   Name
    ,   Code
    ,   Href
    ,   FormatString
    ,   ProjectionType
    ,   CRSObject(..)
    ,   FeatureID
    ,   BoundingBoxWithoutCRS
    ,   GeoFeature(..)
    ,   GeoFeatureCollection(..)
    -- * Functions
    ,   stripCRSFromPosition
    ,   defaultCRS
    -- * Lenses
    -- ** Geometry Lenses
    ,   unGeoPoint
    ,   unGeoMultiPoint
    ,   unGeoPolygon
    ,   unGeoLine
    ,   unGeoMultiLine
    ,   unGeoMultiPolygon
    -- ** Feature Lenses
    ,   bbox
    ,   geometry
    ,   properties
    ,   featureId
    ,   boundingbox
    ,   geofeatures
    -- * Prisms
    -- ** Geometry
    ,   _NoGeometry
    ,   _Point
    ,   _MultiPoint
    ,   _Polygon
    ,   _MultiPolygon
    ,   _Line
    ,   _MultiLine
    ,   _Collection
    -- ** CRS
    ,   _NoCRS
    ,   _NamedCRS
    ,   _EPSG
    ,   _LinkedCRS
    ) where

import Data.Geospatial.BasicTypes
import Data.Geospatial.CRS
import Data.Geospatial.GeoFeature
import Data.Geospatial.GeoFeatureCollection
import Data.Geospatial.Geometry
import Data.Geospatial.GeoPosition
