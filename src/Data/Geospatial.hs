-------------------------------------------------------------------
-- |
-- Module       : Data.Geospatial
-- Copyright    : (C) 2014-2018 HS-GeoJSON Project
-- License      : BSD-style (see the file LICENSE.md)
-- Maintainer   : Andrew Newman
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
    ,   FeatureID(..)
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
    ,   unGeoPosition
    ,   unBoundingBoxWithoutCrs
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

import           Data.Geospatial.Internal.BasicTypes
import           Data.Geospatial.Internal.CRS
import           Data.Geospatial.Internal.GeoFeature
import           Data.Geospatial.Internal.GeoFeatureCollection
import           Data.Geospatial.Internal.Geometry
import           Data.Geospatial.Internal.GeoPosition
