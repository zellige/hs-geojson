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
    ,   GeoMultiPoint(..)
    ,   GeoPolygon(..)
    ,   GeoMultiPolygon(..)
    ,   GeoLine(..)
    ,   GeoMultiLine(..)
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
    ,   unGeoPoint
    ,   unGeoMultiPoint
    ,   unGeoPolygon
    ,   unGeoLine
    ,   unGeoMultiLine
    ) where

import Data.Geospatial.BasicTypes
import Data.Geospatial.CRS
import Data.Geospatial.GeoFeature
import Data.Geospatial.Geometry
import Data.Geospatial.GeoPosition

import Control.Lens ( makeLenses )
import Text.JSON
