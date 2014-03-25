-------------------------------------------------------------------
-- |
-- Module       : Data.Geospatial.BasicTypes
-- Copyright    : (C) 2014 Dom De Re
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-- Basic types for GeoJSON representations.
-------------------------------------------------------------------
module Data.Geospatial.BasicTypes (
    -- * Coordinate types
        Latitude
    ,   Longitude
    ,   Easting
    ,   Northing
    ,   Altitude
    ,   GeoPositionWithoutCRS
    -- * CRS Reference types
    ,   Name
    ,   Code
    ,   Href
    ,   FormatString
    ,   ProjectionType
    -- * Feature Types
    ,   BoundingBoxWithoutCRS
    ,   FeatureID
    ) where

type Latitude = Float
type Longitude = Float
type Easting = Float
type Northing = Float
type Altitude = Float

-- | (`GeoPositionWithoutCRS` is a catch all for indeterminate CRSs and for expression of positions
-- before a CRS has been determined
--
type GeoPositionWithoutCRS = [Float]

type Name = String
type Code = Int
type Href = String
type FormatString = String
type ProjectionType = String

-- Feature Types

type FeatureID = String

-- | See Section 4 /Bounding Boxes/ of the GeoJSON spec,
-- The length of the list/array must be 2*n where n is the dimensionality of the position type for the CRS
-- with min values first followed by the max values, wich both the min/max sets following the same axis order as the CRS,
-- e.g for WGS84: minLongitude, minLatitude, maxLongitude, maxLatitude
-- The spec mentions that it can be part of a geometry object too but doesnt give an example,
-- This implementation will ignore bboxes on Geometry objects, they can be added if required.
type BoundingBoxWithoutCRS = [Float]


