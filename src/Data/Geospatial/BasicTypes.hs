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
