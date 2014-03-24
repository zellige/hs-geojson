-------------------------------------------------------------------
-- |
-- Module       : Data.Geospatial.GeoPosition
-- Copyright    : (C) 2014 Dom De Re
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-- see Section 2.1.1 /Position/ in the GeoJSON Spec
-------------------------------------------------------------------
module Data.Geospatial.GeoPosition (
    -- * Type
        GeoPosition(..)
    -- * Functions
    ,   stripCRSFromPosition
    ) where

import Data.Geospatial.BasicTypes

-- | see Section 2.1.1 /Position/ in the GeoJSON Spec,
-- I make the assumption here that the only position types we will use will
-- involve easting or northing (+ve or -ve Altitude) or lon or lat (+ve or -ve Altitude)
data GeoPosition =
        LonLat Longitude Latitude
    |   LonLatAlt Longitude Latitude Altitude
    |   EastingNorthing Easting Northing
    |   EastingNorthingAlt Easting Northing Altitude

-- | the `GeoPosition` is a bit special in that when you convert it to GeoJSON,
-- it will lose the CRS info attached to it and cannot be read back in
-- from the GeoJSON.  Hence it is ineligible for the `FromJSON` type class,
-- so this function will strip it down to a `GeoPositionWithoutCRS`, which is eligible
stripCRSFromPosition :: GeoPosition -> GeoPositionWithoutCRS
stripCRSFromPosition (LonLat lon lat)                           = [lon, lat]
stripCRSFromPosition (LonLatAlt lon lat alt)                    = [lon, lat, alt]
stripCRSFromPosition (EastingNorthing easting northing)         = [easting, northing]
stripCRSFromPosition (EastingNorthingAlt easting northing alt)  = [easting, northing, alt]


