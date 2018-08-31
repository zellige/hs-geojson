-------------------------------------------------------------------
-- |
-- Module       : Data.Geospatial.Internal.GeoPosition
-- Copyright    : (C) 2014-2018 HS-GeoJSON Project
-- License      : BSD-style (see the file LICENSE.md)
-- Maintainer   : Andrew Newman
--
-- see Section 2.1.1 /Position/ in the GeoJSON Spec
-------------------------------------------------------------------
module Data.Geospatial.Internal.GeoPosition (
    -- * Type
        GeoPosition(..)
    -- * Functions
    ,   stripCRSFromPosition
    ) where

import qualified Data.Vector.Storable                as VectorStorable

import qualified Data.Geospatial.Internal.BasicTypes as BasicTypes

-- | see Section 2.1.1 /Position/ in the GeoJSON Spec,
-- I make the assumption here that the only position types we will use will
-- involve easting or northing (+ve or -ve Altitude) or lon or lat (+ve or -ve Altitude)
data GeoPosition =
        LonLat BasicTypes.Longitude BasicTypes.Latitude
    |   LonLatAlt BasicTypes.Longitude BasicTypes.Latitude BasicTypes.Altitude
    |   EastingNorthing BasicTypes.Easting BasicTypes.Northing
    |   EastingNorthingAlt BasicTypes.Easting BasicTypes.Northing BasicTypes.Altitude

-- | the `GeoPosition` is a bit special in that when you convert it to GeoJSON,
-- it will lose the CRS info attached to it and cannot be read back in
-- from the GeoJSON.  Hence it is ineligible for the `FromJSON` type class,
-- so this function will strip it down to a `GeoPositionWithoutCRS`, which is eligible
stripCRSFromPosition :: GeoPosition -> BasicTypes.GeoPositionWithoutCRS
stripCRSFromPosition (LonLat lon lat)                           = BasicTypes.GeoPositionWithoutCRS $ VectorStorable.fromList [lon, lat]
stripCRSFromPosition (LonLatAlt lon lat alt)                    = BasicTypes.GeoPositionWithoutCRS $ VectorStorable.fromList [lon, lat, alt]
stripCRSFromPosition (EastingNorthing easting northing)         = BasicTypes.GeoPositionWithoutCRS $ VectorStorable.fromList [easting, northing]
stripCRSFromPosition (EastingNorthingAlt easting northing alt)  = BasicTypes.GeoPositionWithoutCRS $ VectorStorable.fromList [easting, northing, alt]


