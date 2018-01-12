{-# LANGUAGE OverloadedStrings #-}

-------------------------------------------------------------------
-- |
-- Module       : Data.Geospatial.Internal.BasicTypes
-- Copyright    : (C) 2014 Dom De Re
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-- Basic types for GeoJSON representations.
-------------------------------------------------------------------
module Data.Geospatial.Internal.BasicTypes (
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
    ,   FeatureID (..)
    ) where

import           Data.Aeson      (FromJSON (..), ToJSON (..), Value (..))
import           Data.Scientific (Scientific, toBoundedInteger)
import           Data.Text       (Text)

type Latitude = Double
type Longitude = Double
type Easting = Double
type Northing = Double
type Altitude = Double

-- | (`GeoPositionWithoutCRS` is a catch all for indeterminate CRSs and for expression of positions
-- before a CRS has been determined
--
type GeoPositionWithoutCRS = [Double]

type Name = Text
type Code = Int
type Href = Text
type FormatString = Text
type ProjectionType = Text

-- Feature Types

data FeatureID =
        FeatureIDText Text
    |   FeatureIDNumber Int deriving (Show, Eq)

instance FromJSON FeatureID where
    parseJSON (Number nID) =
        case x of
            Nothing -> fail "Not an integer value"
            Just z  -> pure $ FeatureIDNumber z
        where
            x = toBoundedInteger nID :: Maybe Int
    parseJSON (String sID) = pure $ FeatureIDText sID
    parseJSON _            = fail "unknown id type"


instance ToJSON FeatureID where
    toJSON (FeatureIDText a)   = String a
    toJSON (FeatureIDNumber b) = Number (fromInteger $ toInteger b :: Scientific)


-- | See Section 4 /Bounding Boxes/ of the GeoJSON spec,
-- The length of the list/array must be 2*n where n is the dimensionality of the position type for the CRS
-- with min values first followed by the max values, wich both the min/max sets following the same axis order as the CRS,
-- e.g for WGS84: minLongitude, minLatitude, maxLongitude, maxLatitude
-- The spec mentions that it can be part of a geometry object too but doesnt give an example,
-- This implementation will ignore bboxes on Geometry objects, they can be added if required.
type BoundingBoxWithoutCRS = [Double]
