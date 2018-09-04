{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

-------------------------------------------------------------------
-- |
-- Module       : Data.Geospatial.Internal.BasicTypes
-- Copyright    : (C) 2014-2018 HS-GeoJSON Project
-- License      : BSD-style (see the file LICENSE.md)
-- Maintainer   : Andrew Newman
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
    ,   GeoPositionWithoutCRS (..)
    ,   DoubleArray (..)
    ,   LinearRingDouble (..)
    ,   VectorLinearRingDouble (..)
    -- * CRS Reference types
    ,   Name
    ,   Code
    ,   Href
    ,   FormatString
    ,   ProjectionType
    -- * Feature Types
    ,   BoundingBoxWithoutCRS (..)
    ,   FeatureID (..)
    ) where

import qualified Data.Aeson           as Aeson
import qualified Data.Aeson.Types     as AesonTypes
import qualified Data.LinearRing      as LinearRing
import qualified Data.Scientific      as Scientific
import qualified Data.Text            as Text
import qualified Data.Vector          as Vector
import qualified Data.Vector.Storable as VectorStorable
import           GHC.Generics

type Latitude = Double
type Longitude = Double
type Easting = Double
type Northing = Double
type Altitude = Double

newtype DoubleArray = DoubleArray [Double] deriving (Eq, Show, Generic, Aeson.FromJSON, Aeson.ToJSON)

newtype LinearRingDouble = LinearRingDouble (Vector.Vector (LinearRing.LinearRing DoubleArray)) deriving (Eq, Show, Generic, Aeson.FromJSON, Aeson.ToJSON)

newtype VectorLinearRingDouble = VectorLinearRingDouble (Vector.Vector (Vector.Vector (LinearRing.LinearRing DoubleArray))) deriving (Eq, Show, Generic, Aeson.FromJSON, Aeson.ToJSON)


-- | (`GeoPositionWithoutCRS` is a catch all for indeterminate CRSs and for expression of positions
-- before a CRS has been determined
--
newtype GeoPositionWithoutCRS = GeoPositionWithoutCRS { unGeoPosition :: VectorStorable.Vector Double } deriving (Eq, Show)

instance Aeson.FromJSON GeoPositionWithoutCRS where
    parseJSON obj = do
        doubles <- Aeson.parseJSON obj :: AesonTypes.Parser [Double]
        pure . GeoPositionWithoutCRS $ VectorStorable.fromList doubles

instance Aeson.ToJSON GeoPositionWithoutCRS where
    toJSON = Aeson.toJSON . VectorStorable.toList . unGeoPosition

type Name = Text.Text
type Code = Int
type Href = Text.Text
type FormatString = Text.Text
type ProjectionType = Text.Text

-- Feature Types

data FeatureID =
        FeatureIDText Text.Text
    |   FeatureIDNumber Int deriving (Show, Eq)

instance Aeson.FromJSON FeatureID where
    parseJSON (Aeson.Number nID) =
        case x of
            Nothing -> fail "Not an integer value"
            Just z  -> pure $ FeatureIDNumber z
        where
            x = Scientific.toBoundedInteger nID :: Maybe Int
    parseJSON (Aeson.String sID) = pure $ FeatureIDText sID
    parseJSON _                  = fail "unknown id type"


instance Aeson.ToJSON FeatureID where
    toJSON (FeatureIDText a)   = Aeson.String a
    toJSON (FeatureIDNumber b) = Aeson.Number (fromInteger $ toInteger b :: Scientific.Scientific)


-- | See Section 4 /Bounding Boxes/ of the GeoJSON spec,
-- The length of the list/array must be 2*n where n is the dimensionality of the position type for the CRS
-- with min values first followed by the max values, wich both the min/max sets following the same axis order as the CRS,
-- e.g for WGS84: minLongitude, minLatitude, maxLongitude, maxLatitude
-- The spec mentions that it can be part of a geometry object too but doesnt give an example,
-- This implementation will ignore bboxes on Geometry objects, they can be added if required.
newtype BoundingBoxWithoutCRS = BoundingBoxWithoutCRS { unBoundingBoxWithoutCrs :: VectorStorable.Vector Double } deriving (Eq, Show)

instance Aeson.FromJSON BoundingBoxWithoutCRS where
    parseJSON obj = do
        doubles <- Aeson.parseJSON obj :: AesonTypes.Parser [Double]
        pure . BoundingBoxWithoutCRS $ VectorStorable.fromList doubles

instance Aeson.ToJSON BoundingBoxWithoutCRS where
    toJSON = Aeson.toJSON . VectorStorable.toList . unBoundingBoxWithoutCrs
