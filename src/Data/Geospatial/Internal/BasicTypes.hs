{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

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
    ,   retrieveXY
    ,   PointXY (..)
    ,   PointXYZ (..)
    ,   PointXYZM (..)
    ,   DoubleArray (..)
    ,   HasGeoPositionWithoutCRS(..)
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

import           Control.DeepSeq
import           Control.Lens.TH  (makeClassy)
import qualified Data.Aeson       as Aeson
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.Foldable    as Foldable
import qualified Data.Maybe       as DataMaybe
import qualified Data.Scientific  as Scientific
import qualified Data.Sequence    as Sequence
import qualified Data.Text        as Text
import           GHC.Generics

type Latitude = Double
type Longitude = Double
type Easting = Double
type Northing = Double
type Altitude = Double

newtype DoubleArray = DoubleArray [Double] deriving (Eq, Show, Generic, NFData, Aeson.FromJSON, Aeson.ToJSON)

-- | (`GeoPositionWithoutCRS` is a catch all for indeterminate CRSs and for expression of positions
-- before a CRS has been determined
--
data PointXY = PointXY
    { _xyX :: !Double
    , _xyY :: !Double
    } deriving (Show, Eq, Generic, NFData)

data PointXYZ = PointXYZ
    { _xyzX :: !Double
    , _xyzY :: !Double
    , _xyzZ :: !Double
    } deriving (Show, Eq, Generic, NFData)

data PointXYZM = PointXYZM
    { _xyzmX :: !Double
    , _xyzmY :: !Double
    , _xyzmZ :: !Double
    , _xyzmM :: !Double
    } deriving (Show, Eq, Generic, NFData)

data GeoPositionWithoutCRS = GeoEmpty | GeoPointXY PointXY | GeoPointXYZ PointXYZ | GeoPointXYZM PointXYZM deriving (Show, Eq, Generic, NFData)

makeClassy ''GeoPositionWithoutCRS

_toDoubleArray :: GeoPositionWithoutCRS -> [Double]
_toDoubleArray GeoEmpty                           = []
_toDoubleArray (GeoPointXY (PointXY x y))         = [x, y]
_toDoubleArray (GeoPointXYZ (PointXYZ x y z))     = [x, y, z]
_toDoubleArray (GeoPointXYZM (PointXYZM x y z m)) = [x, y, z, m]

_toGeoPoint :: DoubleArray -> Maybe GeoPositionWithoutCRS
_toGeoPoint (DoubleArray [])           = Just GeoEmpty
_toGeoPoint (DoubleArray [x, y])       = Just $ GeoPointXY (PointXY x y)
_toGeoPoint (DoubleArray [x, y, z])    = Just $ GeoPointXYZ (PointXYZ x y z)
_toGeoPoint (DoubleArray [x, y, z, m]) = Just $ GeoPointXYZM (PointXYZM x y z m)
_toGeoPoint _                          = Nothing

retrieveXY :: GeoPositionWithoutCRS -> PointXY
retrieveXY position =
  case position of
    GeoEmpty                             -> undefined -- TODO - Fix - represent this like WKB - NaN value
    (GeoPointXY p)                       -> p
    (GeoPointXYZ (PointXYZ pX pY _))     -> PointXY pX pY
    (GeoPointXYZM (PointXYZM pX pY _ _)) -> PointXY pX pY
{-# INLINE retrieveXY #-}

-- instances

instance Aeson.ToJSON GeoPositionWithoutCRS where
  --  toJSON :: a -> Value
  toJSON a = Aeson.toJSON $ _toDoubleArray a

instance Aeson.FromJSON GeoPositionWithoutCRS where
--  parseJSON :: Value -> Parser a
  parseJSON o = do
    x <- Aeson.parseJSON o
    DataMaybe.maybe (fail "Illegal coordinates") pure (_toGeoPoint x)

type Name = Text.Text
type Code = Int
type Href = Text.Text
type FormatString = Text.Text
type ProjectionType = Text.Text

-- Feature Types

data FeatureID =
        FeatureIDText Text.Text
    |   FeatureIDNumber Int deriving (Show, Eq, Generic, NFData)

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
newtype BoundingBoxWithoutCRS = BoundingBoxWithoutCRS { unBoundingBoxWithoutCrs :: Sequence.Seq Double } deriving (Eq, Show, Generic, NFData)

instance Aeson.FromJSON BoundingBoxWithoutCRS where
    parseJSON obj = do
        doubles <- Aeson.parseJSON obj :: AesonTypes.Parser [Double]
        pure . BoundingBoxWithoutCRS $ Sequence.fromList doubles

instance Aeson.ToJSON BoundingBoxWithoutCRS where
    toJSON = Aeson.toJSON . Foldable.toList . unBoundingBoxWithoutCrs
