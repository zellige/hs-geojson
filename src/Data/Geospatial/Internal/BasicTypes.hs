{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
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
import qualified Data.Maybe           as DataMaybe
import qualified Data.Scientific      as Scientific
import qualified Data.Text            as Text
import qualified Data.Vector.Storable as VectorStorable
import qualified Data.Word            as DataWord
import           Foreign.Storable
import           GHC.Generics

type Latitude = Double
type Longitude = Double
type Easting = Double
type Northing = Double
type Altitude = Double

newtype DoubleArray = DoubleArray [Double] deriving (Eq, Show, Generic, Aeson.FromJSON, Aeson.ToJSON)

-- | (`GeoPositionWithoutCRS` is a catch all for indeterminate CRSs and for expression of positions
-- before a CRS has been determined
--
data GeoPositionWithoutCRS =
  PointXY
    { _xyX :: !Double
    , _xyY :: !Double
    }
  | PointXYZ
    { _xyzX :: !Double
    , _xyzY :: !Double
    , _xyzZ :: !Double
    }
  | PointXYZM
    { _xyzX :: !Double
    , _xyzY :: !Double
    , _xyzZ :: !Double
    , _xyzM :: !Double
    }
  deriving (Show, Eq)

_toDoubleArray :: GeoPositionWithoutCRS -> [Double]
_toDoubleArray (PointXY x y)       = [x, y]
_toDoubleArray (PointXYZ x y z)    = [x, y, z]
_toDoubleArray (PointXYZM x y z m) = [x, y, z, m]

_toGeoPoint :: DoubleArray -> Maybe GeoPositionWithoutCRS
_toGeoPoint (DoubleArray [x, y])       = Just $ PointXY x y
_toGeoPoint (DoubleArray [x, y, z])    = Just $ PointXYZ x y z
_toGeoPoint (DoubleArray [x, y, z, m]) = Just $ PointXYZM x y z m
_toGeoPoint _                          = Nothing

-- instances

instance Aeson.ToJSON GeoPositionWithoutCRS where
  --  toJSON :: a -> Value
  toJSON a = Aeson.toJSON $ _toDoubleArray a

instance Aeson.FromJSON GeoPositionWithoutCRS where
--  parseJSON :: Value -> Parser a
  parseJSON o = do
    x <- Aeson.parseJSON o
    DataMaybe.maybe (fail "Illegal coordinates") pure (_toGeoPoint x)

sizeOfDouble :: Int
sizeOfDouble = sizeOf (undefined :: Double)

alignmentOfDouble :: Int
alignmentOfDouble = alignment (undefined :: Double)

instance VectorStorable.Storable GeoPositionWithoutCRS where
  sizeOf pt =
    case pt of
      PointXY {}   -> 1 + (sizeOfDouble * 2)
      PointXYZ {}  -> 1 + (sizeOfDouble * 3)
      PointXYZM {} -> 1 + (sizeOfDouble * 4)
  alignment pt =
    case pt of
      PointXY {}   -> 1 + (alignmentOfDouble * 2)
      PointXYZ {}  -> 1 + (alignmentOfDouble * 3)
      PointXYZM {} -> 1 + (alignmentOfDouble * 4)
  {-# INLINE peek #-}
  peek p = do
      t <- peekByteOff p 0
      case (t :: DataWord.Word8)  of
        0 -> PointXY   <$> peekByteOff p 1 <*> peekByteOff p 9
        1 -> PointXYZ  <$> peekByteOff p 1 <*> peekByteOff p 9 <*> peekByteOff p 17
        _ -> PointXYZM <$> peekByteOff p 1 <*> peekByteOff p 9 <*> peekByteOff p 17 <*> peekByteOff p 25
  poke p val =
    case val of
      PointXY x y       -> pokeByteOff p 0 (0 :: DataWord.Word8) *> pokeByteOff p 1 x  *> pokeByteOff p 9 y
      PointXYZ x y z    -> pokeByteOff p 0 (1 :: DataWord.Word8) *> pokeByteOff p 1 x  *> pokeByteOff p 9 y *> pokeByteOff p 17 z
      PointXYZM x y z m -> pokeByteOff p 0 (2 :: DataWord.Word8) *> pokeByteOff p 1 x  *> pokeByteOff p 9 y *> pokeByteOff p 17 z *> pokeByteOff p 25 m

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
