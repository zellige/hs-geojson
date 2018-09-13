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
    ,   retrieveXY
    ,   PointXY (..)
    ,   PointXYZ (..)
    ,   PointXYZM (..)
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
data PointXY = PointXY
    { _xyX :: !Double
    , _xyY :: !Double
    } deriving (Show, Eq)

data PointXYZ = PointXYZ
    { _xyzX :: !Double
    , _xyzY :: !Double
    , _xyzZ :: !Double
    } deriving (Show, Eq)

data PointXYZM = PointXYZM
    { _xyzmX :: !Double
    , _xyzmY :: !Double
    , _xyzmZ :: !Double
    , _xyzmM :: !Double
    } deriving (Show, Eq)

data GeoPositionWithoutCRS = GeoEmpty | GeoPointXY PointXY | GeoPointXYZ PointXYZ | GeoPointXYZM PointXYZM deriving (Show, Eq)

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
    GeoEmpty                             -> undefined
    (GeoPointXY p)                       -> p
    (GeoPointXYZ (PointXYZ pX pY _))     -> PointXY pX pY
    (GeoPointXYZM (PointXYZM pX pY _ _)) -> PointXY pX pY

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
      GeoEmpty                    -> 1 + sizeOfDouble
      (GeoPointXY PointXY {})     -> 1 + (sizeOfDouble * 2)
      (GeoPointXYZ PointXYZ {})   -> 1 + (sizeOfDouble * 3)
      (GeoPointXYZM PointXYZM {}) -> 1 + (sizeOfDouble * 4)
  alignment pt =
    case pt of
      GeoEmpty                    -> 1 + alignmentOfDouble
      (GeoPointXY PointXY {})     -> 1 + (alignmentOfDouble * 2)
      (GeoPointXYZ PointXYZ {})   -> 1 + (alignmentOfDouble * 3)
      (GeoPointXYZM PointXYZM {}) -> 1 + (alignmentOfDouble * 4)
  {-# INLINE peek #-}
  peek p = do
      t <- peekByteOff p 0
      case (t :: DataWord.Word8)  of
        0 -> pure GeoEmpty
        1 -> fmap GeoPointXY $ PointXY <$> peekByteOff p 1 <*> peekByteOff p 9
        2 -> fmap GeoPointXYZ $ PointXYZ  <$> peekByteOff p 1 <*> peekByteOff p 9 <*> peekByteOff p 17
        _ -> fmap GeoPointXYZM $ PointXYZM <$> peekByteOff p 1 <*> peekByteOff p 9 <*> peekByteOff p 17 <*> peekByteOff p 25
  poke p val =
    case val of
      GeoEmpty                           -> pokeByteOff p 0 (0 :: DataWord.Word8) *> pokeByteOff p 1 (0 :: Double)
      (GeoPointXY   (PointXY x y))       -> pokeByteOff p 0 (1 :: DataWord.Word8) *> pokeByteOff p 1 x  *> pokeByteOff p 9 y
      (GeoPointXYZ  (PointXYZ x y z))    -> pokeByteOff p 0 (2 :: DataWord.Word8) *> pokeByteOff p 1 x  *> pokeByteOff p 9 y *> pokeByteOff p 17 z
      (GeoPointXYZM (PointXYZM x y z m)) -> pokeByteOff p 0 (3 :: DataWord.Word8) *> pokeByteOff p 1 x  *> pokeByteOff p 9 y *> pokeByteOff p 17 z *> pokeByteOff p 25 m

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
