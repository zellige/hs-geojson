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

import           Control.DeepSeq
import qualified Data.Aeson           as Aeson
import qualified Data.Aeson.Types     as AesonTypes
import qualified Data.Maybe           as DataMaybe
import qualified Data.Scientific      as Scientific
import qualified Data.Text            as Text
import qualified Data.Vector.Storable as VectorStorable
import           Foreign.Storable
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
{-# INLINE sizeOfDouble #-}

alignmentOfDouble :: Int
alignmentOfDouble = alignment (undefined :: Double)
{-# INLINE alignmentOfDouble #-}

instance VectorStorable.Storable PointXY where
  sizeOf _ = 2 * sizeOfDouble
  alignment _ = alignmentOfDouble
  {-# INLINE peek #-}
  peek p = PointXY <$> peekByteOff p 0 <*> peekByteOff p (1 * sizeOfDouble)
  {-# INLINE poke #-}
  poke p (PointXY x y) = pokeByteOff p 0 x  *> pokeByteOff p (1 * sizeOfDouble) y

instance VectorStorable.Storable GeoPositionWithoutCRS where
  sizeOf _ = 5 * sizeOfDouble
  alignment _ = alignmentOfDouble
  {-# INLINE peek #-}
  peek p = do
      t <- peekByteOff p 0
      case (t :: Double) of
        0 -> pure GeoEmpty
        1 -> fmap GeoPointXY $ PointXY <$> peekByteOff p (1 * sizeOfDouble) <*> peekByteOff p (2 * sizeOfDouble)
        2 -> fmap GeoPointXYZ $ PointXYZ  <$> peekByteOff p (1 * sizeOfDouble) <*> peekByteOff p (2 * sizeOfDouble) <*> peekByteOff p (3 * sizeOfDouble)
        _ -> fmap GeoPointXYZM $ PointXYZM <$> peekByteOff p (1 * sizeOfDouble) <*> peekByteOff p (2 * sizeOfDouble) <*> peekByteOff p (3 * sizeOfDouble) <*> peekByteOff p (4 * sizeOfDouble)
  {-# INLINE poke #-}
  poke p val =
    case val of
      GeoEmpty                           -> pokeByteOff p 0 (0 :: Double) *> pokeByteOff p (1 * sizeOfDouble) (0 :: Double)
      (GeoPointXY   (PointXY x y))       -> pokeByteOff p 0 (1 :: Double) *> pokeByteOff p (1 * sizeOfDouble) x  *> pokeByteOff p (2 * sizeOfDouble) y
      (GeoPointXYZ  (PointXYZ x y z))    -> pokeByteOff p 0 (2 :: Double) *> pokeByteOff p (1 * sizeOfDouble) x  *> pokeByteOff p (2 * sizeOfDouble) y *> pokeByteOff p (3 * sizeOfDouble) z
      (GeoPointXYZM (PointXYZM x y z m)) -> pokeByteOff p 0 (3 :: Double) *> pokeByteOff p (1 * sizeOfDouble) x  *> pokeByteOff p (2 * sizeOfDouble) y *> pokeByteOff p (3 * sizeOfDouble) z *> pokeByteOff p (4 * sizeOfDouble) m

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
newtype BoundingBoxWithoutCRS = BoundingBoxWithoutCRS { unBoundingBoxWithoutCrs :: VectorStorable.Vector Double } deriving (Eq, Show, Generic, NFData)

instance Aeson.FromJSON BoundingBoxWithoutCRS where
    parseJSON obj = do
        doubles <- Aeson.parseJSON obj :: AesonTypes.Parser [Double]
        pure . BoundingBoxWithoutCRS $ VectorStorable.fromList doubles

instance Aeson.ToJSON BoundingBoxWithoutCRS where
    toJSON = Aeson.toJSON . VectorStorable.toList . unBoundingBoxWithoutCrs
