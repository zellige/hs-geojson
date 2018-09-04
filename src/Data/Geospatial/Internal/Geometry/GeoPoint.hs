-------------------------------------------------------------------
-- |
-- Module       : Data.Geospatial.Internal.Geometry.GeoPoint
-- Copyright    : (C) 2014-2018 HS-GeoJSON Project
-- License      : BSD-style (see the file LICENSE.md)
-- Maintainer   : Andrew Newman
--
-------------------------------------------------------------------
module Data.Geospatial.Internal.Geometry.GeoPoint (
    -- * Type
        GeoPoint(..)
    ,   unGeoPoint
    ,  _unGeoPoint
    ) where

import           Control.Monad                           (mzero)
import qualified Data.Aeson                              as Aeson
import           Data.Geospatial.Internal.BasicTypes
import           Data.Geospatial.Internal.Geometry.Aeson
import qualified Data.Maybe                              as DataMaybe
import qualified Data.Vector.Storable                    as VectorStorable
import qualified Data.Word                               as DataWord
import           Foreign.Storable


data GeoPoint =
  GeoPointXY
    { _xyX :: !Double
    , _xyY :: !Double
    }
  | GeoPointXYZ
    { _xyzX :: !Double
    , _xyzY :: !Double
    , _xyzZ :: !Double
    }
  | GeoPointXYZM
    { _xyzX :: !Double
    , _xyzY :: !Double
    , _xyzZ :: !Double
    , _xyzM :: !Double
    }

 deriving (Show, Eq)

_unGeoPoint :: GeoPoint -> [Double]
_unGeoPoint (GeoPointXY x y)       = [x, y]
_unGeoPoint (GeoPointXYZ x y z)    = [x, y, z]
_unGeoPoint (GeoPointXYZM x y z m) = [x, y, z, m]

unGeoPoint :: DoubleArray -> Maybe GeoPoint
unGeoPoint (DoubleArray [x, y])       = Just $ GeoPointXY x y
unGeoPoint (DoubleArray [x, y, z])    = Just $ GeoPointXYZ x y z
unGeoPoint (DoubleArray [x, y, z, m]) = Just $ GeoPointXYZM x y z m
unGeoPoint _                          = Nothing

-- instances

sizeOfDouble :: Int
sizeOfDouble = sizeOf (undefined :: Double)

alignmentOfDouble :: Int
alignmentOfDouble = alignment (undefined :: Double)

offsetOfDouble :: Int
offsetOfDouble = sizeOfDouble `div` 2

instance VectorStorable.Storable GeoPoint where
  sizeOf pt =
    case pt of
      GeoPointXY {}   -> 2 + (sizeOfDouble * 2)
      GeoPointXYZ {}  -> 2 + (sizeOfDouble * 3)
      GeoPointXYZM {} -> 2 + (sizeOfDouble * 4)
  alignment pt =
    case pt of
      GeoPointXY {}   -> 2 + (alignmentOfDouble * 2)
      GeoPointXYZ {}  -> 2 + (alignmentOfDouble * 3)
      GeoPointXYZM {} -> 2 + (alignmentOfDouble * 4)
  {-# INLINE peek #-}
  peek p = do
     t <- peekByteOff p 0
     case (t :: DataWord.Word8)  of
       0 -> GeoPointXY   <$> peekByteOff p 1 <*> peekByteOff p 9
       1 -> GeoPointXYZ  <$> peekByteOff p 1 <*> peekByteOff p 9 <*> peekByteOff p 17
       _ -> GeoPointXYZM <$> peekByteOff p 1 <*> peekByteOff p 9 <*> peekByteOff p 17 <*> peekByteOff p 25
  poke p val =
    case val of
      GeoPointXY x y       -> pokeByteOff p 0 (1 :: DataWord.Word8) *> pokeByteOff p 1 x  *> pokeByteOff p 9 y
      GeoPointXYZ x y z    -> pokeByteOff p 0 (2 :: DataWord.Word8) *> pokeByteOff p 1 x  *> pokeByteOff p 9 y *> pokeByteOff p 17 z
      GeoPointXYZM x y z m -> pokeByteOff p 0 (3 :: DataWord.Word8) *> pokeByteOff p 1 x  *> pokeByteOff p 9 y *> pokeByteOff p 17 z *> pokeByteOff p 25 m

instance Aeson.ToJSON GeoPoint where
--  toJSON :: a -> Value
    toJSON = makeGeometryGeoAeson "Point" . _unGeoPoint

instance Aeson.FromJSON GeoPoint where
--  parseJSON :: Value -> Parser a
    parseJSON (Aeson.Object o) = do
      x <- readGeometryGeoAeson "Point" DoubleArray o
      DataMaybe.maybe (fail "Illegal coordinates") pure (unGeoPoint x)
    parseJSON _          = mzero
