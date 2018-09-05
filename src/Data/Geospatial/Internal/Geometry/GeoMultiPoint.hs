{-# LANGUAGE TemplateHaskell #-}
-------------------------------------------------------------------
-- |
-- Module       : Data.Geospatial.Internal.Geometry.GeoMultiPoint
-- Copyright    : (C) 2014-2018 HS-GeoJSON Project
-- License      : BSD-style (see the file LICENSE.md)
-- Maintainer   : Andrew Newman
--
-------------------------------------------------------------------
module Data.Geospatial.Internal.Geometry.GeoMultiPoint (
    -- * Type
        GeoMultiPoint(..)
    -- * Lenses
    ,   unGeoMultiPoint
    -- * To Points
    ,   splitGeoMultiPoint, mergeGeoPoints
    ) where

import           Data.Geospatial.Internal.BasicTypes
import           Data.Geospatial.Internal.Geometry.Aeson

import           Control.Lens                            (makeLenses)
import           Control.Monad                           (mzero)
import qualified Data.Aeson                              as Aeson
import qualified Data.Vector                             as Vector

newtype GeoMultiPoint = GeoMultiPoint { _unGeoMultiPoint :: Vector.Vector GeoPositionWithoutCRS } deriving (Show, Eq)

makeLenses ''GeoMultiPoint

-- | Split GeoMultiPoint coordinates into multiple GeoPoints
splitGeoMultiPoint:: GeoMultiPoint -> Vector.Vector GeoPositionWithoutCRS
splitGeoMultiPoint = _unGeoMultiPoint

-- | Merge multiple GeoPoints into one GeoMultiPoint
mergeGeoPoints :: Vector.Vector GeoPositionWithoutCRS -> GeoMultiPoint
mergeGeoPoints = GeoMultiPoint

-- instances

instance Aeson.ToJSON GeoMultiPoint where
  --  toJSON :: a -> Value
  toJSON = makeGeometryGeoAeson "MultiPoint" . _unGeoMultiPoint

instance Aeson.FromJSON GeoMultiPoint where
  --  parseJSON :: Value -> Parser a
  parseJSON (Aeson.Object o) = readGeometryGeoAeson "MultiPoint" GeoMultiPoint o
  parseJSON _          = mzero
