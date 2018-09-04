{-# LANGUAGE TemplateHaskell #-}
-------------------------------------------------------------------
-- |
-- Module       : Data.Geospatial.Internal.Geometry.GeoMultiPolygon
-- Copyright    : (C) 2014-2018 HS-GeoJSON Project
-- License      : BSD-style (see the file LICENSE.md)
-- Maintainer   : Andrew Newman
--
-------------------------------------------------------------------
module Data.Geospatial.Internal.Geometry.GeoMultiPolygon (
    -- * Type
        GeoMultiPolygon(..)
    -- * Lenses
    ,   unGeoMultiPolygon
    -- * To Polygons
    ,   splitGeoMultiPolygon, mergeGeoPolygons
    ) where

import           Data.Geospatial.Internal.BasicTypes
import           Data.Geospatial.Internal.Geometry.Aeson
import qualified Data.Geospatial.Internal.Geometry.GeoPoint   as GeoPoint
import           Data.Geospatial.Internal.Geometry.GeoPolygon as GeoPolygon
import qualified Data.LinearRing                              as LinearRing

import           Control.Lens                                 (makeLenses)
import           Control.Monad                                (mzero)
import qualified Data.Aeson                                   as Aeson
import qualified Data.Maybe                                   as DataMaybe
import qualified Data.Vector                                  as Vector

newtype GeoMultiPolygon = GeoMultiPolygon { _unGeoMultiPolygon :: Vector.Vector (Vector.Vector (LinearRing.LinearRing GeoPoint.GeoPoint)) } deriving (Show, Eq)

-- | Split GeoMultiPolygon coordinates into multiple GeoPolygons
splitGeoMultiPolygon :: GeoMultiPolygon -> Vector.Vector GeoPolygon
splitGeoMultiPolygon = Vector.map GeoPolygon . _unGeoMultiPolygon

-- | Merge multiple GeoPolygons into one GeoMultiPolygon
mergeGeoPolygons :: Vector.Vector GeoPolygon -> GeoMultiPolygon
mergeGeoPolygons = GeoMultiPolygon . Vector.map GeoPolygon._unGeoPolygon

makeLenses ''GeoMultiPolygon

-- instances

instance Aeson.ToJSON GeoMultiPolygon where
--  toJSON :: a -> Value
    toJSON x = makeGeometryGeoAeson "MultiPolygon" (fmap (fmap (fmap GeoPoint._unGeoPoint . LinearRing.fromLinearRing)) (_unGeoMultiPolygon x))

instance Aeson.FromJSON GeoMultiPolygon where
--  parseJSON :: Value -> Parser a
    parseJSON (Aeson.Object o)    = do
      lrd <- readGeometryGeoAeson "MultiPolygon" VectorLinearRingDouble o
      DataMaybe.maybe (fail "Illegal coordinates") pure (vectorLinearRingDoubleToMaybeGeoPolygon lrd)
    parseJSON _             = mzero

vectorLinearRingDoubleToMaybeGeoPolygon :: VectorLinearRingDouble -> Maybe GeoMultiPolygon
vectorLinearRingDoubleToMaybeGeoPolygon (VectorLinearRingDouble vlrd) =
  if Vector.null vlrd then
    Just (GeoMultiPolygon Vector.empty)
  else
    mkMaybeGeoMultiPolygon $ maybeValidGeoPoints vlrd

maybeValidGeoPoints :: Vector.Vector (Vector.Vector (LinearRing.LinearRing DoubleArray)) -> Vector.Vector (Vector.Vector (Maybe (LinearRing.LinearRing GeoPoint.GeoPoint)))
maybeValidGeoPoints = fmap (fmap (traverse GeoPoint.unGeoPoint))

mkMaybeGeoMultiPolygon :: Vector.Vector (Vector.Vector (Maybe (LinearRing.LinearRing GeoPoint.GeoPoint))) -> Maybe GeoMultiPolygon
mkMaybeGeoMultiPolygon maybePoints =
  if Vector.null validLinearRings then
    Nothing
  else
    Just (GeoMultiPolygon validLinearRings)
  where
    validLinearRings = removeMaybeLinearRings maybePoints

removeMaybeLinearRings :: Vector.Vector (Vector.Vector (Maybe (LinearRing.LinearRing GeoPoint.GeoPoint))) -> Vector.Vector (Vector.Vector (LinearRing.LinearRing GeoPoint.GeoPoint))
removeMaybeLinearRings = fmap (foldr (\lr acc -> maybe acc (`Vector.cons` acc) lr) Vector.empty)

