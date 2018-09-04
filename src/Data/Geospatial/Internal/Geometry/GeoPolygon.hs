{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell           #-}

-------------------------------------------------------------------
-- |
-- Module       : Data.Geospatial.Internal.Geometry.GeoPolygon
-- Copyright    : (C) 2014-2018 HS-GeoJSON Project
-- License      : BSD-style (see the file LICENSE.md)
-- Maintainer   : Andrew Newman
--
-------------------------------------------------------------------
module Data.Geospatial.Internal.Geometry.GeoPolygon (
    -- * Type
        GeoPolygon(..)
    -- * Lenses
    ,   unGeoPolygon
    ) where

import           Control.Lens                               (makeLenses)
import           Control.Monad                              (mzero)
import qualified Data.Aeson                                 as Aeson
import           Data.Geospatial.Internal.BasicTypes
import           Data.Geospatial.Internal.Geometry.Aeson
import qualified Data.Geospatial.Internal.Geometry.GeoPoint as GeoPoint
import qualified Data.LinearRing                            as LinearRing
import qualified Data.Maybe                                 as DataMaybe
import qualified Data.Vector                                as Vector

newtype GeoPolygon = GeoPolygon { _unGeoPolygon :: Vector.Vector (LinearRing.LinearRing GeoPoint.GeoPoint) } deriving (Show, Eq)

-- Vector.Vector (LinearRing.LinearRing DoubleArray)

makeLenses ''GeoPolygon

-- instances

instance Aeson.ToJSON GeoPolygon where
--  toJSON :: a -> Value
    toJSON x = makeGeometryGeoAeson "Polygon" (fmap (fmap GeoPoint._unGeoPoint . LinearRing.fromLinearRing) (_unGeoPolygon x))

instance Aeson.FromJSON GeoPolygon where
--  parseJSON :: Value -> Parser a
    parseJSON (Aeson.Object o) = do
        lrd <- readGeometryGeoAeson "Polygon" LinearRingDouble o
        DataMaybe.maybe (fail "Illegal coordinates") pure (linearRingToMaybeGeoPolygon lrd)
    parseJSON _                = mzero

linearRingToMaybeGeoPolygon :: LinearRingDouble -> Maybe GeoPolygon
linearRingToMaybeGeoPolygon (LinearRingDouble lrd) =
  if Vector.null lrd then
    Just (GeoPolygon Vector.empty)
  else
    mkMaybeGeoPolygon $ maybeValidGeoPoints lrd

maybeValidGeoPoints :: Vector.Vector (LinearRing.LinearRing DoubleArray) -> Vector.Vector (Maybe (LinearRing.LinearRing GeoPoint.GeoPoint))
maybeValidGeoPoints = fmap (traverse GeoPoint.unGeoPoint)

mkMaybeGeoPolygon :: Vector.Vector (Maybe (LinearRing.LinearRing GeoPoint.GeoPoint)) -> Maybe GeoPolygon
mkMaybeGeoPolygon maybePoints =
  if Vector.null validLinearRings then
    Nothing
  else
    Just (GeoPolygon validLinearRings)
  where
    validLinearRings = removeMaybeLinearRings maybePoints

removeMaybeLinearRings :: Vector.Vector (Maybe (LinearRing.LinearRing GeoPoint.GeoPoint)) -> Vector.Vector (LinearRing.LinearRing GeoPoint.GeoPoint)
removeMaybeLinearRings = foldr (\lr acc -> maybe acc (`Vector.cons` acc) lr) Vector.empty
