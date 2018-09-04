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
import           Data.Geospatial.Internal.Geometry.GeoPolygon as GeoPolygon
import qualified Data.LinearRing                              as LinearRing

import           Control.Lens                                 (makeLenses)
import           Control.Monad                                (mzero)
import qualified Data.Aeson                                   as Aeson
import qualified Data.Vector                                  as Vector

newtype GeoMultiPolygon = GeoMultiPolygon { _unGeoMultiPolygon :: Vector.Vector (Vector.Vector (LinearRing.LinearRing GeoPositionWithoutCRS)) } deriving (Show, Eq)

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
  toJSON = makeGeometryGeoAeson "MultiPolygon" . _unGeoMultiPolygon

instance Aeson.FromJSON GeoMultiPolygon where
  --  parseJSON :: Value -> Parser a
  parseJSON (Aeson.Object o)    = readGeometryGeoAeson "MultiPolygon" GeoMultiPolygon o
  parseJSON _             = mzero
