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

import           Data.Geospatial.Internal.Geometry.Aeson
import qualified Data.Geospatial.Internal.Geometry.GeoPoint   as GeoPoint
import           Data.Geospatial.Internal.Geometry.GeoPolygon
import           Data.LinearRing

import           Control.Lens                                 (makeLenses)
import           Control.Monad                                (mzero)
import           Data.Aeson                                   (FromJSON (..),
                                                               ToJSON (..),
                                                               Value (..))
import qualified Data.Vector                                  as Vector

newtype GeoMultiPolygon = GeoMultiPolygon { _unGeoMultiPolygon :: Vector.Vector (Vector.Vector (LinearRing GeoPoint.GeoPoint)) } deriving (Show, Eq)

-- | Split GeoMultiPolygon coordinates into multiple GeoPolygons
splitGeoMultiPolygon :: GeoMultiPolygon -> Vector.Vector GeoPolygon
splitGeoMultiPolygon = Vector.map GeoPolygon . _unGeoMultiPolygon

-- | Merge multiple GeoPolygons into one GeoMultiPolygon
mergeGeoPolygons :: Vector.Vector GeoPolygon -> GeoMultiPolygon
mergeGeoPolygons = GeoMultiPolygon . Vector.map _unGeoPolygon

makeLenses ''GeoMultiPolygon

-- instances

instance ToJSON GeoMultiPolygon where
--  toJSON :: a -> Value
    toJSON = makeGeometryGeoAeson "MultiPolygon" . _unGeoMultiPolygon

instance FromJSON GeoMultiPolygon where
--  parseJSON :: Value -> Parser a
    parseJSON (Object o)    = readGeometryGeoAeson "MultiPolygon" GeoMultiPolygon o
    parseJSON _             = mzero
