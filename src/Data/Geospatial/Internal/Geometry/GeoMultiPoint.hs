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
import           Data.Geospatial.Internal.Geometry.GeoPoint

import           Control.Lens                               (makeLenses)
import           Control.Monad                              (mzero)
import           Data.Aeson                                 (FromJSON (..),
                                                             ToJSON (..),
                                                             Value (..))
import qualified Data.Vector                                as Vector

newtype GeoMultiPoint = GeoMultiPoint { _unGeoMultiPoint :: Vector.Vector GeoPositionWithoutCRS } deriving (Show, Eq)

makeLenses ''GeoMultiPoint


-- | Split GeoMultiPoint coordinates into multiple GeoPoints
splitGeoMultiPoint:: GeoMultiPoint -> Vector.Vector GeoPoint
splitGeoMultiPoint = Vector.map GeoPoint . _unGeoMultiPoint

-- | Merge multiple GeoPoints into one GeoMultiPoint
mergeGeoPoints :: Vector.Vector GeoPoint -> GeoMultiPoint
mergeGeoPoints = GeoMultiPoint . Vector.map _unGeoPoint

-- instances

instance ToJSON GeoMultiPoint where
--  toJSON :: a -> Value
    toJSON = makeGeometryGeoAeson "MultiPoint" . _unGeoMultiPoint

instance FromJSON GeoMultiPoint where
--  parseJSON :: Value -> Parser a
    parseJSON (Object o) = readGeometryGeoAeson "MultiPoint" GeoMultiPoint o
    parseJSON _          = mzero
