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

import           Data.Geospatial.Internal.Geometry.Aeson
import           Data.Geospatial.Internal.Geometry.GeoPoint

import           Control.Lens                               (makeLenses)
import           Control.Monad                              (mzero)
import           Data.Aeson                                 (FromJSON (..),
                                                             ToJSON (..),
                                                             Value (..))
import qualified Data.Vector.Storable                       as VectorStorable

newtype GeoMultiPoint = GeoMultiPoint { _unGeoMultiPoint :: VectorStorable.Vector GeoPoint } deriving (Show, Eq)

makeLenses ''GeoMultiPoint


-- | Split GeoMultiPoint coordinates into multiple GeoPoints
splitGeoMultiPoint:: GeoMultiPoint -> VectorStorable.Vector GeoPoint
splitGeoMultiPoint = _unGeoMultiPoint

-- | Merge multiple GeoPoints into one GeoMultiPoint
mergeGeoPoints :: VectorStorable.Vector GeoPoint -> GeoMultiPoint
mergeGeoPoints = GeoMultiPoint

-- instances

instance ToJSON GeoMultiPoint where
--  toJSON :: a -> Value
    toJSON = makeGeometryGeoAeson "MultiPoint" . _unGeoMultiPoint

instance FromJSON GeoMultiPoint where
--  parseJSON :: Value -> Parser a
    parseJSON (Object o) = readGeometryGeoAeson "MultiPoint" GeoMultiPoint o
    parseJSON _          = mzero
