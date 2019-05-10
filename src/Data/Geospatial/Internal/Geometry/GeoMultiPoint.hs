{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
-------------------------------------------------------------------
-- |
-- Module       : Data.Geospatial.Internal.Geometry.GeoMultiPoint
-- Copyright    : (C) 2014-2019 HS-GeoJSON Project
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

import           Control.DeepSeq
import           Control.Lens                               (makeLenses)
import           Control.Monad                              (mzero)
import qualified Data.Aeson                                 as Aeson
import qualified Data.Sequence                              as Sequence
import           GHC.Generics                               (Generic)

newtype GeoMultiPoint = GeoMultiPoint { _unGeoMultiPoint :: Sequence.Seq GeoPositionWithoutCRS } deriving (Show, Eq, Generic, NFData)

makeLenses ''GeoMultiPoint

-- | Split GeoMultiPoint coordinates into multiple GeoPoints
splitGeoMultiPoint:: GeoMultiPoint -> Sequence.Seq GeoPoint
splitGeoMultiPoint = fmap GeoPoint . _unGeoMultiPoint

-- | Merge multiple GeoPoints into one GeoMultiPoint
mergeGeoPoints :: Sequence.Seq GeoPoint -> GeoMultiPoint
mergeGeoPoints = GeoMultiPoint . fmap _unGeoPoint

-- instances

instance Aeson.ToJSON GeoMultiPoint where
  --  toJSON :: a -> Value
  toJSON = makeGeometryGeoAeson "MultiPoint" . _unGeoMultiPoint

instance Aeson.FromJSON GeoMultiPoint where
  --  parseJSON :: Value -> Parser a
  parseJSON (Aeson.Object o) = readGeometryGeoAeson "MultiPoint" GeoMultiPoint o
  parseJSON _          = mzero
