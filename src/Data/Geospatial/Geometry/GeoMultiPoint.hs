{-# LANGUAGE TemplateHaskell #-}
-------------------------------------------------------------------
-- |
-- Module       : Data.Geospatial.Geometry.GeoMultiPoint
-- Copyright    : (C) 2014 Dom De Re
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Data.Geospatial.Geometry.GeoMultiPoint (
    -- * Type
        GeoMultiPoint(..)
    -- * Lenses
    ,   unGeoMultiPoint
    -- * To Points
    ,   splitGeoMultiPoint, mergeGeoPoints
    ) where

import Data.Geospatial.BasicTypes
import Data.Geospatial.Geometry.GeoPoint
import Data.Geospatial.Geometry.Aeson

import Control.Lens ( makeLenses )
import Control.Monad ( mzero )
import Data.Aeson ( FromJSON(..), ToJSON(..), Value(..), Object )

newtype GeoMultiPoint = GeoMultiPoint { _unGeoMultiPoint :: [GeoPositionWithoutCRS] } deriving (Show, Eq)

makeLenses ''GeoMultiPoint


-- | Split GeoMultiPoint coordinates into multiple GeoPoints
splitGeoMultiPoint:: GeoMultiPoint -> [GeoPoint]
splitGeoMultiPoint = map GeoPoint . _unGeoMultiPoint

-- | Merge multiple GeoPoints into one GeoMultiPoint
mergeGeoPoints :: [GeoPoint] -> GeoMultiPoint
mergeGeoPoints = GeoMultiPoint . map _unGeoPoint

-- instances

instance ToJSON GeoMultiPoint where
--  toJSON :: a -> Value
    toJSON = makeGeometryGeoAeson "MultiPoint" . _unGeoMultiPoint

instance FromJSON GeoMultiPoint where
--  parseJSON :: Value -> Parser a
    parseJSON (Object o)    = readGeometryGeoAeson "MultiPoint" GeoMultiPoint o
    parseJSON _             = mzero
