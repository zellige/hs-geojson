{-# LANGUAGE TemplateHaskell #-}
-------------------------------------------------------------------
-- |
-- Module       : Data.Geospatial.Internal.Geometry.GeoMultiPolygon
-- Copyright    : (C) 2014 Dom De Re
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
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
import           Data.Geospatial.Internal.Geometry.GeoPolygon
import           Data.LinearRing

import           Control.Lens                                 (makeLenses)
import           Control.Monad                                (mzero)
import           Data.Aeson                                   (FromJSON (..),
                                                               ToJSON (..),
                                                               Value (..))

newtype GeoMultiPolygon = GeoMultiPolygon { _unGeoMultiPolygon :: [[LinearRing GeoPositionWithoutCRS]] } deriving (Show, Eq)

-- | Split GeoMultiPolygon coordinates into multiple GeoPolygons
splitGeoMultiPolygon :: GeoMultiPolygon -> [GeoPolygon]
splitGeoMultiPolygon = map GeoPolygon . _unGeoMultiPolygon

-- | Merge multiple GeoPolygons into one GeoMultiPolygon
mergeGeoPolygons :: [GeoPolygon] -> GeoMultiPolygon
mergeGeoPolygons = GeoMultiPolygon . map _unGeoPolygon

makeLenses ''GeoMultiPolygon

-- instances

instance ToJSON GeoMultiPolygon where
--  toJSON :: a -> Value
    toJSON = makeGeometryGeoAeson "MultiPolygon" . _unGeoMultiPolygon

instance FromJSON GeoMultiPolygon where
--  parseJSON :: Value -> Parser a
    parseJSON (Object o)    = readGeometryGeoAeson "MultiPolygon" GeoMultiPolygon o
    parseJSON _             = mzero
