{-# LANGUAGE TemplateHaskell #-}
-------------------------------------------------------------------
-- |
-- Module       : Data.Geospatial.Geometry.GeoMultiPolygon
-- Copyright    : (C) 2014 Dom De Re
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Data.Geospatial.Geometry.GeoMultiPolygon (
    -- * Type
        GeoMultiPolygon(..)
    -- * Lenses
    ,   unGeoMultiPolygon
    ) where

import Data.Geospatial.BasicTypes
import Data.Geospatial.Geometry.GeoPolygon
import Data.Geospatial.Geometry.Aeson

import Control.Lens ( makeLenses )
import Control.Monad ( mzero )
import Data.Aeson ( FromJSON(..), ToJSON(..), Value(..), Object )

newtype GeoMultiPolygon = GeoMultiPolygon { _unGeoMultiPolygon :: [GeoPolygon] } deriving (Show, Eq)

makeLenses ''GeoMultiPolygon

-- instances

instance ToJSON GeoMultiPolygon where
--  toJSON :: a -> Value
    toJSON = makeGeometryGeoAeson "MultiPolygon" . _unGeoMultiPolygon

instance FromJSON GeoMultiPolygon where
--  parseJSON :: Value -> Parser a
    parseJSON (Object o)    = readGeometryGeoAeson "MultiPolygon" GeoMultiPolygon o
    parseJSON _             = mzero
