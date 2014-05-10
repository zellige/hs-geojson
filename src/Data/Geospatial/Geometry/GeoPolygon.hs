{-# LANGUAGE TemplateHaskell #-}
-------------------------------------------------------------------
-- |
-- Module       : Data.Geospatial.Geometry.GeoPolygon
-- Copyright    : (C) 2014 Dom De Re
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Data.Geospatial.Geometry.GeoPolygon (
    -- * Type
        GeoPolygon(..)
    -- * Lenses
    ,   unGeoPolygon
    ) where

import Data.Geospatial.BasicTypes
import Data.Geospatial.Geometry.Aeson
import Data.Geospatial.GeoPosition
import Data.LinearRing

import Control.Lens ( makeLenses )
import Control.Monad ( mzero )
import Data.Aeson ( FromJSON(..), ToJSON(..), Value(..), Object )

newtype GeoPolygon = GeoPolygon { _unGeoPolygon :: [LinearRing GeoPositionWithoutCRS] } deriving (Show, Eq)

makeLenses ''GeoPolygon

-- instances

instance ToJSON GeoPolygon where
--  toJSON :: a -> Value
    toJSON = makeGeometryGeoAeson "Polygon" . _unGeoPolygon

instance FromJSON GeoPolygon where
--  parseJSON :: Value -> Parser a
    parseJSON (Object o)    = readGeometryGeoAeson "Polygon" GeoPolygon o
    parseJSON _             = mzero
