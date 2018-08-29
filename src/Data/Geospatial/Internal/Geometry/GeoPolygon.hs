{-# LANGUAGE TemplateHaskell #-}
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

import           Data.Geospatial.Internal.BasicTypes
import           Data.Geospatial.Internal.Geometry.Aeson
import           Data.LinearRing

import           Control.Lens                            (makeLenses)
import           Control.Monad                           (mzero)
import           Data.Aeson                              (FromJSON (..),
                                                          ToJSON (..),
                                                          Value (..))

newtype GeoPolygon = GeoPolygon { _unGeoPolygon :: [LinearRing GeoPositionWithoutCRS] } deriving (Show, Eq)

makeLenses ''GeoPolygon

-- instances

instance ToJSON GeoPolygon where
--  toJSON :: a -> Value
    toJSON = makeGeometryGeoAeson "Polygon" . _unGeoPolygon

instance FromJSON GeoPolygon where
--  parseJSON :: Value -> Parser a
    parseJSON (Object o) = readGeometryGeoAeson "Polygon" GeoPolygon o
    parseJSON _          = mzero
