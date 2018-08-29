{-# LANGUAGE TemplateHaskell #-}
-------------------------------------------------------------------
-- |
-- Module       : Data.Geospatial.Internal.Geometry.GeoPoint
-- Copyright    : (C) 2014-2018 HS-GeoJSON Project
-- License      : BSD-style (see the file LICENSE.md)
-- Maintainer   : Andrew Newman
--
-------------------------------------------------------------------
module Data.Geospatial.Internal.Geometry.GeoPoint (
    -- * Type
        GeoPoint(..)
    -- * Lenses
    ,   unGeoPoint
    ) where

import           Data.Geospatial.Internal.BasicTypes
import           Data.Geospatial.Internal.Geometry.Aeson

import           Control.Lens                            (makeLenses)
import           Control.Monad                           (mzero)
import           Data.Aeson                              (FromJSON (..),
                                                          ToJSON (..),
                                                          Value (..))

newtype GeoPoint = GeoPoint { _unGeoPoint :: GeoPositionWithoutCRS } deriving (Show, Eq)

makeLenses ''GeoPoint

-- instances

instance ToJSON GeoPoint where
--  toJSON :: a -> Value
    toJSON = makeGeometryGeoAeson "Point" . _unGeoPoint

instance FromJSON GeoPoint where
--  parseJSON :: Value -> Parser a
    parseJSON (Object o) = readGeometryGeoAeson "Point" GeoPoint o
    parseJSON _          = mzero
