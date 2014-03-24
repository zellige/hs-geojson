{-# LANGUAGE TemplateHaskell #-}
-------------------------------------------------------------------
-- |
-- Module       : Data.Geospatial.Geometry.GeoPoint
-- Copyright    : (C) 2014 Dom De Re
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-- see Section 2.1.1 /Position/ in the GeoJSON Spec
-------------------------------------------------------------------
module Data.Geospatial.Geometry.GeoPoint (
    -- * Type
        GeoPoint(..)
    -- * Lenses
    ,   unGeoPoint
    ) where

import Data.Geospatial.BasicTypes
import Data.Geospatial.Geometry.JSON

import Control.Lens ( makeLenses )
import Text.JSON ( JSON(..) )

newtype GeoPoint = GeoPoint { _unGeoPoint :: GeoPositionWithoutCRS } deriving (Show, Eq)

makeLenses ''GeoPoint

-- instances

instance JSON GeoPoint where
    readJSON = readGeometryGeoJSON "Point" GeoPoint

    showJSON (GeoPoint point) = makeGeometryGeoJSON "Point" point

