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
    ,   unGeoPoint
    ,   retrieveXY
    ) where

import           Control.Lens                            (makeLenses)
import           Control.Monad                           (mzero)
import qualified Data.Aeson                              as Aeson
import           Data.Geospatial.Internal.BasicTypes
import           Data.Geospatial.Internal.Geometry.Aeson


newtype GeoPoint = GeoPoint { _unGeoPoint :: GeoPositionWithoutCRS } deriving (Show, Eq)

makeLenses ''GeoPoint

retrieveXY :: GeoPoint -> PointXY
retrieveXY (GeoPoint position) =
  case position of
    (GeoPointXY p)                       -> p
    (GeoPointXYZ (PointXYZ pX pY _))     -> PointXY pX pY
    (GeoPointXYZM (PointXYZM pX pY _ _)) -> PointXY pX pY

-- instances

instance Aeson.ToJSON GeoPoint where
--  toJSON :: a -> Value
    toJSON = makeGeometryGeoAeson "Point" . _unGeoPoint

instance Aeson.FromJSON GeoPoint where
--  parseJSON :: Value -> Parser a
    parseJSON (Aeson.Object o) = readGeometryGeoAeson "Point" GeoPoint o
    parseJSON _                = mzero
