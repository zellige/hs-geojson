{-# LANGUAGE TemplateHaskell #-}
-------------------------------------------------------------------
-- |
-- Module       : Data.Geospatial.Geometry.GeoLine
-- Copyright    : (C) 2014 Dom De Re
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Data.Geospatial.Geometry.GeoLine (
    -- * Type
        GeoLine(..)
    -- * Lenses
    ,   unGeoLine
    ) where

import Data.Geospatial.BasicTypes
import Data.Geospatial.Geometry.Aeson
import Data.LineString

import Control.Lens ( makeLenses )
import Control.Monad ( mzero )
import Data.Aeson ( FromJSON(..), ToJSON(..), Value(..) )

newtype GeoLine = GeoLine { _unGeoLine :: LineString GeoPositionWithoutCRS } deriving (Show, Eq)

makeLenses ''GeoLine

-- instances

instance ToJSON GeoLine where
--  toJSON :: a -> Value
    toJSON = makeGeometryGeoAeson "LineString" . _unGeoLine

instance FromJSON GeoLine where
--  parseJSON :: Value -> Parser a
    parseJSON (Object o)    = readGeometryGeoAeson "LineString" GeoLine o
    parseJSON _             = mzero
