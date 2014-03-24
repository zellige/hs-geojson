{-# LANGUAGE TemplateHaskell #-}
-------------------------------------------------------------------
-- |
-- Module       : Data.Geospatial.Geometry.GeoMultiLine
-- Copyright    : (C) 2014 Dom De Re
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Data.Geospatial.Geometry.GeoMultiLine (
    -- * Type
        GeoMultiLine(..)
    -- * Lenses
    ,   unGeoMultiLine
    ) where

import Data.Geospatial.BasicTypes
import Data.Geospatial.Geometry.GeoLine
import Data.Geospatial.Geometry.Aeson
import Data.Geospatial.Geometry.JSON

import Control.Lens ( makeLenses )
import Control.Monad ( mzero )
import Data.Aeson ( FromJSON(..), ToJSON(..), Value(..), Object )
import Text.JSON ( JSON(..) )

newtype GeoMultiLine    = GeoMultiLine { _unGeoMultiLine :: [GeoLine] } deriving (Show, Eq)

makeLenses ''GeoMultiLine

-- instances

instance JSON GeoMultiLine where
    readJSON = readGeometryGeoJSON "MultiLine" GeoMultiLine

    showJSON (GeoMultiLine multiline) = makeGeometryGeoJSON "MultiLine" multiline

instance ToJSON GeoMultiLine where
--  toJSON :: a -> Value
    toJSON = makeGeometryGeoAeson "MultiLine" . _unGeoMultiLine

instance FromJSON GeoMultiLine where
--  parseJSON :: Value -> Parser a
    parseJSON (Object o)    = readGeometryGeoAeson "MultiLine" GeoMultiLine o
    parseJSON _             = mzero
