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
    -- * To Points
    ,   splitGeoMultiLine, mergeGeoLines
    ) where

import Data.Geospatial.BasicTypes
import Data.Geospatial.Geometry.GeoLine
import Data.Geospatial.Geometry.Aeson
import Data.LineString

import Control.Lens ( makeLenses )
import Control.Monad ( mzero )
import Data.Aeson ( FromJSON(..), ToJSON(..), Value(..), Object )

newtype GeoMultiLine    = GeoMultiLine { _unGeoMultiLine :: [LineString GeoPositionWithoutCRS] } deriving (Show, Eq)

makeLenses ''GeoMultiLine


-- | Split GeoMultiLine coordinates into multiple GeoLines
splitGeoMultiLine:: GeoMultiLine -> [GeoLine]
splitGeoMultiLine = map GeoLine . _unGeoMultiLine

-- | Merge multiple GeoLines into one GeoMultiLine
mergeGeoLines :: [GeoLine] -> GeoMultiLine
mergeGeoLines = GeoMultiLine . map _unGeoLine

-- instances

instance ToJSON GeoMultiLine where
--  toJSON :: a -> Value
    toJSON = makeGeometryGeoAeson "MultiLineString" . _unGeoMultiLine

instance FromJSON GeoMultiLine where
--  parseJSON :: Value -> Parser a
    parseJSON (Object o)    = readGeometryGeoAeson "MultiLineString" GeoMultiLine o
    parseJSON _             = mzero
