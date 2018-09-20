{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------
-- |
-- Module       : Data.Geosptial.Geometry.Aeson
-- Copyright    : (C) 2014-2018 HS-GeoJSON Project
-- License      : BSD-style (see the file LICENSE.md)
-- Maintainer   : Andrew Newman
--
-- Some helpers for some of the common Aeson ops
-------------------------------------------------------------------
module Data.Geospatial.Internal.Geometry.Aeson (
    -- * Geometry
        readGeometryGeoAeson
    ,   makeGeometryGeoAeson
    -- * Optional fields
    ,   optValFromObj
    ,   optAttributes
    ) where

import           Control.Applicative ((<$>))
import           Control.Monad       (mzero)
import           Data.Aeson          (FromJSON (..), Object, ToJSON (..), Value,
                                      object, (.:), (.:?), (.=))
import           Data.Aeson.Types    (Pair, Parser)
import           Data.Maybe          (Maybe (..))
import           Data.Text           (Text)


-- | A generic function that can be used to read in the GeoJSON for:
-- `GeoPoint`, `GeoMultiPoint`, `GeoLine`, `GeoMultiLine`, `GeoPolygon` and `GeoMultiPolygon`
-- Takes in a String for the GeoJSON geometry type, the type constructor
-- for the datatype and the JSON object containing both the 'type' val and the 'coordinates' val
--
readGeometryGeoAeson :: (FromJSON a, FromJSON b) => String -> (a -> b) -> Object -> Parser b
readGeometryGeoAeson geomTypeString geomType geopointObj = do
    geometryType <- geopointObj .: "type"
    if geometryType == geomTypeString
        then
            geomType <$> geopointObj .: "coordinates"
        else
            mzero

-- | The inverse to the above, you just give it the type string and the value for the coordinates
-- and it will create the JSON object
--
makeGeometryGeoAeson :: (ToJSON a) => String -> a -> Value
makeGeometryGeoAeson typeString coordinates =
    object ["type" .= typeString, "coordinates" .= coordinates]

-- | get an optional value out of a JSON object:
--
optValFromObj :: (FromJSON a) => Text -> Object -> Parser (Maybe a)
optValFromObj = flip (.:?)

-- | The other way around, given an optional value, will return the attributes that
-- should be added to the makeObj input
--
optAttributes :: (ToJSON a) => Text -> Maybe a -> [Pair]
optAttributes _ Nothing     = []
optAttributes name (Just x) = [name .= x]
