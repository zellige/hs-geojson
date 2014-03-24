-------------------------------------------------------------------
-- |
-- Module       : Data.Geosptial.Geometry.JSON
-- Copyright    : (C) 2014 Dom De Re
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-- Some helpers for some of the common JSON ops
-------------------------------------------------------------------
module Data.Geospatial.Geometry.JSON (
    -- * Geometry
        readGeometryGeoJSON
    ,   makeGeometryGeoJSON
    -- * Optional fields
    ,   optAttributes
    ,   optValFromObj
    ) where

import Control.Applicative ( (<$>) )
import Data.Maybe ( Maybe(..) )
import Text.JSON
    (   JSON(..)
    ,   JSObject
    ,   JSValue
    ,   Result(..)
    ,   makeObj
    ,   valFromObj
    )

-- | A generic function that can be used to read in the GeoJSON for:
-- GeoPoint, GeoMultiPoint, GeoLine, GeoMultiLine, GeoPolygon and GeoMultiPolygon
-- Takes in a String for the GeoJSON geometry type, the type constructor
-- for the datatype and the JSON object containing both the 'type' val and the 'coordinates' val
--
readGeometryGeoJSON :: (JSON a, JSON b) => String -> (a -> b) -> JSValue -> Result b
readGeometryGeoJSON geomTypeString geomType json = do
    geopointObj <- readJSON json
    geometryType <- valFromObj "type" geopointObj
    if geometryType == geomTypeString
        then
            geomType <$> valFromObj "coordinates" geopointObj
        else
            fail $ "Invalid Geometry Type: " ++ geometryType

-- | The inverse to the above, you just give it the type string and the value for the coordinates
-- and it will create the JSON object
--
makeGeometryGeoJSON :: (JSON a) => String -> a -> JSValue
makeGeometryGeoJSON typeString coordinates =
    makeObj [("type", showJSON typeString), ("coordinates", showJSON coordinates)]

-- | The other way around, given an optional value, will return the attributes that
-- should be added to the makeObj input
--
optAttributes :: (JSON a) => String -> Maybe a -> [(String, JSValue)]
optAttributes _ Nothing     = []
optAttributes name (Just x) = [(name, showJSON x)]

-- | get an optional value out of a JSON object:
--
optValFromObj :: (JSON a) => String -> JSObject JSValue -> Result (Maybe a)
optValFromObj attribute object = resultToMaybe $ valFromObj attribute object
    where
        resultToMaybe (Ok x)    = Ok $ Just x
        resultToMaybe (Error _) = Ok Nothing

