-------------------------------------------------------------------
-- |
-- Module       : Data.Geospatial.Geometry.LinearRing
-- Copyright    : (C) 2014 Dom De Re
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-- Refer to the GeoJSON Spec <http://geojson.org/geojson-spec.html#linestring>
--
-------------------------------------------------------------------
module Data.Geospatial.Geometry.LinearRing (
    -- * Type
--        GeoLinearRing
    ) where

-- |
-- A `LinearRing` is a closed LineString/GeoLine with at least 4 points.
-- according to <http://geojson.org/geojson-spec.html#polygon>, the first and
-- last points are expected to be the same (hence it is closed)
--
