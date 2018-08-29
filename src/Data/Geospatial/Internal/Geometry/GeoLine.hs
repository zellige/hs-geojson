{-# LANGUAGE TemplateHaskell #-}
-------------------------------------------------------------------
-- |
-- Module       : Data.Geospatial.Internal.Geometry.GeoLine
-- Copyright    : (C) 2014-2018 HS-GeoJSON Project
-- License      : BSD-style (see the file LICENSE.md)
-- Maintainer   : Andrew Newman
--
-------------------------------------------------------------------
module Data.Geospatial.Internal.Geometry.GeoLine (
    -- * Type
        GeoLine(..)
    -- * Lenses
    ,   unGeoLine
    ) where

import           Data.Geospatial.Internal.BasicTypes
import           Data.Geospatial.Internal.Geometry.Aeson
import           Data.LineString

import           Control.Lens                            (makeLenses)
import           Control.Monad                           (mzero)
import           Data.Aeson                              (FromJSON (..),
                                                          ToJSON (..),
                                                          Value (..))

newtype GeoLine = GeoLine { _unGeoLine :: LineString GeoPositionWithoutCRS } deriving (Show, Eq)

makeLenses ''GeoLine

-- instances

instance ToJSON GeoLine where
--  toJSON :: a -> Value
    toJSON = makeGeometryGeoAeson "LineString" . _unGeoLine

instance FromJSON GeoLine where
--  parseJSON :: Value -> Parser a
    parseJSON (Object o) = readGeometryGeoAeson "LineString" GeoLine o
    parseJSON _          = mzero
