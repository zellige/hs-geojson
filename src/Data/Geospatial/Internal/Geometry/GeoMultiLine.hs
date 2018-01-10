{-# LANGUAGE TemplateHaskell #-}
-------------------------------------------------------------------
-- |
-- Module       : Data.Geospatial.Internal.Geometry.GeoMultiLine
-- Copyright    : (C) 2014 Dom De Re
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Data.Geospatial.Internal.Geometry.GeoMultiLine (
    -- * Type
        GeoMultiLine(..)
    -- * Lenses
    ,   unGeoMultiLine
    -- * To Points
    ,   splitGeoMultiLine, mergeGeoLines
    ) where

import           Data.Geospatial.Internal.BasicTypes
import           Data.Geospatial.Internal.Geometry.Aeson
import           Data.Geospatial.Internal.Geometry.GeoLine
import           Data.LineString

import           Control.Lens                              (makeLenses)
import           Control.Monad                             (mzero)
import           Data.Aeson                                (FromJSON (..),
                                                            ToJSON (..),
                                                            Value (..))

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
