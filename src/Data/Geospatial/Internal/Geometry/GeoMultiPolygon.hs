{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

-------------------------------------------------------------------

-------------------------------------------------------------------

-- |
-- Module       : Data.Geospatial.Internal.Geometry.GeoMultiPolygon
-- Copyright    : (C) 2014-2019 HS-GeoJSON Project
-- License      : BSD-style (see the file LICENSE.md)
-- Maintainer   : Andrew Newman
module Data.Geospatial.Internal.Geometry.GeoMultiPolygon
  ( -- * Type
    GeoMultiPolygon (..),

    -- * Lenses
    unGeoMultiPolygon,

    -- * To Polygons
    splitGeoMultiPolygon,
    mergeGeoPolygons,
  )
where

import Control.DeepSeq
import Control.Lens (makeLenses)
import Control.Monad (mzero)
import qualified Data.Aeson as Aeson
import Data.Geospatial.Internal.BasicTypes
import Data.Geospatial.Internal.Geometry.Aeson
import Data.Geospatial.Internal.Geometry.GeoPolygon as GeoPolygon
import qualified Data.LinearRing as LinearRing
import qualified Data.Sequence as Sequence
import GHC.Generics (Generic)

newtype GeoMultiPolygon = GeoMultiPolygon {_unGeoMultiPolygon :: Sequence.Seq (Sequence.Seq (LinearRing.LinearRing GeoPositionWithoutCRS))} deriving (Show, Eq, Generic, NFData)

-- | Split GeoMultiPolygon coordinates into multiple GeoPolygons
splitGeoMultiPolygon :: GeoMultiPolygon -> Sequence.Seq GeoPolygon
splitGeoMultiPolygon = fmap GeoPolygon . _unGeoMultiPolygon

-- | Merge multiple GeoPolygons into one GeoMultiPolygon
mergeGeoPolygons :: Sequence.Seq GeoPolygon -> GeoMultiPolygon
mergeGeoPolygons = GeoMultiPolygon . fmap GeoPolygon._unGeoPolygon

makeLenses ''GeoMultiPolygon

-- instances

instance Aeson.ToJSON GeoMultiPolygon where
  --  toJSON :: a -> Value
  toJSON = makeGeometryGeoAeson "MultiPolygon" . _unGeoMultiPolygon

instance Aeson.FromJSON GeoMultiPolygon where
  --  parseJSON :: Value -> Parser a
  parseJSON (Aeson.Object o) = readGeometryGeoAeson "MultiPolygon" GeoMultiPolygon o
  parseJSON _ = mzero
