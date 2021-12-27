{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

-------------------------------------------------------------------

-- |
-- Module       : Data.Geospatial.Internal.Geometry.GeoPolygon
-- Copyright    : (C) 2014-2021 HS-GeoJSON Project
-- License      : BSD-style (see the file LICENSE.md)
-- Maintainer   : Andrew Newman
module Data.Geospatial.Internal.Geometry.GeoPolygon
  ( -- * Type
    GeoPolygon (..),

    -- * Lenses
    unGeoPolygon,
  )
where

import Control.DeepSeq
import Control.Lens (makeLenses)
import Control.Monad (mzero)
import qualified Data.Aeson as Aeson
import Data.Geospatial.Internal.BasicTypes
import Data.Geospatial.Internal.Geometry.Aeson
import qualified Data.LinearRing as LinearRing
import qualified Data.Sequence as Sequence
import GHC.Generics (Generic)

newtype GeoPolygon = GeoPolygon {_unGeoPolygon :: Sequence.Seq (LinearRing.LinearRing GeoPositionWithoutCRS)} deriving (Show, Eq, Generic, NFData)

-- Sequence.Seq (LinearRing.LinearRing DoubleArray)

makeLenses ''GeoPolygon

-- instances

instance Aeson.ToJSON GeoPolygon where
  --  toJSON :: a -> Value
  toJSON = makeGeometryGeoAeson "Polygon" . _unGeoPolygon

instance Aeson.FromJSON GeoPolygon where
  --  parseJSON :: Value -> Parser a
  parseJSON (Aeson.Object o) = readGeometryGeoAeson "Polygon" GeoPolygon o
  parseJSON _ = mzero
