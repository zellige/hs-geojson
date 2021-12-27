{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

-------------------------------------------------------------------

-------------------------------------------------------------------

-- |
-- Module       : Data.Geospatial.Internal.Geometry.GeoPoint
-- Copyright    : (C) 2014-2019 HS-GeoJSON Project
-- License      : BSD-style (see the file LICENSE.md)
-- Maintainer   : Andrew Newman
module Data.Geospatial.Internal.Geometry.GeoPoint
  ( -- * Type
    GeoPoint (..),
    unGeoPoint,
    retrieveXY,
  )
where

import Control.DeepSeq
import Control.Lens (makeLenses)
import Control.Monad (mzero)
import qualified Data.Aeson as Aeson
import Data.Geospatial.Internal.BasicTypes
import Data.Geospatial.Internal.Geometry.Aeson
import GHC.Generics (Generic)

newtype GeoPoint = GeoPoint {_unGeoPoint :: GeoPositionWithoutCRS} deriving (Show, Eq, Generic, NFData)

makeLenses ''GeoPoint

-- instances

instance Aeson.ToJSON GeoPoint where
  --  toJSON :: a -> Value
  toJSON = makeGeometryGeoAeson "Point" . _unGeoPoint

instance Aeson.FromJSON GeoPoint where
  --  parseJSON :: Value -> Parser a
  parseJSON (Aeson.Object o) = readGeometryGeoAeson "Point" GeoPoint o
  parseJSON _ = mzero
