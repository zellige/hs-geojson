{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

-------------------------------------------------------------------
-- |
-- Module       : Data.Geospatial.Internal.Geometry.GeoPoint
-- Copyright    : (C) 2014-2018 HS-GeoJSON Project
-- License      : BSD-style (see the file LICENSE.md)
-- Maintainer   : Andrew Newman
--
-------------------------------------------------------------------
module Data.Geospatial.Internal.Geometry.GeoPoint (
    -- * Type
        GeoPoint(..)
    ,   _unGeoPoint
    ) where

import qualified Control.Applicative                     as Control
import           Control.Monad                           (mzero)
import           Data.Aeson                              (FromJSON (..),
                                                          ToJSON (..),
                                                          Value (..))
import           Data.Geospatial.Internal.BasicTypes
import           Data.Geospatial.Internal.Geometry.Aeson
import qualified Data.Maybe                              as DataMaybe
import           GHC.Generics

data GeoPoint = GeoPoint
  { _x    :: !Double
  , _y    :: !Double
  , _aOrE :: Maybe Double
 } deriving (Show, Eq)

newtype DoubleArray = DoubleArray [Double] deriving (Eq, Show, Generic, FromJSON, ToJSON)

_unGeoPoint :: GeoPoint -> [Double]
_unGeoPoint (GeoPoint x y Nothing)     = [x, y]
_unGeoPoint (GeoPoint x y (Just aOrE)) = [x, y, aOrE]

unGeoPoint :: DoubleArray -> Maybe GeoPoint
unGeoPoint (DoubleArray [x, y])       = Just $ GeoPoint x y Nothing
unGeoPoint (DoubleArray [x, y, aOrE]) = Just $ GeoPoint x y (Just aOrE)
unGeoPoint _                          = Nothing

-- instances

instance ToJSON GeoPoint where
--  toJSON :: a -> Value
    toJSON = makeGeometryGeoAeson "Point" . _unGeoPoint

instance FromJSON GeoPoint where
--  parseJSON :: Value -> Parser a
    parseJSON (Object o) = do
      x <- readGeometryGeoAeson "Point" DoubleArray o
      DataMaybe.maybe (fail "Illegal coordinates") pure (unGeoPoint x)
    parseJSON _          = mzero
