{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
-------------------------------------------------------------------
-- |
-- Module       : Data.Geospatial.Internal.GeoFeature
-- Copyright    : (C) 2014-2018 HS-GeoJSON Project
-- License      : BSD-style (see the file LICENSE.md)
-- Maintainer   : Andrew Newman
--
-- See Section 2.2 /Feature Objects/ of the GeoJSON spec.
-- Parameterised on the property type
--
-------------------------------------------------------------------
module Data.Geospatial.Internal.GeoFeature (
    -- * Types
        GeoFeature(..)
    -- * Lenses
    ,   bbox
    ,   geometry
    ,   properties
    ,   featureId
    -- * Utils
    ,   reWrapGeometry
    ) where

import           Data.Geospatial.Internal.BasicTypes
import           Data.Geospatial.Internal.Geometry
import           Data.Geospatial.Internal.Geometry.Aeson

import           Control.Applicative                     ((<$>), (<*>))
import           Control.Lens                            (makeLenses)
import           Control.Monad                           (mzero)
import           Data.Aeson                              (FromJSON (..),
                                                          ToJSON (..),
                                                          Value (..), object,
                                                          (.:), (.:?), (.=))
import           Data.List                               ((++))
import           Data.Maybe                              (Maybe)
import           Data.Text                               (Text)
import           Prelude                                 (Eq (..), Show, ($))

-- | See Section 2.2 /Feature Objects/ of the GeoJSON spec.
-- Parameterised on the property type
data GeoFeature a = GeoFeature {
    _bbox       :: Maybe BoundingBoxWithoutCRS,
    _geometry   :: GeospatialGeometry,
    _properties :: a,
    _featureId  :: Maybe FeatureID } deriving (Show, Eq)

reWrapGeometry :: GeoFeature a -> GeospatialGeometry -> GeoFeature a
reWrapGeometry (GeoFeature bbox _ props fId) geom = GeoFeature bbox geom props fId

makeLenses ''GeoFeature

-- instances

-- | Decodes Feature objects from GeoJSON
--
instance (FromJSON a) => FromJSON (GeoFeature a) where
--  parseJSON :: Value -> Parse a
    parseJSON (Object obj) = do
        objType <- obj .: ("type" :: Text)
        if objType /= ("Feature" :: Text)
            then
                mzero
            else
                GeoFeature
                    <$> obj .:? ("bbox" :: Text)
                    <*> obj .: ("geometry" :: Text)
                    <*> obj .: ("properties" :: Text)
                    <*> obj .:? ("id" :: Text)
    parseJSON _ = mzero

-- | Encodes Feature objects to GeoJSON
--
instance (ToJSON a) => ToJSON (GeoFeature a) where
--  toJSON :: a -> Value
    toJSON (GeoFeature bbox' geom props featureId') = object $ baseAttributes ++ optAttributes "bbox" bbox' ++ optAttributes "id" featureId'
        where
            baseAttributes = ["type" .= ("Feature" :: Text), "properties" .= props, "geometry" .= geom]
