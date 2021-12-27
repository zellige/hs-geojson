{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

-------------------------------------------------------------------

-- |
-- Module       : Data.Geospatial.Internal.GeoFeature
-- Copyright    : (C) 2014-2021 HS-GeoJSON Project
-- License      : BSD-style (see the file LICENSE.md)
-- Maintainer   : Andrew Newman
--
-- See Section 2.3 /Feature Collection Objects/ of the GeoJSON spec
module Data.Geospatial.Internal.GeoFeatureCollection
  ( -- * Types
    GeoFeatureCollection (..),

    -- * Lenses
    boundingbox,
    geofeatures,
  )
where

import Control.Applicative ((<$>), (<*>))
import Control.Lens (makeLenses)
import Control.Monad (mzero)
import Data.Aeson
  ( FromJSON (..),
    ToJSON (..),
    Value (..),
    object,
    (.:),
    (.:?),
    (.=),
  )
import Data.Geospatial.Internal.BasicTypes
import Data.Geospatial.Internal.GeoFeature
import Data.Geospatial.Internal.Geometry.Aeson
import Data.List ((++))
import Data.Maybe (Maybe (..))
import qualified Data.Sequence as Sequence
import Data.Text (Text)
import Prelude (Eq (..), Show, ($))

-- | See Section 2.3 /Feature Collection Objects/ of the GeoJSON spec
data GeoFeatureCollection a = GeoFeatureCollection
  { _boundingbox :: Maybe BoundingBoxWithoutCRS,
    _geofeatures :: Sequence.Seq (GeoFeature a)
  }
  deriving (Show, Eq)

makeLenses ''GeoFeatureCollection

-- instances

-- | Decode FeatureCollection objects from GeoJSON
instance (FromJSON a) => FromJSON (GeoFeatureCollection a) where
  --  parseJSON :: Value -> Parse a
  parseJSON (Object obj) = do
    objType <- obj .: "type"
    if objType /= ("FeatureCollection" :: Text)
      then mzero
      else
        GeoFeatureCollection
          <$> obj .:? "bbox"
          <*> obj .: "features"
  parseJSON _ = mzero

-- | Encode FeatureCollection objects to GeoJSON
instance (ToJSON a) => ToJSON (GeoFeatureCollection a) where
  --  toJSON :: a -> Value
  toJSON (GeoFeatureCollection bbox' features) = object $ baseAttributes ++ optAttributes "bbox" bbox'
    where
      baseAttributes = ["type" .= ("FeatureCollection" :: Text), "features" .= features]
