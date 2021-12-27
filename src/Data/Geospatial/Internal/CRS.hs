{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
-------------------------------------------------------------------
-- |
-- Module       : Data.Geospatial.Internal.CRS
-- Copyright    : (C) 2014-2019 HS-GeoJSON Project
-- License      : BSD-style (see the file LICENSE.md)
-- Maintainer   : Andrew Newman
--
-- See Section 3 /Coordinate Reference System Objects/
-- in the GeoJSON Spec
--
-------------------------------------------------------------------
module Data.Geospatial.Internal.CRS (
    -- * Types
        CRSObject(..)
    -- * Functions
    ,   defaultCRS
    -- * Prisms
    ,   _NoCRS
    ,   _NamedCRS
    ,   _EPSG
    ,   _LinkedCRS
    ) where

import           Data.Geospatial.Internal.BasicTypes

import           Control.Lens                        (makePrisms)
import           Control.Monad                       (mzero)
import           Data.Aeson                          (FromJSON (..), Object,
                                                      ToJSON (..), Value (..),
                                                      object, (.:), (.=))
import           Data.Aeson.Types                    (Parser)
import           Data.Text                           (Text)
import           Data.Aeson.Key                      (fromText)


-- | See Section 3 /Coordinate Reference System Objects/ in the GeoJSON Spec
-- `NoCRS` is required because no 'crs' attribute in a GeoJSON feature is NOT the same thing as
-- a null 'crs' attribute. no 'crs' value implies the default CRS, while a null CRS means
-- you cannot assume a CRS, null will mapped to `NoCRS` while a non-existent attribute will
-- be mapped to a `Nothing` `Maybe` value
data CRSObject =
        NoCRS
    |   NamedCRS !Name
    |   EPSG Code
    |   LinkedCRS !Href !FormatString  deriving (Show, Eq)

makePrisms ''CRSObject

-- | The default CRS according to Section 3 /Coordinate Reference System Objects/ is WGS84 which I believe,
-- from <http://spatialreference.org/ref/epsg/4326/> which translates to this in JSON: <http://spatialreference.org/ref/epsg/4326/json/>)
-- is represented thus:
defaultCRS :: CRSObject
defaultCRS = EPSG 4326

-- instances

-- |
-- decode CRS Objects from GeoJSON
--
-- Aeson doesnt decode "null" to `Null` unfortunately
--
instance FromJSON CRSObject where
    parseJSON Null = return NoCRS
    parseJSON (Object obj) = do
        crsType <- obj .: "type"
        crsObjectFromAeson crsType obj
    parseJSON _ = mzero

-- |
-- encode CRS Objects to GeoJSON
--
instance ToJSON CRSObject where
    toJSON (NamedCRS name)          = object ["type" .= ("name" :: Text), "properties" .= object ["name" .= name]]
    toJSON (EPSG code)              = object ["type" .= ("epsg" :: Text), "properties" .= object ["code" .= code]]
    toJSON (LinkedCRS href format)  = object ["type" .= ("link" :: Text), "properties" .= object ["href" .= href, "type" .= format]]
    toJSON NoCRS                    = Null

-- helpers

crsPropertyFromAesonObj :: (FromJSON a) => Text -> Object -> Parser a
crsPropertyFromAesonObj name obj = do
    props <- obj .: "properties"
    props .: k
  where
      k = fromText name

crsObjectFromAeson :: Text -> Object -> Parser CRSObject
crsObjectFromAeson "name" obj   = NamedCRS <$> crsPropertyFromAesonObj "name" obj
crsObjectFromAeson "epsg" obj   = EPSG <$> crsPropertyFromAesonObj "code" obj
crsObjectFromAeson "link" obj   = LinkedCRS <$> crsPropertyFromAesonObj "href" obj <*> crsPropertyFromAesonObj "type" obj
crsObjectFromAeson _ _          = mzero
