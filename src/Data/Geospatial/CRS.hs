{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
-------------------------------------------------------------------
-- |
-- Module       : Data.Geospatial.CRS
-- Copyright    : (C) 2014 Dom De Re
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-- See Section 3 /Coordinate Reference System Objects/
-- in the GeoJSON Spec
--
-------------------------------------------------------------------
module Data.Geospatial.CRS (
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

import Data.Geospatial.BasicTypes
import Data.Geospatial.Geometry
import Data.Geospatial.GeoPosition

import Control.Applicative ( (<$>), (<*>) )
import Control.Lens ( makePrisms )
import Control.Monad ( mzero )
import Data.Aeson
    (   FromJSON(..)
    ,   ToJSON(..)
    ,   Value(..)
    ,   Object
    ,   (.:)
    ,   (.=)
    ,   object
    )
import Data.Aeson.Types ( Parser )
import Data.Text ( Text )

-- $setup
--
-- >>> import qualified Data.Aeson as A
-- >>> import qualified Data.ByteString.Lazy.Char8 as BS
--
-- Test CRS Data
-- >>> let testLinkCRSJSON = "{\"type\":\"link\",\"properties\":{\"href\":\"www.google.com.au\",\"type\":\"proj4\"}}"
-- >>> let testLinkCRS = LinkedCRS "www.google.com.au" "proj4"
-- >>> let testEPSGJSON = "{\"type\":\"epsg\",\"properties\":{\"code\":4326}}"
-- >>> let testEPSG = EPSG 4326
-- >>> let testNamedCRSJSON = "{\"type\":\"name\",\"properties\":{\"name\":\"urn:ogc:def:crs:OGC:1.3:CRS84\"}}"
-- >>> let testNamedCRS = NamedCRS "urn:ogc:def:crs:OGC:1.3:CRS84"
--

-- | See Section 3 /Coordinate Reference System Objects/ in the GeoJSON Spec
-- `NoCRS` is required because no 'crs' attribute in a GeoJSON feature is NOT the same thing as
-- a null 'crs' attribute. no 'crs' value implies the default CRS, while a null CRS means
-- you cannot assume a CRS, null will mapped to `NoCRS` while a non-existent attribute will
-- be mapped to a `Nothing` `Maybe` value
data CRSObject =
        NoCRS
    |   NamedCRS Name
    |   EPSG Code
    |   LinkedCRS Href FormatString  deriving (Show, Eq)

makePrisms ''CRSObject

-- | The default CRS according to Section 3 /Coordinate Reference System Objects/ is WGS84 which I believe,
-- from <http://spatialreference.org/ref/epsg/4326/> which translates to this in JSON: <http://spatialreference.org/ref/epsg/4326/json/>)
-- is represented thus:
defaultCRS :: CRSObject
defaultCRS = EPSG 4326

-- instances

-- |
-- encode and decodes CRS Objects to and from GeoJSON
--
-- >>> (A.decode . BS.pack) testLinkCRSJSON == Just testLinkCRS
-- True
--
-- >>> (A.decode . BS.pack) testNamedCRSJSON == Just testNamedCRS
-- True
--
-- >>> (A.decode . BS.pack) testEPSGJSON == Just testEPSG
-- True
--
-- >>> (A.decode . BS.pack) "null" == Just NoCRS
-- True
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
-- >>> A.encode testLinkCRS == BS.pack testLinkCRSJSON
-- True
--
-- >>> A.encode testNamedCRS == BS.pack testNamedCRSJSON
-- True
--
-- >>> A.encode testEPSG == BS.pack testEPSGJSON
-- True
--
-- >>> A.encode NoCRS
-- "null"
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
    props .: name

crsObjectFromAeson :: Text -> Object -> Parser CRSObject
crsObjectFromAeson "name" obj   = NamedCRS <$> crsPropertyFromAesonObj "name" obj
crsObjectFromAeson "epsg" obj   = EPSG <$> crsPropertyFromAesonObj "code" obj
crsObjectFromAeson "link" obj   = LinkedCRS <$> crsPropertyFromAesonObj "href" obj <*> crsPropertyFromAesonObj "type" obj
crsObjectFromAeson _ _          = mzero


