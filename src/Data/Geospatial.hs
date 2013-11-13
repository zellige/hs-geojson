-- Refer to the GeoJSON Spec http://www.geojson.org/geojson-spec.html

module Data.Geospatial  (
                        --  Types
                            Latitude
                        ,   Longitude
                        ,   Easting
                        ,   Northing
                        ,   Altitude
                        ,   GeoPositionWithoutCRS
                        ,   GeoPosition(..)
                        ,   GeoPoint(..)
                        ,   GeoMultiPoint(..)
                        ,   GeoPolygon(..)
                        ,   GeoMultiPolygon(..)
                        ,   GeoLine(..)
                        ,   GeoMultiLine(..)
                        ,   GeoPolyLine(..)
                        ,   GeospatialGeometry(..)
                        ,   Name
                        ,   Code
                        ,   Href
                        ,   FormatString
                        ,   ProjectionType
                        ,   CRSObject(..)
                        ,   FeatureID
                        ,   GeoProperty(..)
                        ,   GeoPropertyObject
                        ,   BoundingBoxWithoutCRS
                        ,   GeoFeature(..)
                        ,   GeoFeatureCollection(..)
                        --  Functions
                        ,   stripCRSFromPosition
                        ,   defaultCRS
                        ) where

import Text.JSON

type Latitude = Float
type Longitude = Float
type Easting = Float
type Northing = Float
type Altitude = Float

-- | ("WithoutCRS" is a catch all for indeterminate CRSs and for expression of positions
-- before a CRS has been determined

type GeoPositionWithoutCRS = [Float]

-- | see Section 2.1.1 "Position" in the GeoJSON Spec,
-- I make the assumption here that the only position types we will use will
-- involve easting/northing (+/- Altitude) or lon/lat (+/- Altitude)
data GeoPosition =
        LonLat Longitude Latitude
    |   LonLatAlt Longitude Latitude Altitude
    |   EastingNorthing Easting Northing
    |   EastingNorthingAlt Easting Northing Altitude

-- | the GeoPosition is a bit special in that when you convert it to GeoJSON,
-- it will lose the CRS info attached to it and cannot be read back in
-- from the GeoJSON.  Hence it is ineligible for the JSON type class,
-- so this function will strip it down to a GeoPositionWithoutCRS, which is eligible
stripCRSFromPosition :: GeoPosition -> GeoPositionWithoutCRS
stripCRSFromPosition (LonLat lon lat)                           = [lon, lat]
stripCRSFromPosition (LonLatAlt lon lat alt)                    = [lon, lat, alt]
stripCRSFromPosition (EastingNorthing easting northing)         = [easting, northing]
stripCRSFromPosition (EastingNorthingAlt easting northing alt)  = [easting, northing, alt]

-- These are all using newtype so that I can override their JSON instances..

newtype GeoPoint        = GeoPoint GeoPositionWithoutCRS deriving (Show, Eq)
newtype GeoMultiPoint   = GeoMultiPoint [GeoPoint] deriving (Show, Eq)
newtype GeoPolygon      = GeoPolygon [GeoPositionWithoutCRS] deriving (Show, Eq)
newtype GeoMultiPolygon = GeoMultiPolygon [GeoPolygon] deriving (Show, Eq)
newtype GeoLine         = GeoLine [GeoPositionWithoutCRS] deriving (Show, Eq)
newtype GeoMultiLine    = GeoMultiLine [GeoLine] deriving (Show, Eq)

data GeoPolyLine = Poly GeoPolygon | LineString GeoLine

-- | See section 2.1 "Geometry Objects" in the GeoJSON Spec.
data GeospatialGeometry =
        NoGeometry
    |   Point GeoPoint
    |   MultiPoint GeoMultiPoint
    |   Polygon GeoPolygon
    |   MultiPolygon GeoMultiPolygon
    |   Line GeoLine
    |   MultiLine GeoMultiLine
    |   Collection [GeospatialGeometry] deriving (Show, Eq)

type Name = String
type Code = Int
type Href = String
type FormatString = String
type ProjectionType = String

-- | See Section 3 "Coordinate Reference System Objects" in the GeoJSON Spec
-- "NoCRS" is required because no 'crs' attribute in a GeoJSON feature is NOT the same thing as
-- a null 'crs' attribute. no 'crs' value implies the default CRS, while a null CRS means
-- you cannot assume a CRS, null will mapped to NoCRS while a non-existent attribute will
-- be mapped to a Nothing Maybe value
data CRSObject =
        NoCRS
    |   NamedCRS Name
    |   EPSG Code
    |   LinkedCRS Href FormatString  deriving (Show, Eq)

-- | The default CRS according to Section 3 "Coordinate Reference System Objects" is WGS84 which I believe,
-- from http://spatialreference.org/ref/epsg/4326/ -> [JSON](http://spatialreference.org/ref/epsg/4326/json/ "WGS84 in JSON")
-- is represented thus:
defaultCRS :: CRSObject
defaultCRS = EPSG 4326

type FeatureID = String

data GeoProperty =
        StringProperty String
    |   FloatProperty Float
    |   DoubleProperty Double
    |   IntProperty Int
    |   PropertyObject GeoPropertyObject deriving (Show, Eq)

-- | According to section 2.2 "Feature Objects" of the GeoJSON spec, the "properties" object
-- can be any JSON object, or a null value, we may as well just make it a Type Alias
-- for JSValue, even though it has a wider scope (Integer, Rational, String etc...)
-- to take advantage of the solid JSON code out there to handle generic objects and the null value
type GeoPropertyObject = JSValue

-- | See Section 4 "Bounding Boxes" of the GeoJSON spec,
-- The length of the list/array must be 2*n where n is the dimensionality of the position type for the CRS
-- with min values first followed by the max values, wich both the min/max sets following the same axis order as the CRS,
-- e.g for WGS84: minLongitude, minLatitude, maxLongitude, maxLatitude
-- The spec mentions that it can be part of a geometry object too but doesnt give an example,
-- This implementation will ignore bboxes on Geometry objects, they can be added if required.
type BoundingBoxWithoutCRS = [Float]

-- | See Section 2.2 "Feature Objects" of the GeoJSON spec.
data GeoFeature = GeoFeature {
    bbox :: Maybe BoundingBoxWithoutCRS,
    geometry :: GeospatialGeometry,
    properties :: GeoPropertyObject,
    featureId :: Maybe FeatureID } deriving (Show, Eq)

-- | See Section 2.3 "Feature Collection Objects" of the GeoJSON spec
data GeoFeatureCollection = GeoFeatureCollection (Maybe BoundingBoxWithoutCRS) [GeoFeature] deriving (Show, Eq)


