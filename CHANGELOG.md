# 1.3.0

## 1.2.0 -> 1.3.0

-   `MultiPolygon`, `MultiPoint`, `MultiLine` were all corrected as they weren't compliant with the spec (See [here] (https://github.com/domdere/hs-geojson/issues/9) for reference).

# 1.2.x

## 1.1.1 -> 1.2.0

-   Added the `LineString` data type for use with `GeoLine`, now a `GeoLine` with less than 2 positions is a type error or a parse error, as it should be according to the [spec] (http://geojson.org/geojson-spec.html#linestring "LineString in the GeoJSON v1.0 spec")

# 1.1.x

## 1.1.0 -> 1.1.1

-   Just some version bumps, the `lens` version bounds for the `transformer` library (`<0.4`) were causing some conflicts.

## 1.0.1 -> 1.1.0

-   Corrected an issue with the `Polygon` ([here] (https://github.com/domdere/hs-geojson/issues/2 "Polygon Issue"))
    -   Introduced the `LinearRing` datatype to handle the LinearRings described in the spec.
        -   Valid JSON for a LinearRing contains at least 4 points (this is checked, parsing fails if the list is too short) and the last element should match the first, but this isnt checked due to performance issues with the current implementation (though its a resolvable issue)).
        -   A `LinearRing` can be converted to a List with `fromLinearRing` or you can just fold/traverse over it.
        -   Creating a `LinearRing` can be done with one of these:
            -   `makeLinearRing :: a -> a -> a -> [a] -> LinearRing a`
            -   `fromList` and `fromListWithEqCheck`, which will return a `Validate` result (see the type sigs) and [**Data.Validation**] (https://hackage.haskell.org/package/validation "Data.Validation")

# 1.0.x

## 0.0.2 -> 1.0.0

-   Migrated from `json` to `aeson`, all `JSON` instances were removed and replaced with `ToJSON` and `FromJSON` instances
-   `GeoFeature` and `GeoFeatureCollection` are now of kind `* -> *`, parameterised on the Property type, `GeoFeature a` and `GeoFeatureCollection a` are in `ToJSON` and/or `FromJSON` if `a` is in `ToJSON` and/or `FromJSON` respectively.
-   Lenses and prisms have been generated for each of the types.
-   Orphan Instances were removed.  Orphan Instances are bad.  Breaking type class coherency had some rather annoying consequences, you only need to import `Data.Geospatial` now and the `ToJSON` and `FromJSON` instances will come with it.  I wanted to split up the files and thought seperating the instances from the declarations was a good idea but it wasnt.  Instead I split the data types into seperate modules and kept the instances with their respective data type declarations.

