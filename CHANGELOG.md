# 1.0.x

## 0.0.2 -> 1.0.0

-   Migrated from `json` to `aeson`, all `JSON` instances were removed and replaced with `ToJSON` and `FromJSON` instances
-   `GeoFeature` and `GeoFeatureCollection` are now of kind `* -> *`, parameterised on the Property type, `GeoFeature a` and `GeoFeatureCollection a` are in `ToJSON` and/or `FromJSON` if `a` is in `ToJSON` and/or `FromJSON` respectively.
-   Lenses and prisms have been generated for each of the types.
-   Orphan Instances were removed.  Orphan Instances are bad.  Breaking type class coherency had some rather annoying consequences, you only need to import `Data.Geospatial` now and the `ToJSON` and `FromJSON` instances will come with it.  I wanted to split up the files and thought seperating the instances from the declarations was a good idea but it wasnt.  Instead I split the data types into seperate modules and kept the instances with their respective data type declarations.

