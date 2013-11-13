# geojson

A thin GeoJSON Layer above the json library

## Building the project

The project must be "configured" at least once everytime `geojson.cabal` changes, this can be done with:

    cabal configure

If you wish to run the unit tests you will have to run:

    cabal configure --enable-tests

Then finally build it with:

    cabal build

See `cabal build --help` for more build options.

## Running Unit Tests

**After** running `cabal build`, you can run the unit tests with the command:

    cabal test

## Adding Unit tests

Unit tests are written with [**doctest**] [doctest-github], for instructions on how to add unit tests 
see the **doctest** [**User Guide**] [doctest-userguide].

Currently only files in the `src/` directory are searched for tests, it is assumed that the code in `main/`
is a thin layer of code that uses modules from `src/`

[doctest-github]: https://github.com/sol/doctest-haskell "sol/doctest-haskell on GitHub.com"
[doctest-userguide]: https://github.com/sol/doctest-haskell/blob/master/README.markdown#usage "doctest Usage Guide"

