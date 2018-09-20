# geojson [![Build Status](https://travis-ci.org/indicatrix/hs-geojson.png?branch=master)](https://travis-ci.org/indicatrix/hs-geojson) [Hackage](https://hackage.haskell.org/package/geojson)

A thin GeoJSON Layer above the `aeson` library

## Building the project

Install the dependencies with:

    cabal install --only-dependencies

Optionally add `--enable-tests` if you intend to run the unit tests

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

## Development: Cabal Dependency Hell?

Cabal's great, but when you are developing a few different projects with their own dependency chains, sometimes installing all your libraries to the same place causes problems,

Consider trying [`cabal-dev`] [cabal-dev]. In terms of using it, all thats required is replacing `cabal` with `cabal-dev` in all the above command lines.

It will download and install all the dependencies for your project and install them in a `cabal-dev/` directory in your project directory, and they will only be used for this project.

Those with newer versions of `cabal` (`>= 1.18` I think) can skip `cabal-dev` and instead run `cabal sandbox init`, and just start runnign the above instructions
as is from `cabal install --only-dependencies`

[cabal-dev]: https://github.com/creswick/cabal-dev "creswick/cabal-dev on GitHub.com"

