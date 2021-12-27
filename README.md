# geojson [![Build Status](https://github.com/zellige/hs-geojson/actions/workflows/ci.yaml/badge.svg)] [Hackage](https://hackage.haskell.org/package/geojson)

A thin GeoJSON Layer above the `aeson` library

## Development

- We use [Ormolu](https://hackage.haskell.org/package/ormolu) for code formatting.

## Visual Studio Code Setup

Install Command Line Apps: ormolu and hlint.

Plugins:

- Haskell https://marketplace.visualstudio.com/items?itemName=haskell.haskell
- Haskell Syntax Highlighting https://marketplace.visualstudio.com/items?itemName=justusadam.language-haskell
- Haskell Linter https://marketplace.visualstudio.com/items?itemName=hoovercj.haskell-linter
- Run on Save https://marketplace.visualstudio.com/items?itemName=pucelle.run-on-save

One installed to reformat a file:

- Shift-Alt-F

Or configure Run on Save:

```
    {
        "globMatch": "{**/src/**/*.hs,**/test/**/*.hs}",
        "command": "ormolu --color always --check-idempotence --mode inplace ${file}",
        "runIn": "terminal",
        "runningStatusMessage": "Checking code style ${fileBasename}",
        "finishStatusMessage": "${fileBasename} checked"
    }
```

Run tests:
* `stack test :geojson-test`

Run hlint:
* `stack test :geojson-hlint`

### Other Projects/Documentation

[Geometry Survival Guide](https://www.gaia-gis.it/spatialite-3.0.0-BETA/GeoNotations.pdf)