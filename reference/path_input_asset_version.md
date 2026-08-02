# Asset version for the path input

Bump on every change to `inst/assets/js/path-input.js` or its
stylesheet: the version is what busts a browser's cached copy, so
shipping JS without bumping it means users keep running the old file.
Named rather than inlined so a test can assert the dependency carries it
without hardcoding the number in two places.

## Usage

``` r
path_input_asset_version()
```
