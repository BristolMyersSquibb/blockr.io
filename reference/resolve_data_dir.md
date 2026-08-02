# Resolve paths against the board's data directory

Relative paths are taken to be relative to the data directory; absolute
ones and URLs are left alone. Vectorized, so a multi-file read resolves
in one call.

## Usage

``` r
resolve_data_dir(paths, data_dir = "")
```
