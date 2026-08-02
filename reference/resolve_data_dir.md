# Resolve paths against the board's data directory

Relative paths are taken to be relative to the data directory; absolute
ones and URLs are left alone. Vectorized, so a multi-file read resolves
in one call.

## Usage

``` r
resolve_data_dir(paths, data_dir = "")
```

## Arguments

- paths:

  Character vector of paths, each absolute, relative or a URL.

- data_dir:

  The board's data directory. `""` (the default) leaves every path
  alone, which is what a board without one wants.

## Value

`paths`, unnamed, with the relative ones prefixed by `data_dir`.

## Details

Every block that takes a path has to answer the same question – is this
absolute, or is it relative to the board's data directory – and each one
that answers it in its own words is a block that can resolve differently
from the path widget beside it, reading one file while showing another.
Exported so there is one answer.
