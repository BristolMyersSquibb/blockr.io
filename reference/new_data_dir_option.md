# Data directory board option

A board-level option that sets a default data directory for read and
write blocks. When set, file paths entered in blocks are resolved
relative to this directory.

## Usage

``` r
new_data_dir_option(
  value = blockr_option("data_dir", ""),
  category = "Data",
  ...
)
```

## Arguments

- value:

  Character. Initial directory path. Default: empty string (no data
  directory).

- category:

  Character. Option category for UI grouping.

- ...:

  Forwarded to
  [`blockr.core::new_board_option()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_board_options.html).

## Value

A `board_option` object.
