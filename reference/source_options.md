# The options a source offers

What can be set when reading `path` in the given mode: the format's own
options for a single file, and for a container the options its members
understand (a folder of CSVs offers the csv options; a folder of parquet
offers none). This is what a block gates its settings affordance on – no
options, no gear – and what it generates the fields from.

## Usage

``` r
source_options(path, mode = c("single", "container"))
```

## Arguments

- path:

  Character. A resolved location.

- mode:

  `"single"` (one file, one data frame) or `"container"` (a location
  holding several tables).

## Value

Named list of
[format_opt](https://bristolmyerssquibb.github.io/blockr.io/reference/format_opt.md)
specs, possibly empty.
