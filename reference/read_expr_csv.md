# Create CSV/TSV/delimited file reading expression

Create CSV/TSV/delimited file reading expression

## Usage

``` r
read_expr_csv(path, ...)
```

## Arguments

- path:

  Character. File path

- ...:

  Reading parameters: sep, col_names, skip, n_max, quote, encoding

## Value

Expression calling readr::read_csv, readr::read_tsv, or
readr::read_delim
