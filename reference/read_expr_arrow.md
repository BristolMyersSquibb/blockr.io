# Create Arrow file reading expression

Create Arrow file reading expression

## Usage

``` r
read_expr_arrow(path, ...)
```

## Arguments

- path:

  Character. File path

- ...:

  Reading parameters (currently unused)

## Value

Expression calling arrow::read_parquet, arrow::read_feather, or
arrow::read_ipc_file
