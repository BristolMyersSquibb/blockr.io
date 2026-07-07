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

Expression calling arrow::read_parquet (preferred) or
nanoparquet::read_parquet for parquet, and arrow::read_feather /
arrow::read_ipc_file for the Arrow-only feather and IPC formats
