# Build expression to write data to Arrow format (Parquet or Feather)

Build expression to write data to Arrow format (Parquet or Feather)

## Usage

``` r
write_expr_arrow(data_names, path, format = "parquet", as_sym = as_bare_sym)
```

## Arguments

- data_names:

  Character vector of data object names to write

- path:

  Character. Full file path for output

- format:

  Character. Either "parquet" or "feather"

## Value

A language object (expression) that writes Arrow file(s)
