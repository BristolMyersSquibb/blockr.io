# Build expression to write data to CSV file(s)

Build expression to write data to CSV file(s)

## Usage

``` r
write_expr_csv(data_names, path, args = list())
```

## Arguments

- data_names:

  Character vector of data object names to write

- path:

  Character. Full file path for output

- args:

  List of write parameters (sep, quote, na, etc.)

## Value

A language object (expression) that writes CSV file(s)
