# Create a write expression for a single file

Convenience wrapper that detects the file format from the path extension
and returns an unevaluated R expression for writing the data. Useful for
sibling packages that construct pipelines programmatically (e.g. DM
blocks).

## Usage

``` r
write_file_expr(data, path, ...)
```

## Arguments

- data:

  Character. Name of the data object to write.

- path:

  Character. Output file path (extension determines format).

- ...:

  Additional parameters forwarded to the writer (e.g. `sep`, `quote`,
  `na` for CSV files).

## Value

A language object (unevaluated call) that, when evaluated, writes the
data frame to the file.

## Examples

``` r
write_file_expr("mtcars", "/tmp/cars.csv")
#> readr::write_csv(x = mtcars, file = "/tmp/cars.csv", quote = "needed", 
#>     na = "NA")
write_file_expr("iris", "/tmp/flowers.xlsx")
#> writexl::write_xlsx(x = list(iris = iris), path = "/tmp/flowers.xlsx")
write_file_expr("df", "/tmp/data.parquet")
#> arrow::write_parquet(x = df, sink = "/tmp/data.parquet")
```
