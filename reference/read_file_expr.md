# Create a read expression for a single file

Convenience wrapper that detects the file category from the path and
returns an unevaluated R expression for reading the file. Useful for
sibling packages that construct pipelines programmatically.

## Usage

``` r
read_file_expr(path, ...)
```

## Arguments

- path:

  Character. Path to a single file.

- ...:

  Additional parameters forwarded to the reader (e.g. `sep`, `sheet`,
  `skip`).

## Value

A language object (unevaluated call) that, when evaluated, reads the
file into a data frame.
