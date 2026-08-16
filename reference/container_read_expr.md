# Read expression for a container

Resolves `path` against the container registry and returns one
unevaluated expression that evaluates to a bare named list of data
frames, restricted to `tables`.

## Usage

``` r
container_read_expr(path, tables = NULL, ...)
```

## Arguments

- path:

  Character. A resolved location.

- tables:

  Character vector of table names to read, or `NULL` for all.

- ...:

  Uniform member options, forwarded by the entry into each member's
  format entry, which picks the parameters it understands:
  `container_read_expr(dir, sep = ";")` reads every csv in the folder as
  semicolon-separated and leaves the other formats untouched.

## Value

A language object that, when evaluated, produces a named list of data
frames.
