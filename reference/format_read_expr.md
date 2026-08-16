# Read expression for a single file

Resolves `path`'s extension against the registry (named entries first,
then the rio fallback) and returns the entry's unevaluated read
expression. This is what the read block calls when it builds its
expression, and what a container's builder calls per member.

## Usage

``` r
format_read_expr(path, ...)
```

## Arguments

- path:

  Character. A resolved file path – never a user-typed one.

- ...:

  Format options forwarded to the entry (e.g. `sep`, `sheet`, `skip`).

## Value

A language object that, when evaluated, reads the file into a data
frame.
