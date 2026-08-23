# Create expression for a single file

Dispatches through the format registry by extension. `file_type` is kept
for callers but no longer decides the reader: the registry's named
entries cover the same mapping
[`file_category()`](https://bristolmyerssquibb.github.io/blockr.io/reference/file_category.md)
used to hardcode, and per-path dispatch means a mixed multi-file read no
longer sends every file to the first file's reader.

## Usage

``` r
read_expr_single(path, file_type, ..., .emit = NULL)
```

## Arguments

- path:

  Character. Single file path

- file_type:

  Character. Type of file (unused, kept for callers)

- ...:

  Parameters for the reader function

## Value

A language object (expression)
