# File extension for a write format

Single source of truth mapping a
[`write_formats()`](https://bristolmyerssquibb.github.io/blockr.io/reference/write_formats.md)
value to its output file extension. Used by the internal `write_expr()`
builder and by the download handlers of
[`new_write_block()`](https://bristolmyerssquibb.github.io/blockr.io/reference/write.md)
and
[`new_download_block()`](https://bristolmyerssquibb.github.io/blockr.io/reference/download.md).

## Usage

``` r
format_extension(format, needs_zip = FALSE)
```

## Arguments

- format:

  Character. One of the values in
  [`write_formats()`](https://bristolmyerssquibb.github.io/blockr.io/reference/write_formats.md).

- needs_zip:

  Logical. If `TRUE`, returns `".zip"` regardless of format (for
  multi-input non-Excel downloads). Default: `FALSE`.

## Value

Character scalar, e.g. `".csv"`, `".xlsx"`, `".parquet"`, `".feather"`,
or `".zip"`.
