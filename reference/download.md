# Download-only file export block

A variadic block that lets the user download one or more data frames as
a file in the browser. Intended as a lightweight counterpart to
[`new_write_block()`](https://bristolmyerssquibb.github.io/blockr.io/reference/write.md)
for the common case where no server-side save is needed.

## Usage

``` r
new_download_block(filename = "", format = "csv", args = list(), ...)
```

## Arguments

- filename:

  Character. Optional fixed filename (without extension). If empty
  (default), a timestamped filename is generated.

- format:

  Character. One of the values in
  [`write_formats()`](https://bristolmyerssquibb.github.io/blockr.io/reference/write_formats.md):
  `"csv"`, `"excel"`, `"parquet"`, or `"feather"`. Default: `"csv"`.

- args:

  Named list of format-specific writing parameters (same as
  [`new_write_block()`](https://bristolmyerssquibb.github.io/blockr.io/reference/write.md)).
  Only relevant values for the selected format are used.

- ...:

  Forwarded to
  [`blockr.core::new_transform_block()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_transform_block.html).

## Value

A blockr transform block exposing a download button.

## Details

Multi-input behavior matches
[`new_write_block()`](https://bristolmyerssquibb.github.io/blockr.io/reference/write.md):
multiple inputs produce a multi-sheet Excel file for `format = "excel"`,
or a ZIP archive for CSV, Parquet, and Feather.

Adding a new format (e.g. SAS) only requires extending
[`write_formats()`](https://bristolmyerssquibb.github.io/blockr.io/reference/write_formats.md),
[`format_extension()`](https://bristolmyerssquibb.github.io/blockr.io/reference/format_extension.md),
and the dispatch in `write_expr()` - both
[`new_write_block()`](https://bristolmyerssquibb.github.io/blockr.io/reference/write.md)
and `new_download_block()` pick it up automatically.

## Examples

``` r
if (interactive()) {
  library(blockr.core)
  serve(new_download_block())
}
```
