# File category from extension

The file extension of a path that might be a URL

## Usage

``` r
path_ext(x)
```

## Arguments

- x:

  Character. A file path or URL.

## Value

Character. The extension, lower case, `""` when there is none.

## Details

[`tools::file_ext()`](https://rdrr.io/r/tools/fileutils.html) anchors at
the end of the string, so a URL that names its format before a query
string loses it: `file_ext("https://x.org/series.csv?dataset=y")` is
`""`, and the reader lookup then fails with "No reader registered for
extension" on a path that says `.csv` in plain sight. Query and fragment
come off first.

Only for paths that reach us from a user. A zip member name or a
directory entry cannot carry a query, and stripping one there would only
be a way to mangle a file whose name contains a `?`.
