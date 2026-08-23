# Register a file format

Teaches blockr.io a file format at load time. The format then works
everywhere blockr.io's format knowledge is consulted: the read block's
dispatch,
[`file_extensions()`](https://bristolmyerssquibb.github.io/blockr.io/reference/file_extensions.md)
(and with it the file browser filter and upload accept lists), and any
package that asks the registry rather than carrying its own extension
list.

## Usage

``` r
register_format(extensions, read, options = NULL, url_ok = FALSE)
```

## Arguments

- extensions:

  Character vector of file extensions (without dots).

- read:

  `function(path, ...)` returning an unevaluated expression that reads
  `path` into a data frame. The expression may only call the reader
  package (e.g. `bquote(your.pkg::read_special(.(path)))`), never
  blockr. `...` carries format options forwarded by the calling block.
  `path` is a resolved location and may be a language object (a
  container splicing member reads against a run-time prefix), so splice
  it, do not inspect it.

- options:

  Named list of
  [format_opt](https://bristolmyerssquibb.github.io/blockr.io/reference/format_opt.md)
  specs declaring what `read` accepts through `...`, or `NULL` for a
  format with nothing to tune. Blocks generate their settings fields
  from this and show no settings affordance at all when it is empty, so
  a format declaring options gets them offered everywhere without any
  block changing.

- url_ok:

  Does this format's reader accept an `https://` location where it
  accepts a path? `readr`'s delimited readers do; `readxl` and `arrow`
  do not. Blocks reading a URL download it to a temp file so that
  detection, size checks and a failed fetch all behave, and a format
  that declares `url_ok = TRUE` gets the URL back in the EXPRESSION it
  emits, so exported code names the source instead of a temp path that
  exists on one machine for one session. Default `FALSE`, which is the
  safe answer for a reader that cannot open a connection.

## Value

Invisible `NULL`, called for its side effect.

## Details

Call from your package's `.onLoad()`. The registering namespace is
recorded automatically for collision messages. Registration never
throws; two packages claiming the same extension and mode error at first
lookup, naming both.

## See also

[`register_container()`](https://bristolmyerssquibb.github.io/blockr.io/reference/register_container.md),
[`format_read_expr()`](https://bristolmyerssquibb.github.io/blockr.io/reference/format_read_expr.md),
[format_opt](https://bristolmyerssquibb.github.io/blockr.io/reference/format_opt.md)
