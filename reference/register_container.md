# Register a container

A container is a location that holds several tables: a directory, a zip,
a workbook read sheet by sheet. A container entry produces a bare named
list of data frames – no class. Whatever the consuming block wants the
list to become (a dm, via `dm::new_dm()`) is its own wrap, made outside
this package.

## Usage

``` r
register_container(opens, list_tables, read, options = NULL)
```

## Arguments

- opens:

  Character vector of file extensions (without dots), or the string
  `"directory"`. Directories are not a special case in the lookup, only
  a key that is not an extension.

- list_tables:

  `function(path)` returning the table names available at `path`,
  cheaply: discovery must not read table data where the format allows it
  (a zip's central directory, a workbook's sheet index). Formats whose
  only index is the data itself (an `.rds`) may read; they are local
  files read once at configure time. Return a character vector, or a
  data frame with a `name` column and an optional `label` column
  carrying a short display string per table (`"CSV 2.1 MB"`).

- read:

  `function(path, tables, ...)` returning one unevaluated expression
  that evaluates to a named list of data frames, restricted to `tables`
  (`NULL` means all). The expression may compose member reads via
  [`format_read_expr()`](https://bristolmyerssquibb.github.io/blockr.io/reference/format_read_expr.md)
  at build time, and may only call reader packages. `...` carries
  uniform member options: an entry that holds files of other formats
  should forward it into each member's
  [`format_read_expr()`](https://bristolmyerssquibb.github.io/blockr.io/reference/format_read_expr.md)
  call, where the member's entry picks the parameters it understands (so
  `sep = ";"` reaches every csv in a folder and everything else ignores
  it).

- options:

  What the container accepts through `read`'s `...`: a named list of
  [format_opt](https://bristolmyerssquibb.github.io/blockr.io/reference/format_opt.md)
  specs, or a `function(path)` returning one, since a container that
  holds files of other formats cannot know its options until it is
  pointed somewhere
  ([`member_options()`](https://bristolmyerssquibb.github.io/blockr.io/reference/member_options.md)
  over its members is the usual answer). `NULL` for a container with
  nothing to tune.

## Value

Invisible `NULL`, called for its side effect.

## See also

[`register_format()`](https://bristolmyerssquibb.github.io/blockr.io/reference/register_format.md),
[`container_list_tables()`](https://bristolmyerssquibb.github.io/blockr.io/reference/container_list_tables.md),
[`container_read_expr()`](https://bristolmyerssquibb.github.io/blockr.io/reference/container_read_expr.md),
[format_opt](https://bristolmyerssquibb.github.io/blockr.io/reference/format_opt.md)
