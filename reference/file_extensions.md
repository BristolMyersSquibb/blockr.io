# Supported file extensions

Returns a character vector of file extensions (without dots) readable in
single mode: the registry's named entries plus the rio fallback's
claims. Useful for sibling packages that need to filter or validate file
paths before passing them to blockr.io. The set grows exactly when the
registry does – a format registered via
[`register_format()`](https://bristolmyerssquibb.github.io/blockr.io/reference/register_format.md)
shows up here, and with it in the file browser filter and upload accept
lists.

## Usage

``` r
file_extensions()
```

## Value

Character vector of file extensions (without dots)
