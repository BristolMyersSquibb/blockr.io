# The options a format declares

Looks up `x` (a path, or a bare extension) in the single-mode registry
and returns the entry's declared option specs, or an empty list when the
format declares none – which is the answer for most formats, and the
reason a block should ask rather than assume.

## Usage

``` r
format_options(x)
```

## Arguments

- x:

  Character. A file path or a bare extension.

## Value

Named list of
[format_opt](https://bristolmyerssquibb.github.io/blockr.io/reference/format_opt.md)
specs, possibly empty.
