# Fields for a set of declared options

Renders one field per spec, laid out as this package's settings bands
are. Values come from a block's option list; anything absent shows its
declared default.

## Usage

``` r
format_options_ui(specs, ns, values = list())
```

## Arguments

- specs:

  Named list of
  [format_opt](https://bristolmyerssquibb.github.io/blockr.io/reference/format_opt.md)
  specs, e.g. from
  [`source_options()`](https://bristolmyerssquibb.github.io/blockr.io/reference/source_options.md).

- ns:

  Namespace function,
  [`shiny::NS()`](https://rdrr.io/pkg/shiny/man/NS.html) of the calling
  module's id. Field input ids are the spec names, namespaced.

- values:

  Named list of current option values (deviations only).

## Value

A
[htmltools::tag](https://rstudio.github.io/htmltools/reference/builder.html),
or `NULL` when there are no specs.
