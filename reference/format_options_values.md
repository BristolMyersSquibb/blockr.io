# Read declared option fields back off the inputs

The inverse of
[`format_options_ui()`](https://bristolmyerssquibb.github.io/blockr.io/reference/format_options_ui.md):
collects the fields into the named list a block carries as its options,
dropping every value still at its declared default so an untouched block
stays empty.

## Usage

``` r
format_options_values(input, specs)
```

## Arguments

- input:

  The module's `input`.

- specs:

  Named list of
  [format_opt](https://bristolmyerssquibb.github.io/blockr.io/reference/format_opt.md)
  specs.

## Value

Named list of option values; empty when nothing deviates.
