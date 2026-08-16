# Push option values back into declared option fields

For an external write: moves the fields to match `values` without
re-rendering them (which would drop focus mid-edit).

## Usage

``` r
format_options_update(session, specs, values = list())
```

## Arguments

- session:

  The module's `session`.

- specs:

  Named list of
  [format_opt](https://bristolmyerssquibb.github.io/blockr.io/reference/format_opt.md)
  specs.

- values:

  Named list of current option values.

## Value

Invisible `NULL`.
