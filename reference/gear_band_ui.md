# Gear button and settings band

The gear-and-band affordance blockr.io's blocks use for advanced
settings: a small gear button, and an in-flow panel that opens beneath
it (class-driven via `blockrIoGearToggle()`, so it pushes content down
rather than overlaying it). Exported so sibling packages can mount the
same affordance with the same styling; the required JS and CSS ride
along as an html dependency.

## Usage

``` r
gear_band_ui(gear_id, band_id, ..., band_label = "Settings")
```

## Arguments

- gear_id, band_id:

  Element ids (namespace them with
  [`shiny::NS()`](https://rdrr.io/pkg/shiny/man/NS.html)). The two are
  wired: the button toggles the band and carries the `aria-controls`
  linkage.

- ...:

  Band content.

- band_label:

  Accessible label for the band region.

## Value

A
[`htmltools::tagList()`](https://rstudio.github.io/htmltools/reference/tagList.html)
with the gear row and the band; place them together at the top of the
block section they configure.

## Details

Lay fields out with `blockr-settings__grid` / `blockr-settings__field`
divs and `blockr-label` labels, matching this package's own bands.
