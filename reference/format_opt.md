# Declare a format's reader options

The option kinds an entry may declare through
[`register_format()`](https://bristolmyerssquibb.github.io/blockr.io/reference/register_format.md)'s
`options` argument. Each returns one spec: a label, a default, and
enough shape for a block to render a field and read it back.

## Usage

``` r
opt_choice(label, choices, default, create = FALSE)

opt_text(label, default = "", placeholder = NULL)

opt_number(label, default = 0, placeholder = NULL)

opt_flag(label, default = TRUE)
```

## Arguments

- label:

  Field label, shown to the user.

- choices:

  Named character vector of choices (names are labels).

- default:

  The value the reader uses when the option is unset.

- create:

  For `opt_choice()`, whether the user may type a value that is not in
  `choices`.

- placeholder:

  Hint shown in an empty field. Defaults to naming the default value.

## Value

A `blockr_format_opt` object.

## Details

A value equal to the declared default is dropped when a block collects
its options, so a block nobody has touched carries no options at all and
serializes clean.

## See also

[`register_format()`](https://bristolmyerssquibb.github.io/blockr.io/reference/register_format.md),
[`format_options()`](https://bristolmyerssquibb.github.io/blockr.io/reference/format_options.md)
