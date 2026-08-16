# The options a set of files understands

The union of the members' declared options, which is what a container
offers: uniform member options are handed to every member, so an option
any member takes is settable and the members that do not take it ignore
it – exactly what the emitted read expression does. Containers holding
files of other formats declare `options = function(path)` in terms of
this.

## Usage

``` r
member_options(paths)
```

## Arguments

- paths:

  Character vector of member paths (or names; only the extension is
  read).

## Value

Named list of
[format_opt](https://bristolmyerssquibb.github.io/blockr.io/reference/format_opt.md)
specs, possibly empty.
