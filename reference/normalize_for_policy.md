# Normalize a path for policy checks

[`base::normalizePath()`](https://rdrr.io/r/base/normalizePath.html)
resolves symlinks and `..` only for the part of a path that exists on
disk; for a path (or path prefix) that does not exist — common for write
targets and for hand-crafted serialized boards — it leaves `..` segments
in place on Linux, which would let `study/../other` slip past a
[`startsWith()`](https://rdrr.io/r/base/startsWith.html) allowlist
check. So after
[`normalizePath()`](https://rdrr.io/r/base/normalizePath.html) we also
collapse `.`/`..` lexically, independent of what exists on disk.

## Usage

``` r
normalize_for_policy(path)
```

## Arguments

- path:

  Character vector of paths.

## Value

Character vector of normalized, dot-collapsed paths.
