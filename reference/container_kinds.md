# What containers are registered

The keys the container registry currently holds (`"directory"`, `"zip"`,
...). A block that offers container reading should gate its affordances
on this, so nothing offers an action that is guaranteed to fail.

## Usage

``` r
container_kinds()
```

## Value

Character vector of container keys.
