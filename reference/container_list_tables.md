# Discover the tables in a container

Resolves `path` (a directory, or a file whose extension has a container
entry) and returns the table names its entry can see, without reading
table data where the format allows. `container_table_info()` returns the
same discovery with each table's display label (`"CSV 2.1 MB"`,
`"12 rows"`, `""` when the entry has none), for blocks that render a
picker.

## Usage

``` r
container_list_tables(path)

container_table_info(path)
```

## Arguments

- path:

  Character. A resolved location.

## Value

For `container_list_tables()`, a character vector of table names. For
`container_table_info()`, a data frame with columns `name` and `label`.
