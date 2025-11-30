# Generate filename for write operations

Generate filename for write operations

## Usage

``` r
generate_filename(filename = "", timestamp = Sys.time())
```

## Arguments

- filename:

  Character. User-specified filename (without extension). If empty or
  NULL, generates timestamped filename.

- timestamp:

  POSIXct timestamp for auto-generated names. Default: Sys.time()

## Value

Character. Base filename without extension
