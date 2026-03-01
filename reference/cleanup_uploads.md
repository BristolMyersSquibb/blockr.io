# Clean up old uploaded files

Removes files older than a given age from an upload directory.

## Usage

``` r
cleanup_uploads(upload_dir, max_age_days = 30)
```

## Arguments

- upload_dir:

  Character. Path to the upload directory.

- max_age_days:

  Numeric. Maximum age in days. Files older than this are removed.
  Default: 30.

## Value

Invisible NULL.
