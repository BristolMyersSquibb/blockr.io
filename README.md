# blockr.io

<!-- badges: start -->

<!-- badges: end -->

Upload and download multiple file formats such as Excel, csv, xpt, etc.
to and from a blockr data pipeline.

## Installation

You can install the development version of blockr.io from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("BristolMyersSquibb/blockr.io")
```

## Example

Read a CSV file and write to Excel:

``` r
library(blockr.core)
library(blockr.io)

serve(
  new_board(
    blocks = blocks(
      a = new_read_block(source = "path"),  # Browse for files
      b = new_write_block(format = "excel", mode = "download")
    ),
    links = links(ab = new_link("a", "b"))
  )
)
```

The unified `new_read_block()` supports multiple sources (browse, upload, URL) and
formats (CSV, Excel, Parquet, etc.) with smart format detection. The `new_write_block()`
can output to various formats and supports both download and filesystem modes.
