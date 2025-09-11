
<!-- README.md is generated from README.Rmd. Please edit that file -->

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
devtools::install_github("cynkra/blockr.io")
```

## Example

Several example Excel files are available from
\[readxl::readxl_example()\] as

``` r
readxl::readxl_example("clippy.xlsx")
```

Such a file can then be round-tripped as

options(blockr.volumes = c(home = "~"))


``` r
library(blockr.core)
library(blockr.io)

serve(
  new_board(
    blocks = blocks(
      a = new_readxlsx_block(),
      b = new_writexlsx_block()
    ),
    links = links(ab = new_link("a", "b"))
  )
)

pkgload::load_all(); blockr.core::serve(new_read_block())
```
