# blockr.io

<!-- badges: start -->
[![check](https://github.com/BristolMyersSquibb/blockr.io/actions/workflows/ci.yaml/badge.svg)](https://github.com/BristolMyersSquibb/blockr.io/actions/workflows/ci.yaml)
[![coverage](https://codecov.io/gh/BristolMyersSquibb/blockr.io/graph/badge.svg)](https://app.codecov.io/gh/BristolMyersSquibb/blockr.io)
<!-- badges: end -->

blockr.io provides file I/O blocks for reading and writing data in various formats. Load CSV, Excel, Parquet, and more through visual interfaces - no code required.

## Installation

```r
install.packages("blockr.io")
```
Or install the development version from GitHub:

```r
# install.packages("pak")
pak::pak("BristolMyersSquibb/blockr.io")
```

## Getting Started

```r
library(blockr.io)
serve(new_board())
```

This opens a visual interface in your browser. Add blocks using the "+" button and connect them to build data pipelines.

## Available Blocks

### Read Block

Load data from multiple sources with automatic format detection:

- **From Browser**: Upload files via drag-and-drop (persisted across sessions)
- **From Server**: Browse the file system directly
- **From URL**: Download data from web URLs

Supports CSV, Excel, Parquet, Feather, JSON, SPSS, Stata, SAS, and more.

### Write Block

Export data to various formats with two output modes:

- **To Browser**: Download files directly (recommended for most users)
- **To Server**: Write to the file system (for automated pipelines)

Supports CSV, Excel, Parquet, and Feather. Multiple inputs become Excel sheets or ZIP archives.

## Learn More

See `vignette("blockr-io-showcase")` for screenshots and detailed examples.

The [blockr.io website](https://bristolmyerssquibb.github.io/blockr.io/) includes full documentation. For the workflow engine, see [blockr.core](https://bristolmyerssquibb.github.io/blockr.core/).
