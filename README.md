# blockr.io

<!-- badges: start -->
[![check](https://github.com/BristolMyersSquibb/blockr.io/actions/workflows/ci.yaml/badge.svg)](https://github.com/BristolMyersSquibb/blockr.io/actions/workflows/ci.yaml)
[![coverage](https://codecov.io/gh/BristolMyersSquibb/blockr.io/graph/badge.svg)](https://app.codecov.io/gh/BristolMyersSquibb/blockr.io)
<!-- badges: end -->

blockr.io provides file I/O blocks for reading and writing data in various formats such as Excel, CSV, Parquet, and more.

## Overview

blockr.io is part of the blockr ecosystem and provides file I/O blocks.

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

Create and launch an empty dashboard:

```r
library(blockr.io)
serve(new_board())
```

This opens a visual interface in your web browser. Add blocks using the "+" button, connect them by dragging, and configure each block through its settings.

## Available Blocks

The unified `new_read_block()` supports multiple sources (browse, upload, URL) and
formats (CSV, Excel, Parquet, etc.) with smart format detection. The `new_write_block()`
can output to various formats and supports both download and filesystem modes.

## Learn More

The [blockr.io website](https://bristolmyerssquibb.github.io/blockr.io/) includes full documentation and examples. For information on the workflow engine, see [blockr.core](https://bristolmyerssquibb.github.io/blockr.core/).
