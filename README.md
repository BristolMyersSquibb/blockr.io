# blockr.io

<!-- badges: start -->
[![check](https://github.com/BristolMyersSquibb/blockr.io/actions/workflows/check.yaml/badge.svg)](https://github.com/BristolMyersSquibb/blockr.io/actions/workflows/check.yaml)
[![coverage](https://codecov.io/gh/BristolMyersSquibb/blockr.io/graph/badge.svg)](https://app.codecov.io/gh/BristolMyersSquibb/blockr.io)
<!-- badges: end -->

Upload and download multiple file formats such as Excel, csv, xpt, etc.
to and from a blockr data pipeline.

## Overview

blockr.io is part of the blockr ecosystem. blockr.core provides the workflow engine, blockr.ui provides the visual interface, and blockr.io provides file I/O blocks for reading and writing data in various formats. These packages work together to create interactive data workflows with flexible file handling.

## Installation

```r
# install.packages("pak")
pak::pak("BristolMyersSquibb/blockr.io")
pak::pak("BristolMyersSquibb/blockr.core")
pak::pak("BristolMyersSquibb/blockr.ui")
```

## Getting Started

Create and launch an empty dashboard:

```r
library(blockr.core)
library(blockr.ui)
library(blockr.io)
serve(new_dag_board())
```

This opens a visual interface in your web browser. Add blocks using the "+" button, connect them by dragging, and configure each block through its settings.

## Available Blocks

The unified `new_read_block()` supports multiple sources (browse, upload, URL) and
formats (CSV, Excel, Parquet, etc.) with smart format detection. The `new_write_block()`
can output to various formats and supports both download and filesystem modes.

## Learn More

The [blockr.io website](https://bristolmyerssquibb.github.io/blockr.io/) includes full documentation and examples. For information on the broader blockr ecosystem, see [blockr.core](https://bristolmyerssquibb.github.io/blockr.core/) and [blockr.dplyr](https://bristolmyerssquibb.github.io/blockr.dplyr/).
