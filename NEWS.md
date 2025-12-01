# blockr.io 0.0.1

Initial CRAN release.

## Features

### Data Import Block
- `new_read_block()`: Read data from various file formats
  - CSV, TSV, and delimited files (via readr)
  - Excel files (via readxl)
  - Arrow formats: Parquet, Feather, IPC (via arrow)
  - Other formats via rio package
  - URL support for remote files
  - Multi-file selection with combine options (rbind, cbind)
  - Advanced CSV options (delimiter, encoding, skip rows, etc.)

### Data Export Block
- `new_write_block()`: Write data to various file formats
  - CSV export (via readr)
  - Excel export with multiple sheets (via writexl)
  - Parquet and Feather export (via arrow)
  - Directory browser for output location
  - Auto-timestamped filenames
  - Multiple dataset export (zipped for CSV/Arrow)

## Documentation
- Full documentation for all exported functions
- Package website with examples
