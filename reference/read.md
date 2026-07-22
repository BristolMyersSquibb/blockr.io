# Unified file reading block

A single block for reading files in various formats with smart UI that
adapts based on detected file type. Supports "From Browser" (upload) and
"Location" (path/URL input) modes with persistent storage for uploaded
files.

## Usage

``` r
new_read_block(
  path = character(),
  source = "upload",
  combine = "auto",
  args = list(),
  ...
)
```

## Arguments

- path:

  Character vector of file paths to pre-load. Accepts local paths and
  URLs. When provided, automatically switches to "path" mode regardless
  of the source parameter.

- source:

  Either "upload" for file upload widget or "path" for path/URL input.
  Default: "upload". Automatically set based on path parameter.

- combine:

  Strategy for combining multiple files: "auto", "rbind", "cbind",
  "first"

- args:

  Named list of format-specific reading parameters. Only specify values
  that differ from defaults. Available parameters:

  - **For CSV files:** `sep` (default: ","), `quote` (default: '"'),
    `encoding` (default: "UTF-8"), `skip` (default: 0), `n_max`
    (default: Inf), `col_names` (default: TRUE)

  - **For Excel files:** `sheet` (default: NULL), `range` (default:
    NULL), `skip` (default: 0), `n_max` (default: Inf), `col_names`
    (default: TRUE)

- ...:

  Forwarded to
  [`blockr.core::new_data_block()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_data_block.html)

## Value

A blockr data block that reads file(s) and returns a data.frame.

## Details

### File Handling Modes

The block supports two modes:

**From Browser mode** (upload):

- User uploads files from their computer via the browser

- Files are copied to persistent storage directory (upload_path)

- State stores permanent file paths

- Works across R sessions with state restoration

**Location mode** (path):

- User enters a file path or URL in a text input with autocomplete

- The path is committed (and the file read) on Enter, blur, or a
  dropdown selection — never while typing; an "Enter" chip shows while
  the typed path is not yet applied

- For server paths: reads directly from original location

- For URLs: downloads to a temporary file each time

- When a board-level data directory is set, paths are resolved relative
  to it

### Smart Adaptive UI

After file selection, the UI detects file type and shows relevant
options:

- **CSV/TSV:** Delimiter, quote character, encoding options

- **Excel:** Sheet selection, cell range

- **Other formats:** Minimal or no options (handled automatically)

### Multi-file Support

When multiple files are selected:

- **"auto"**: Attempts rbind, falls back to first file if incompatible

- **"rbind"**: Row-binds files (requires same columns)

- **"cbind"**: Column-binds files (requires same row count)

- **"first"**: Uses only the first file

## Configuration

The following settings are retrieved from options and not stored in
block state:

- **upload_path**: Directory for persistent file storage. Set via
  `options(blockr.upload_path = "/path")` or environment variable
  `BLOCKR_UPLOAD_PATH`. Default: `tools::R_user_dir("blockr", "data")`

## Examples

``` r
# Create a read block for a CSV file
csv_file <- tempfile(fileext = ".csv")
write.csv(mtcars[1:5, ], csv_file, row.names = FALSE)
block <- new_read_block(path = csv_file)
block
#> <read_block<data_block<block>>>
#> Name: "Read"
#> No data inputs
#> Initial block state:
#>  $ path   : chr "/tmp/RtmpV4Zk3y/file19c57a6b9127.csv"
#>  $ source : chr "upload"
#>  $ combine: chr "auto"
#>  $ args   : list()
#> Constructor: blockr.io::new_read_block()

# With custom CSV parameters
block <- new_read_block(
  path = csv_file,
  args = list(n_max = 3)
)

if (interactive()) {
  # Launch interactive app
  serve(new_read_block())
}
```
