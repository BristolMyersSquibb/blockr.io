# Unified file reading block

A single block for reading files in various formats with smart UI that
adapts based on detected file type. Supports "From Browser" (upload) and
"From Server" (browse) modes with persistent storage for uploaded files.

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

  Character vector of file paths to pre-load. When provided,
  automatically switches to "path" mode regardless of the source
  parameter.

- source:

  Either "upload" for file upload widget, "path" for file browser, or
  "url" for URL download. Default: "upload". Automatically set based on
  path parameter.

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

The block supports three modes:

**From Browser mode** (upload):

- User uploads files from their computer via the browser

- Files are copied to persistent storage directory (upload_path)

- State stores permanent file paths

- Works across R sessions with state restoration

**From Server mode** (path):

- User picks files that already exist on the server

- No file copying, reads directly from original location

- State stores selected file paths

- When running locally, this is your computer's file system

**URL mode:**

- User provides a URL to a data file

- File is downloaded to temporary location each time

- Always fetches fresh data from URL

- State stores the URL (not file path)

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

- **volumes**: File browser mount points. Set via
  `options(blockr.volumes = c(name = "path"))` or environment variable
  `BLOCKR_VOLUMES`. Default: `c(home = "~")`

- **upload_path**: Directory for persistent file storage. Set via
  `options(blockr.upload_path = "/path")` or environment variable
  `BLOCKR_UPLOAD_PATH`. Default: `rappdirs::user_data_dir("blockr")`

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
#>  $ path   : chr "/tmp/Rtmph5uf70/file190b1b542039.csv"
#>  $ source : chr "upload"
#>  $ combine: chr "auto"
#>  $ args   : list()
#> Constructor: blockr.io::new_read_block()

# With custom CSV parameters
block <- new_read_block(
  path = csv_file,
  args = list(n_max = 3)
)

if (FALSE) { # \dontrun{
# Launch interactive app
serve(new_read_block())
} # }
```
