# Unified file writing block

A variadic block for writing dataframes to files in various formats.
Accepts multiple input dataframes and handles single files, multi-sheet
Excel, or ZIP archives depending on format and number of inputs.

## Usage

``` r
new_write_block(
  directory = "",
  filename = "",
  format = "csv",
  mode = "download",
  auto_write = FALSE,
  args = list(),
  ...
)
```

## Arguments

- directory:

  Character. Default directory for file output (browse mode only). Can
  be configured via `options(blockr.write_dir = "/path")` or environment
  variable `BLOCKR_WRITE_DIR`. Default: current working directory.

- filename:

  Character. Optional fixed filename (without extension).

  - **If provided**: Writes to same file path on every upstream change
    (auto-overwrite)

  - **If empty** (default): Generates timestamped filename (e.g.,
    `data_20250127_143022.csv`)

- format:

  Character. Output format: "csv", "excel", "parquet", or "feather".
  Default: "csv"

- mode:

  Character. Either "download" for "To Browser" (triggers browser
  download), or "browse" for "To Server" (writes to server filesystem).
  Default: "download"

- auto_write:

  Logical. When TRUE, automatically writes files when data changes
  (browse mode only). When FALSE (default), user must click "Submit"
  button to save. Has no effect in download mode.

- args:

  Named list of format-specific writing parameters. Only specify values
  that differ from defaults. Available parameters:

  - **For CSV files:** `sep` (default: ","), `quote` (default: TRUE),
    `na` (default: "")

  - **For Excel/Arrow:** Minimal options needed (handled by underlying
    packages)

- ...:

  Forwarded to
  [`blockr.core::new_transform_block()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_transform_block.html)

## Value

A blockr transform block that writes dataframes to files

## Details

### Variadic Inputs

This block accepts multiple dataframe inputs (1 or more) similar to
`bind_rows_block`. Inputs can be numbered ("1", "2", "3") or named
("sales_data", "inventory"). Input names are used for sheet names
(Excel) or filenames (multi-file ZIP).

### File Output Behavior

**Single input:**

- Writes single file in specified format

- Filename: `{filename}.{ext}` or `data_{timestamp}.{ext}`

**Multiple inputs + Excel:**

- Single Excel file with multiple sheets

- Sheet names derived from input names

**Multiple inputs + CSV/Arrow:**

- Single ZIP file containing individual files

- Each file named from input names

### Filename Behavior

**Fixed filename** (`filename = "output"`):

- Reproducible path: always writes to `{directory}/output.{ext}`

- Overwrites file on every upstream data change

- Ideal for automated pipelines

**Auto-timestamped** (`filename = ""`):

- Unique files: `{directory}/data_YYYYMMDD_HHMMSS.{ext}`

- Preserves history, prevents accidental overwrites

- Safe default behavior

### Mode: To Browser vs To Server

**To Browser mode** (download):

- Exports files to your computer

- Triggers a download to your browser's download folder

- Useful for exporting results

**To Server mode** (browse):

- Saves files directly on the server

- User selects directory with file browser

- Files persist on server

- When running locally, this is your computer's file system

## Examples

``` r
# Create a write block for CSV output
block <- new_write_block(
  directory = tempdir(),
  filename = "output",
  format = "csv"
)
block
#> <write_block<rbind_block<transform_block<block>>>>
#> Name: "Write"
#> Indefinite arity
#> Initial block state:
#>  $ directory : chr "/tmp/RtmpWm1JFR"
#>  $ filename  : chr "output"
#>  $ format    : chr "csv"
#>  $ mode      : chr "download"
#>  $ auto_write: logi FALSE
#>  $ args      : list()
#> Constructor: blockr.io::new_write_block()

# Write block for Excel with auto-timestamp
block <- new_write_block(
  directory = tempdir(),
  filename = "",
  format = "excel"
)

if (FALSE) { # \dontrun{
# Launch interactive app
serve(new_write_block())
} # }
```
