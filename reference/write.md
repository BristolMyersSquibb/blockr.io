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
  auto_write = FALSE,
  args = list(),
  mode = NULL,
  ...
)
```

## Arguments

- directory:

  Character. Default directory for file output. When non-empty, enables
  server-side writing. Can be configured via
  `options(blockr.write_dir = "/path")` or environment variable
  `BLOCKR_WRITE_DIR`. Default: `""` (empty – download-only until user
  sets a path).

- filename:

  Character. Optional fixed filename (without extension).

  - **If provided**: Writes to the same file path on every save
    (overwrite)

  - **If empty** (default): Manual saves and downloads generate a
    timestamped filename (e.g., `data_20250127_143022.csv`); auto-write
    uses a fixed `data.{ext}` file so repeated writes overwrite instead
    of littering the directory

- format:

  Character. Output format: "csv", "excel", "parquet", or "feather".
  Default: "csv"

- auto_write:

  Logical. When TRUE, automatically writes files when data changes
  (requires a non-empty directory). When FALSE (default), the user must
  click "Save to Server", and each click writes exactly once.

- args:

  Named list of format-specific writing parameters. Only specify values
  that differ from defaults. Available parameters:

  - **For CSV files:** `sep` (default: ","), `quote` (default: TRUE),
    `na` (default: "")

  - **For Excel/Arrow:** Minimal options needed (handled by underlying
    packages)

- mode:

  **\[deprecated\]** Previously selected between "browse" and "download"
  tabs. Now ignored – both download and server-save are always
  available. Kept for backwards compatibility; emits a deprecation
  warning when non-NULL.

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

- Overwrites the file on every save (every data change with auto-write)

- Ideal for automated pipelines

**Empty filename** (`filename = ""`):

- Manual saves and downloads: unique files
  `{directory}/data_YYYYMMDD_HHMMSS.{ext}` – preserves history, prevents
  accidental overwrites

- Auto-write: fixed `{directory}/data.{ext}`, overwritten on each change
  – a timestamp would create one file per upstream change

### Download vs Server Save

Both options are always available in a flat layout (no tabs):

**Download to Browser:**

- Always available via the download button

- Triggers a download to your browser's download folder

**Save to Server:**

- Active when a server directory path is set (non-empty)

- User types a directory path (committed with Enter, blur, or a dropdown
  selection – an "Enter" chip shows while the typed path is not yet
  applied) in the path input

- The target directory is created at write time if missing

- In manual mode each "Save to Server" click writes exactly once; later
  data changes never rewrite the file

- Files persist on server; when running locally, this is your computer's
  file system

### Pipeline Behavior

The block passes its first input through unchanged. Only with
`auto_write = TRUE` does the block expression itself contain the write
(so exported code reproduces the auto-write); manual saves and downloads
happen in their handlers and keep the expression a pure passthrough.

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
#>  $ directory : chr "/tmp/Rtmpcctq3b"
#>  $ filename  : chr "output"
#>  $ format    : chr "csv"
#>  $ auto_write: logi FALSE
#>  $ args      : list()
#>  $ mode      : NULL
#> Constructor: blockr.io::new_write_block()

# Write block for Excel with auto-timestamp
block <- new_write_block(
  directory = tempdir(),
  filename = "",
  format = "excel"
)

if (interactive()) {
  # Launch interactive app
  serve(new_write_block())
}
```
