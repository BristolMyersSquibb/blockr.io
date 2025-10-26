# blockr.io Restructuring Guide

**Purpose:** This document describes the unified file reading architecture for blockr.io, designed for CRAN readiness and user-friendliness.

## Table of Contents

1. [Overview](#overview)
2. [Architecture](#architecture)
3. [File Handling Modes](#file-handling-modes)
4. [Smart Adaptive UI](#smart-adaptive-ui)
5. [Reader Functions](#reader-functions)
6. [Multi-File Support](#multi-file-support)
7. [Implementation Details](#implementation-details)

---

## Overview

### Design Goals

1. **Single unified block** (`new_read_block()`) replaces all format-specific blocks
2. **Smart UI** that adapts based on detected file type
3. **Persistent file storage** for uploads with state restoration
4. **Modular reader functions** in separate files for maintainability
5. **Always returns data.frame** (never lists of data.frames)

### Key Benefits

- **User-friendly:** One block for all file types
- **Automatic detection:** File format determined from extension
- **Smart defaults:** Advanced options only when needed
- **State restoration:** Uploaded files persist across sessions
- **Maintainable:** Specialized readers in separate files

---

## Architecture

```
new_read_block()  [in read.R]
    │
    ├─> UI Layer
    │   ├─> Upload mode: fileInput + persistence to shared storage
    │   ├─> Browse mode: shinyFiles browser to file system
    │   ├─> URL mode: textInput + download to temp file
    │   └─> Conditional panels based on detected file type
    │
    ├─> Path Abstraction (all modes converge here)
    │   ├─> Upload → permanent path
    │   ├─> Browse → direct path
    │   └─> URL → temp path (re-downloaded each time)
    │
    ├─> File Type Detection
    │   └─> Extension-based detection (detect_file_category)
    │
    ├─> Reader Delegation
    │   ├─> read_csv_blockr()  [in reader_csv.R] for .csv, .tsv, .txt
    │   └─> read_rio_blockr()  [in reader_rio.R] for all other formats
    │
    └─> Multi-File Handling
        └─> Combine strategies: rbind, cbind, first, auto
```

### File Structure

**New/Modified:**
- `read.R` - Main unified block with smart UI
- `reader_csv.R` - Specialized CSV/text file reading with readr
- `reader_rio.R` - Fallback reader for all other formats with rio

**Removed:**
- `csv.R` - Replaced by unified architecture
- `xlsx.R` - Replaced by unified architecture
- `xpt.R` - Replaced by unified architecture
- `multi.R` - Functionality integrated into main block

**Unchanged:**
- `pkg.R`, `utils.R`, `zzz.R`

---

## File Handling Modes

The block supports three modes, all converging to the same core abstraction: a file path that can be read.

### Upload Mode

**User workflow:**
1. User clicks "Upload" and selects file(s)
2. Shiny creates temporary file(s)
3. Block copies file(s) to persistent storage directory
4. State stores permanent path(s)

**State restoration:**
- Block reads from stored permanent path
- Works across R sessions
- No re-upload needed

**Storage directory configuration:**
1. User option: `options(blockr.upload_path = "/my/path")`
2. Environment variable: `BLOCKR_UPLOAD_PATH`
3. Default: `rappdirs::user_data_dir("blockr")`

**Benefits:**
- Uploaded files persist across sessions
- State restoration works reliably
- Compatible with project-based workflows

### Browse Mode

**User workflow:**
1. User clicks "Browse" button
2. ShinyFiles file browser opens
3. User selects file(s) from file system
4. State stores selected path(s) directly

**State restoration:**
- Block reads from original file location
- Assumes file hasn't moved
- No file copying needed

**Benefits:**
- Direct access to existing files
- No storage overhead
- Works with large files on network drives

### URL Mode

**User workflow:**
1. User selects "From URL" radio button
2. User enters URL to a data file
3. Block validates URL format (http:// or https://)
4. Block downloads file to temporary location
5. Temporary file path is used (just like upload/browse)

**State restoration:**
- State stores the URL (not the temp file path)
- On restoration, file is re-downloaded from URL
- Always fetches fresh data from remote source
- Temporary files are cleaned up automatically

**URL configuration:**
```r
# Initialize with URL
new_read_block(url = "https://example.com/data.csv")
```

**Supported URL types:**
- Direct file downloads: `https://example.com/data.xlsx`
- GitHub raw files: `https://raw.githubusercontent.com/user/repo/main/data.csv`
- Open data portals: `https://www.pxweb.bfs.admin.ch/sq/...`
- Any public URL returning a file

**Benefits:**
- Easy access to remote datasets
- Always fetches fresh data
- Great for examples and teaching
- No local storage needed for remote data

**Limitations:**
- Public URLs only (no authentication)
- Simple GET requests only
- No query parameter customization
- Not suitable for complex APIs

### Design Rationale: URL Mode vs. API Block

**Why include URL mode in the read block?**

The URL mode is included in `new_read_block()` because downloading a file from a URL is conceptually the same operation as reading a local file. The workflow is:

1. **Upload** → file lives in shared storage → path
2. **Browse** → file lives on file system → path
3. **URL** → file lives at URL → download to temp → path

All three modes converge to the same abstraction: a file path that can be passed to reader functions. The logic for format detection, UI adaptation, and reading is identical regardless of source.

**Scope of URL mode:**

URL mode is designed for **simple direct file downloads**:
- ✅ Public datasets (open data portals, research data)
- ✅ GitHub raw files
- ✅ Direct download URLs (`https://example.com/data.csv`)
- ✅ Teaching examples with sample data URLs

**Out of scope (requires separate API block):**

For complex API interactions, a separate `new_api_block()` would be more appropriate:
- ❌ APIs requiring authentication (headers, tokens, OAuth)
- ❌ APIs with customizable query parameters
- ❌ REST APIs requiring JSON transformation
- ❌ APIs with pagination
- ❌ POST/PUT/DELETE requests
- ❌ Rate limiting and retry logic
- ❌ API-specific error handling

**Design boundary:**

- **Read block handles:** "I have a file (local, uploaded, or at a simple URL)"
- **Future API block handles:** "I want to interact with an API endpoint with custom parameters"

This separation keeps the read block focused on its core purpose: reading data files, regardless of whether they're local or remote. Complex API interactions are a different problem domain that would benefit from specialized tooling.

### Unified State Format

All three modes store their data source information in a consistent structure:

```r
state = list(
  paths = reactiveVal(c("/path/to/file1.csv")),  # File path (upload/browse) or temp path (url)
  url = reactiveVal("https://example.com/data.csv"),  # Only used in URL mode
  source = reactiveVal("upload"),  # "upload", "path", or "url"
  combine = reactiveVal("auto"),
  volumes = reactiveVal(c(home = "~")),
  # Format-specific options appear conditionally:
  csv_sep = reactiveVal(","),
  csv_quote = reactiveVal("\""),
  csv_encoding = reactiveVal("UTF-8"),
  excel_sheet = reactiveVal(NULL),
  excel_range = reactiveVal(NULL)
)
```

---

## Smart Adaptive UI

The UI adapts based on detected file type, showing relevant options only when needed.

### Base UI (Always Visible)

```r
# Source selection
radioButtons("source", "File source",
             choices = c("Upload" = "upload", "Browse" = "browse"))

# Conditional: Upload widget OR Browse button
conditionalPanel(condition = "source == 'upload'",
  fileInput("upload", "Select files", multiple = TRUE)
)
conditionalPanel(condition = "source == 'browse'",
  shinyFilesButton("browse", "Browse files", multiple = TRUE)
)

# File status display
textOutput("file_info")  # "Selected: data.csv"
```

### Conditional Advanced Options

Appear only after file selection, based on detected type:

**CSV/Text Files (.csv, .tsv, .txt):**
```r
tags$details(
  tags$summary("⚙️ Advanced CSV Options"),
  textInput("csv_sep", "Delimiter", value = ","),
  textInput("csv_quote", "Quote character", value = "\""),
  selectInput("csv_encoding", "Encoding",
              choices = c("UTF-8", "Latin-1", "Windows-1252"))
)
```

**Excel Files (.xls, .xlsx):**
```r
tags$details(
  tags$summary("⚙️ Advanced Excel Options"),
  textInput("excel_sheet", "Sheet name or number", value = ""),
  textInput("excel_range", "Cell range (e.g., A1:C10)", value = "")
)
```

**Other Formats:**
- No additional options (rio handles automatically)
- Optional: Show info about detected format

### Multi-File Options

```r
selectInput("combine", "Multiple files strategy",
  choices = c(
    "Auto (rbind with fallback)" = "auto",
    "Row bind (rbind)" = "rbind",
    "Column bind (cbind)" = "cbind",
    "First file only" = "first"
  )
)
```

---

## Reader Functions

### CSV Reader (`reader_csv.R`)

**Purpose:** Fast, reliable CSV reading with readr

```r
read_csv_blockr <- function(path,
                            sep = ",",
                            quote = "\"",
                            encoding = "UTF-8",
                            skip = 0,
                            n_max = Inf,
                            col_names = TRUE,
                            ...) {
  # Use readr for CSV files
  # Handles encoding, delimiters, quotes intelligently
  # Returns clean data.frame
}
```

**Key features:**
- Uses `readr::read_delim()` for flexibility
- Smart type detection
- Handles various encodings
- Fast for large files
- Clear error messages

### Rio Reader (`reader_rio.R`)

**Purpose:** Fallback for all other formats using rio's auto-detection

```r
read_rio_blockr <- function(path,
                            format = NULL,
                            ...) {
  # Use rio::import() with format auto-detection
  # Handles: Excel, SPSS, SAS, Stata, Parquet, JSON, etc.
  # Normalizes output (e.g., IDate → Date)
  # Returns data.frame
}
```

**Supported formats (via rio):**
- Excel: .xls, .xlsx, .xlsm
- Statistical: .sav, .dta, .sas7bdat, .xpt
- Modern: .parquet, .feather, .arrow
- Web: .json, .xml
- Database: .dbf, .sqlite
- R: .rds, .rdata

**Key features:**
- Format auto-detection from extension
- Consistent data.frame output
- IDate → Date conversion for compatibility
- Handles edge cases automatically

---

## Multi-File Support

### Combination Strategies

When multiple files are selected:

**1. Auto (default):**
- Attempts `rbind` (row-bind)
- If incompatible columns: falls back to first file
- User-friendly default

**2. rbind (Row bind):**
- Combines files vertically
- Requires same column names
- Errors if incompatible

**3. cbind (Column bind):**
- Combines files horizontally
- Requires same row count
- Useful for split datasets

**4. First file only:**
- Ignores additional files
- Simple fallback option

### Implementation

```r
# Single file: direct read
if (length(paths) == 1) {
  result <- read_file(paths[1])
}

# Multiple files: combine based on strategy
if (length(paths) > 1) {
  dfs <- lapply(paths, read_file)

  result <- switch(combine_strategy,
    "auto" = tryCatch(
      do.call(rbind, dfs),
      error = function(e) dfs[[1]]
    ),
    "rbind" = do.call(rbind, dfs),
    "cbind" = do.call(cbind, dfs),
    "first" = dfs[[1]]
  )
}
```

---

## Implementation Details

### File Type Detection

```r
detect_file_type <- function(path) {
  ext <- tolower(tools::file_ext(path))

  if (ext %in% c("csv", "tsv", "txt")) return("csv")
  if (ext %in% c("xls", "xlsx", "xlsm")) return("excel")
  return("other")
}
```

### Upload Persistence

```r
persist_upload <- function(temp_path, original_name) {
  # Get storage directory
  storage_dir <- getOption("blockr.upload_path",
    Sys.getenv("BLOCKR_UPLOAD_PATH",
      rappdirs::user_data_dir("blockr")
    )
  )

  # Create if doesn't exist
  dir.create(storage_dir, recursive = TRUE, showWarnings = FALSE)

  # Generate unique filename
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  safe_name <- gsub("[^A-Za-z0-9._-]", "_", original_name)
  permanent_path <- file.path(storage_dir,
                              paste0(timestamp, "_", safe_name))

  # Copy file
  file.copy(temp_path, permanent_path, overwrite = FALSE)

  permanent_path
}
```

### State Structure

```r
list(
  expr = reactive({
    # Build expression to read file(s)
    # Delegates to appropriate reader
    # Handles multi-file combination
  }),
  state = list(
    paths = reactiveVal(character()),
    source = reactiveVal("upload"),
    combine = reactiveVal("auto"),
    volumes = reactiveVal(c(home = "~")),
    # CSV options (shown conditionally)
    csv_sep = reactiveVal(","),
    csv_quote = reactiveVal("\""),
    csv_encoding = reactiveVal("UTF-8"),
    # Excel options (shown conditionally)
    excel_sheet = reactiveVal(NULL),
    excel_range = reactiveVal(NULL)
  )
)
```

---

## Summary

The restructured blockr.io provides:

✅ Single unified block for all file types
✅ Smart adaptive UI based on file type
✅ Persistent storage for uploaded files
✅ Modular, maintainable reader functions
✅ Consistent data.frame output
✅ Multi-file support with flexible combining
✅ State restoration across sessions

**The result is a user-friendly, CRAN-ready package that handles file I/O elegantly.**
