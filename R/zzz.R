register_io_blocks <- function() {
  # nocov start
  register_blocks(
    "new_read_block",
    name = "Import Data",
    description = "Read and load data from files on your computer, server, or web. Supports CSV, Excel, SPSS, SAS, Stata, Parquet, and more.",
    category = "input",
    icon = "file-earmark-arrow-up",
    arguments = read_block_arguments(),
    package = utils::packageName(),
    overwrite = TRUE
  )

  register_blocks(
    "new_write_block",
    name = "Export Data",
    description = "Write and save your data to CSV, Excel, Parquet, or Feather files. Download directly or save to server.",
    category = "output",
    icon = "save",
    arguments = write_block_arguments(),
    package = utils::packageName(),
    overwrite = TRUE
  )

  register_blocks(
    "new_download_block",
    name = "Download Data",
    description = "Download your data to the browser as CSV, Excel, Parquet, or Feather. A lighter alternative to Export Data when no server-side save is needed.",
    category = "output",
    icon = "download",
    arguments = download_block_arguments(),
    package = utils::packageName(),
    overwrite = TRUE
  )
}

#' @noRd
read_block_arguments <- function() {
  structure(
    c(
      path = paste0(
        "Character vector of file paths to read. Multiple paths are ",
        "combined according to `combine`."
      ),
      source = paste0(
        "Where the file comes from. One of \"upload\" (user uploads at ",
        "runtime) or \"path\" (server-side path). Default \"upload\"."
      ),
      combine = paste0(
        "How to combine multi-file input. One of \"auto\", \"rbind\" ",
        "(row-bind matching schemas), \"cbind\" (column-bind), or ",
        "\"first\" (use only the first file). Default \"auto\"."
      ),
      args = paste0(
        "Named list of extra arguments forwarded to the underlying reader ",
        "(e.g. `list(sheet = 2, col_types = \"c\")` for Excel). Format is ",
        "auto-detected from file extension."
      )
    ),
    examples = list(
      path = list("/data/trial.csv"),
      source = "path",
      combine = "auto",
      args = list()
    ),
    prompt = paste(
      "Reads a single file or multiple files into a data frame. Supports",
      "CSV, TSV, Excel (.xlsx/.xls), SPSS (.sav), SAS (.sas7bdat), Stata",
      "(.dta), Parquet, Feather, RDS, and JSON. Format is auto-detected",
      "from the file extension.",
      "\n\nUse `source = \"upload\"` for user-uploaded files and",
      "`source = \"path\"` for server-side paths. For multi-table Excel",
      "or ZIP files, prefer dm_read_block which returns a dm object."
    )
  )
}

#' @noRd
write_block_arguments <- function() {
  structure(
    c(
      directory = "Character. Output directory path.",
      filename = paste0(
        "Character. Base filename (no extension). Extension is derived ",
        "from `format`."
      ),
      format = paste0(
        "Output format. One of \"csv\", \"excel\", \"parquet\", ",
        "\"feather\". Default \"csv\"."
      ),
      auto_write = paste0(
        "Logical. TRUE writes on every upstream update; FALSE (default) ",
        "writes only on explicit user action."
      ),
      args = paste0(
        "Named list of extra arguments forwarded to the underlying writer."
      )
    ),
    examples = list(
      directory = "/out",
      filename = "trial-cleaned",
      format = "csv",
      auto_write = FALSE,
      args = list()
    ),
    prompt = paste(
      "Writes a data frame to disk in CSV / Excel / Parquet / Feather",
      "form. Use as a terminal block to export cleaned or transformed data."
    )
  )
}

#' @noRd
download_block_arguments <- function() {
  structure(
    c(
      filename = paste0(
        "Character. Base filename (no extension). Extension is derived ",
        "from `format`."
      ),
      format = paste0(
        "Output format. One of \"csv\", \"excel\", \"parquet\", ",
        "\"feather\". Default \"csv\"."
      ),
      args = paste0(
        "Named list of extra arguments forwarded to the underlying writer."
      )
    ),
    examples = list(
      filename = "trial-cleaned",
      format = "csv",
      args = list()
    ),
    prompt = paste(
      "Downloads a data frame to the user's browser as CSV / Excel /",
      "Parquet / Feather. Use as a terminal block when no server-side",
      "save is needed."
    )
  )
}

.onLoad <- function(libname, pkgname) {
  register_io_blocks()

  invisible(NULL)
} # nocov end
