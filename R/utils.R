zchr_to_null <- function(x) {
  if (nzchar(x)) {
    return(x)
  }

  NULL
}

#' Check if object is a single string
#' @keywords internal
is_string <- function(x) {
  is.character(x) && length(x) == 1L
}

#' Get blockr option with fallback
#' @keywords internal
blockr_option <- function(name, default = NULL) {
  opt_name <- paste0("blockr.", name)
  getOption(opt_name, default)
}

#' Set names helper (base R doesn't export this in older versions)
#' @keywords internal
set_names <- function(x, nm) {
  if (length(x) == 0) {
    return(x)
  }
  names(x) <- nm
  x
}

#' Validate URL format
#' @keywords internal
is_valid_url <- function(url) {
  if (!is_string(url)) {
    return(FALSE)
  }
  if (!nzchar(url)) {
    return(FALSE)
  }
  grepl("^https?://[^\\s]+", url, perl = TRUE)
}

#' Download URL to temporary file
#'
#' Downloads a file from a URL to a temporary location. Extracts file extension
#' from URL if available to help with format detection.
#'
#' @param url Character. URL to download from.
#' @return Path to temporary file containing downloaded data.
#' @importFrom utils download.file
#' @keywords internal
download_url_to_temp <- function(url) {
  stopifnot(is_string(url), nzchar(url))

  # Validate URL format
  if (!is_valid_url(url)) {
    stop("Invalid URL format: ", url, call. = FALSE)
  }

  # Extract extension from URL if possible
  # Handle query parameters by taking only the path part
  url_path <- strsplit(url, "?", fixed = TRUE)[[1]][1]
  ext <- tools::file_ext(basename(url_path))
  if (nzchar(ext)) {
    ext <- paste0(".", ext)
  } else {
    ext <- ""
  }

  # Create temp file with extension
  temp_file <- tempfile(fileext = ext)

  # Use curl if available (handles servers that block R's default user-agent)
  method <- if (nzchar(Sys.which("curl"))) "curl" else "auto"

  # Download with error handling
  tryCatch(
    {
      download.file(url, temp_file, quiet = TRUE, mode = "wb", method = method)
      temp_file
    },
    error = function(e) {
      stop(
        "Failed to download from URL: ",
        url,
        "\n  ",
        e$message,
        call. = FALSE
      )
    }
  )
}

#' Get list of file extensions supported by rio
#'
#' Returns a comprehensive list of file formats that can be handled by rio::import().
#' Used for file browser accept parameter and format validation.
#'
#' @return Character vector of file extensions (without dots)
#' @keywords internal
get_rio_extensions <- function() {
  c(
    # Tabular text (though we prefer readr for CSV/TSV)
    "csv",
    "tsv",
    "txt",
    "fwf",

    # Excel
    "xls",
    "xlsx",
    "xlsm",
    "xlsb",

    # Statistical software
    "sav",
    "zsav", # SPSS
    "dta", # Stata
    "sas7bdat",
    "xpt", # SAS

    # Arrow columnar
    "parquet",
    "feather",
    "arrow",

    # OpenDocument
    "ods",
    "fods",

    # Web and config
    "json",
    "xml",
    "html",
    "yml",
    "yaml",

    # Database
    "dbf",
    "sqlite",
    "db",

    # R formats
    "rds",
    "rdata",
    "rda",

    # Other
    "csvy",
    "arff",
    "rec",
    "mtp",
    "syd"
  )
}

#' File category from extension
#'
#' Categorizes a file by its extension into a broad format family that
#' determines reader dispatch and UI adaptation.
#'
#' @param path Character. File path.
#' @return One of `"csv"`, `"excel"`, `"arrow"`, `"other"`.
#' @export
file_category <- function(path) {
  ext <- tolower(tools::file_ext(path))

  if (ext %in% c("csv", "tsv", "txt", "dat", "tab")) {
    return("csv")
  }

  if (ext %in% c("xls", "xlsx", "xlsm", "xlsb")) {
    return("excel")
  }

  if (ext %in% c("parquet", "feather", "arrow")) {
    return("arrow")
  }

  "other"
}

# Variadic ...args helpers, mirroring blockr.core (not exported). A variadic
# block server receives `...args` as a `reactives` object: slots added through
# the DAG UI (an unnamed link) are stored positionally and have NO display name,
# so `names(...args)` returns "" for them (or NULL when every slot is unnamed).
# The old `names(...args)`-based helper collapsed that to NULL, which wedged the
# block's expr reactive (`req(length(arg_names()) > 0)` failed -> silent error ->
# empty condition banner). These mirror core's slot -> symbol mapping:
# `dot_arg_refs()` gives the symbol each slot is bound to in the eval env (the
# link name for named slots, `.arg1`, `.arg2`, ... for unnamed ones), keyed by
# display name; `dot_arg_values()` pairs those reference names with the realized
# slot values and works on both the live-board `reactives` and the
# `reactiveValues` used in tests. Keep in sync with blockr.core
# R/utils-misc.R (dot_sym/arg_refs/dot_arg_refs/dot_arg_values).
dot_sym <- function(i) {
  paste0(".arg", i)
}

arg_refs <- function(nms) {
  unnamed <- !nzchar(nms)
  replace(nms, unnamed, dot_sym(seq_len(sum(unnamed))))
}

dot_arg_refs <- function(x) {
  nms <- names(x)

  if (is.null(nms)) {
    nms <- character(length(x))
  }

  set_names(arg_refs(nms), nms)
}

dot_arg_values <- function(x) {
  vals <- if (inherits(x, "reactivevalues")) {
    reactiveValuesToList(x)
  } else {
    as.list(x)
  }

  set_names(vals, unname(dot_arg_refs(x)))
}

#' Supported file extensions
#'
#' Returns a character vector of file extensions (without dots) supported by
#' the read block. Useful for sibling packages that need to filter or validate
#' file paths before passing them to blockr.io.
#'
#' @return Character vector of file extensions (without dots)
#' @export
file_extensions <- function() {
  get_rio_extensions()
}

#' Clean up old uploaded files
#'
#' Removes files older than a given age from an upload directory.
#'
#' @param upload_dir Character. Path to the upload directory.
#' @param max_age_days Numeric. Maximum age in days. Files older than this
#'   are removed. Default: 30.
#' @return Invisible NULL.
#' @keywords internal
cleanup_uploads <- function(upload_dir, max_age_days = 30) {
  if (!dir.exists(upload_dir)) {
    return(invisible(NULL))
  }

  files <- list.files(upload_dir, full.names = TRUE)
  if (length(files) == 0) {
    return(invisible(NULL))
  }

  info <- file.info(files)
  cutoff <- Sys.time() - as.difftime(max_age_days, units = "days")
  old_files <- files[!is.na(info$mtime) & info$mtime < cutoff]

  if (length(old_files) > 0) {
    unlink(old_files)
  }

  invisible(NULL)
}
