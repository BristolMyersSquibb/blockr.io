#' Supported write formats
#'
#' Returns a named character vector of supported output formats for writing
#' data frames. Names are display labels, values are format identifiers.
#' Used to populate format dropdowns in the write block UI.
#'
#' @return A named character vector.
#' @export
write_formats <- function() {
  c(
    "CSV" = "csv",
    "Excel" = "excel",
    "Parquet" = "parquet",
    "Feather" = "feather"
  )
}

#' File extension for a write format
#'
#' Single source of truth mapping a `write_formats()` value to its output file
#' extension. Used by the internal `write_expr()` builder and by the download
#' handlers of [`new_write_block()`] and [`new_download_block()`].
#'
#' @param format Character. One of the values in [`write_formats()`].
#' @param needs_zip Logical. If `TRUE`, returns `".zip"` regardless of format
#'   (for multi-input non-Excel downloads). Default: `FALSE`.
#'
#' @return Character scalar, e.g. `".csv"`, `".xlsx"`, `".parquet"`,
#'   `".feather"`, or `".zip"`.
#' @keywords internal
format_extension <- function(format, needs_zip = FALSE) {
  if (isTRUE(needs_zip) && format != "excel") {
    return(".zip")
  }
  switch(format,
    csv = ".csv",
    excel = ".xlsx",
    parquet = ".parquet",
    feather = ".feather",
    stop(sprintf("Unsupported write format: '%s'", format))
  )
}


#' Generate filename for write operations
#'
#' @param filename Character. User-specified filename (without extension).
#'   If empty or NULL, generates timestamped filename.
#' @param timestamp POSIXct timestamp for auto-generated names. Default: Sys.time()
#'
#' @return Character. Base filename without extension
#' @keywords internal
generate_filename <- function(filename = "", timestamp = Sys.time()) {
  if (is.null(filename) || filename == "" || !nzchar(filename)) {
    sprintf("data_%s", format(timestamp, "%Y%m%d_%H%M%S"))
  } else {
    # Remove extension if provided
    tools::file_path_sans_ext(filename)
  }
}


#' Build expression to write data to CSV file(s)
#'
#' @param data_names Character vector of data object names to write
#' @param path Character. Full file path for output
#' @param args List of write parameters (sep, quote, na, etc.)
#'
#' @return A language object (expression) that writes CSV file(s)
#' @keywords internal
write_expr_csv <- function(data_names, path, args = list(), as_sym = as_bare_sym) {
  # Extract CSV-specific params with defaults
  sep <- if (is.null(args$sep)) "," else args$sep
  quote <- if (is.null(args$quote)) {
    "needed"
  } else {
    # Convert boolean to readr format
    if (isTRUE(args$quote)) "all" else if (isFALSE(args$quote)) "none" else args$quote
  }
  na <- if (is.null(args$na)) "NA" else args$na

  # Single file case
  if (length(data_names) == 1) {
    data_sym <- as_sym(data_names[1])

    # Use write_csv for comma, write_delim for other separators
    if (sep == ",") {
      return(bquote(readr::write_csv(
        x = .(data_sym),
        file = .(path),
        quote = .(quote),
        na = .(na)
      )))
    } else {
      return(bquote(readr::write_delim(
        x = .(data_sym),
        file = .(path),
        delim = .(sep),
        quote = .(quote),
        na = .(na)
      )))
    }
  }

  # Multiple files - write each to temp, then zip
  # Build list of write expressions
  write_calls <- lapply(seq_along(data_names), function(i) {
    data_sym <- as_sym(data_names[i])
    filename <- paste0(names(data_names)[i], ".csv")

    # Use write_csv for comma, write_delim for other separators
    if (sep == ",") {
      bquote(readr::write_csv(
        x = .(data_sym),
        file = file.path(temp_dir, .(filename)),
        quote = .(quote),
        na = .(na)
      ))
    } else {
      bquote(readr::write_delim(
        x = .(data_sym),
        file = file.path(temp_dir, .(filename)),
        delim = .(sep),
        quote = .(quote),
        na = .(na)
      ))
    }
  })

  # Build complete expression with temp dir creation and zipping
  # Use as.call to build the expression block
  full_expr <- as.call(c(
    quote(`{`),
    quote(temp_dir <- tempfile("blockr_write_")),
    quote(dir.create(temp_dir, showWarnings = FALSE)),
    write_calls,
    quote(files_to_zip <- list.files(temp_dir, full.names = TRUE)),
    bquote(zip::zip(
      zipfile = .(path),
      files = files_to_zip,
      mode = "cherry-pick",
      root = temp_dir
    )),
    quote(unlink(temp_dir, recursive = TRUE))
  ))

  full_expr
}


#' Build expression to write data to Excel file with multiple sheets
#'
#' @param data_names Named character vector where names are sheet names
#' @param path Character. Full file path for output
#'
#' @return A language object (expression) that writes Excel file
#' @keywords internal
write_expr_excel <- function(data_names, path, as_sym = as_bare_sym) {
  # Build named list for writexl
  # names(data_names) are sheet names, values are data object names
  data_list_expr <- lapply(seq_along(data_names), function(i) {
    as_sym(data_names[i])
  })
  names(data_list_expr) <- names(data_names)

  bquote(writexl::write_xlsx(
    x = .(as.call(c(quote(list), data_list_expr))),
    path = .(path)
  ))
}


#' Build expression to write data to Arrow format (Parquet or Feather)
#'
#' @param data_names Character vector of data object names to write
#' @param path Character. Full file path for output
#' @param format Character. Either "parquet" or "feather"
#'
#' @return A language object (expression) that writes Arrow file(s)
#' @keywords internal
write_expr_arrow <- function(data_names, path, format = "parquet", as_sym = as_bare_sym) {
  write_func <- if (format == "feather") {
    quote(arrow::write_feather)
  } else {
    quote(arrow::write_parquet)
  }

  # Single file case
  if (length(data_names) == 1) {
    data_sym <- as_sym(data_names[1])
    return(bquote(.(write_func)(
      x = .(data_sym),
      sink = .(path)
    )))
  }

  # Multiple files - write each to temp, then zip
  ext <- if (format == "feather") ".feather" else ".parquet"
  write_calls <- lapply(seq_along(data_names), function(i) {
    data_sym <- as_sym(data_names[i])
    filename <- paste0(names(data_names)[i], ext)
    bquote(.(write_func)(
      x = .(data_sym),
      sink = file.path(temp_dir, .(filename))
    ))
  })

  # Build complete expression
  full_expr <- as.call(c(
    quote(`{`),
    quote(temp_dir <- tempfile("blockr_write_")),
    quote(dir.create(temp_dir, showWarnings = FALSE)),
    write_calls,
    quote(files_to_zip <- list.files(temp_dir, full.names = TRUE)),
    bquote(zip::zip(
      zipfile = .(path),
      files = files_to_zip,
      mode = "cherry-pick",
      root = temp_dir
    )),
    quote(unlink(temp_dir, recursive = TRUE))
  ))

  full_expr
}


#' Create a write expression for a single file
#'
#' Convenience wrapper that detects the file format from the path extension and
#' returns an unevaluated R expression for writing the data. Useful for
#' sibling packages that construct pipelines programmatically (e.g. DM blocks).
#'
#' @param data Character. Name of the data object to write.
#' @param path Character. Output file path (extension determines format).
#' @param ... Additional parameters forwarded to the writer (e.g. `sep`,
#'   `quote`, `na` for CSV files).
#' @return A language object (unevaluated call) that, when evaluated, writes
#'   the data frame to the file.
#'
#' @examples
#' write_file_expr("mtcars", "/tmp/cars.csv")
#' write_file_expr("iris", "/tmp/flowers.xlsx")
#' write_file_expr("df", "/tmp/data.parquet")
#'
#' @export
write_file_expr <- function(data, path, ...) {
  stopifnot(is_string(data), nzchar(data), is_string(path), nzchar(path))

  category <- file_category(path)
  data_names <- stats::setNames(data, data)

  args <- list(...)

  switch(category,
    csv = write_expr_csv(data_names, path, args),
    excel = write_expr_excel(data_names, path),
    arrow = {
      ext <- tolower(tools::file_ext(path))
      fmt <- if (ext == "feather") "feather" else "parquet"
      write_expr_arrow(data_names, path, format = fmt)
    },
    stop(sprintf("Unsupported file format: '%s'", tools::file_ext(path)))
  )
}

# Internal function - not exported
write_expr <- function(
  data_names,
  directory,
  filename = "",
  format = unname(write_formats()),
  args = list(),
  as_sym = as_bare_sym
) {
  format <- match.arg(format)

  # Handle empty data_names
  if (length(data_names) == 0) {
    return(NULL)
  }

  # Ensure every entry has a display name. Positional (unnamed) variadic
  # slots arrive as "" (or NULL when all are unnamed): left as-is they
  # produce colliding file names ("" -> ".csv", each write clobbering the
  # last) and empty Excel sheet names. Fill blanks with a positional
  # default instead.
  nms <- names(data_names)
  if (is.null(nms)) {
    nms <- character(length(data_names))
  }
  blank <- !nzchar(nms)
  nms[blank] <- paste0("data_", which(blank))
  names(data_names) <- make.unique(nms, sep = "_")

  # Generate base filename
  base_filename <- generate_filename(filename)

  # Determine file extension and whether we need ZIP
  needs_zip <- length(data_names) > 1 && format != "excel"
  ext <- format_extension(format, needs_zip = needs_zip)

  # Build full path
  full_filename <- paste0(base_filename, ext)
  full_path <- file.path(directory, full_filename)

  # Dispatch to appropriate handler
  write_call <- if (format == "csv") {
    write_expr_csv(data_names, full_path, args, as_sym = as_sym)
  } else if (format == "excel") {
    write_expr_excel(data_names, full_path, as_sym = as_sym)
  } else {
    write_expr_arrow(data_names, full_path, format, as_sym = as_sym)
  }

  # The target directory is created here, at write time, and nowhere else —
  # so browsing/typing a path never leaves partial directories behind, and
  # the exported expression is self-contained.
  bquote({
    if (!dir.exists(.(directory))) {
      dir.create(.(directory), recursive = TRUE)
    }
    .(write_call)
  })
}
