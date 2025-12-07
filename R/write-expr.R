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
write_expr_csv <- function(data_names, path, args = list()) {
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
    data_sym <- as.name(data_names[1])

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
    data_sym <- as.name(data_names[i])
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
write_expr_excel <- function(data_names, path) {
  # Build named list for writexl
  # names(data_names) are sheet names, values are data object names
  data_list_expr <- lapply(seq_along(data_names), function(i) {
    as.name(data_names[i])
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
write_expr_arrow <- function(data_names, path, format = "parquet") {
  write_func <- if (format == "feather") {
    quote(arrow::write_feather)
  } else {
    quote(arrow::write_parquet)
  }

  # Single file case
  if (length(data_names) == 1) {
    data_sym <- as.name(data_names[1])
    return(bquote(.(write_func)(
      x = .(data_sym),
      sink = .(path)
    )))
  }

  # Multiple files - write each to temp, then zip
  ext <- if (format == "feather") ".feather" else ".parquet"
  write_calls <- lapply(seq_along(data_names), function(i) {
    data_sym <- as.name(data_names[i])
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


# Internal function - not exported
write_expr <- function(
  data_names,
  directory,
  filename = "",
  format = c("csv", "excel", "parquet", "feather"),
  args = list()
) {
  format <- match.arg(format)

  # Handle empty data_names
  if (length(data_names) == 0) {
    return(NULL)
  }

  # Ensure data_names has names
  if (is.null(names(data_names))) {
    names(data_names) <- as.character(seq_along(data_names))
  }

  # Generate base filename
  base_filename <- generate_filename(filename)

  # Determine file extension and whether we need ZIP
  needs_zip <- length(data_names) > 1 && format != "excel"

  if (needs_zip) {
    ext <- ".zip"
  } else {
    ext <- switch(
      format,
      csv = ".csv",
      excel = ".xlsx",
      parquet = ".parquet",
      feather = ".feather"
    )
  }

  # Build full path
  full_filename <- paste0(base_filename, ext)
  full_path <- file.path(directory, full_filename)

  # Dispatch to appropriate handler
  if (format == "csv") {
    write_expr_csv(data_names, full_path, args)
  } else if (format == "excel") {
    write_expr_excel(data_names, full_path)
  } else {
    write_expr_arrow(data_names, full_path, format)
  }
}
