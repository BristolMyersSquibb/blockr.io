#' Create a read expression for a single file
#'
#' Convenience wrapper that detects the file category from the path and
#' returns an unevaluated R expression for reading the file. Useful for
#' sibling packages that construct pipelines programmatically.
#'
#' @param path Character. Path to a single file.
#' @param ... Additional parameters forwarded to the reader (e.g. `sep`,
#'   `sheet`, `skip`).
#' @return A language object (unevaluated call) that, when evaluated, reads the
#'   file into a data frame.
#' @export
read_file_expr <- function(path, ...) {
  stopifnot(is_string(path), nzchar(path))
  file_type <- file_category(path)
  read_expr_single(path, file_type, ...)
}

# Internal function - not exported
read_expr <- function(
  paths,
  file_type = c("csv", "excel", "arrow", "other"),
  combine = c("first", "rbind", "cbind", "auto"),
  ...
) {
  # Handle empty paths first (no file selected yet)
  if (length(paths) == 0) {
    return(NULL)
  }

  file_type <- match.arg(file_type)
  combine <- match.arg(combine)

  # Single file case - simple
  if (length(paths) == 1) {
    return(read_expr_single(paths[1], file_type, ...))
  }

  # Multi-file case
  if (combine == "first") {
    # Just use first file
    return(read_expr_single(paths[1], file_type, ...))
  }

  # Build expression for each file
  file_exprs <- lapply(paths, function(p) {
    read_expr_single(p, file_type, ...)
  })

  # Combine based on strategy
  if (combine == "rbind" || combine == "auto") {
    combine_expr <- as.call(c(quote(rbind), file_exprs))

    if (combine == "auto") {
      # Wrap in tryCatch to fallback to first file if rbind fails
      bquote(
        tryCatch(
          .(combine_expr),
          error = function(e) .(file_exprs[[1]])
        )
      )
    } else {
      combine_expr
    }
  } else if (combine == "cbind") {
    as.call(c(quote(cbind), file_exprs))
  }
}


#' Create expression for a single file
#'
#' @param path Character. Single file path
#' @param file_type Character. Type of file
#' @param ... Parameters for the reader function
#'
#' @return A language object (expression)
#' @keywords internal
read_expr_single <- function(path, file_type, ...) {
  if (file_type == "csv")   return(read_expr_csv(path, ...))
  if (file_type == "excel") return(read_expr_excel(path, ...))
  if (file_type == "arrow") return(read_expr_arrow(path, ...))
  read_expr_rio(path, ...)
}


#' Create CSV/TSV/delimited file reading expression
#'
#' @param path Character. File path
#' @param ... Reading parameters: sep, col_names, skip, n_max, quote, encoding
#'
#' @return Expression calling readr::read_csv, readr::read_tsv, or readr::read_delim
#' @keywords internal
read_expr_csv <- function(path, ...) {
  # Extract CSV-specific params with defaults
  params <- list(...)
  sep <- if (is.null(params$sep)) "," else params$sep
  col_names <- if (is.null(params$col_names)) TRUE else params$col_names
  skip <- if (is.null(params$skip)) 0 else params$skip
  n_max <- if (is.null(params$n_max)) Inf else params$n_max
  quote <- if (is.null(params$quote)) "\"" else params$quote
  encoding <- if (is.null(params$encoding)) "UTF-8" else params$encoding

  # Remove names from path vector to avoid potential issues
  path <- unname(path)

  # Choose appropriate readr function based on delimiter
  if (sep == ",") {
    bquote(readr::read_csv(
      file = .(path),
      col_names = .(col_names),
      skip = .(skip),
      n_max = .(n_max),
      quote = .(quote),
      locale = readr::locale(encoding = .(encoding)),
      show_col_types = FALSE
    ))
  } else if (sep == "\t") {
    bquote(readr::read_tsv(
      file = .(path),
      col_names = .(col_names),
      skip = .(skip),
      n_max = .(n_max),
      quote = .(quote),
      locale = readr::locale(encoding = .(encoding)),
      show_col_types = FALSE
    ))
  } else {
    bquote(readr::read_delim(
      file = .(path),
      delim = .(sep),
      col_names = .(col_names),
      skip = .(skip),
      n_max = .(n_max),
      quote = .(quote),
      locale = readr::locale(encoding = .(encoding)),
      show_col_types = FALSE
    ))
  }
}


#' Create Excel file reading expression
#'
#' @param path Character. File path
#' @param ... Reading parameters: sheet, range, col_names, skip, n_max
#'
#' @return Expression calling readxl::read_excel
#' @keywords internal
read_expr_excel <- function(path, ...) {
  params <- list(...)
  sheet <- params$sheet
  range <- params$range
  col_names <- if (is.null(params$col_names)) TRUE else params$col_names
  skip <- if (is.null(params$skip)) 0 else params$skip
  n_max <- if (is.null(params$n_max)) Inf else params$n_max

  path <- unname(path)

  bquote(readxl::read_excel(
    path = .(path),
    sheet = .(sheet),
    range = .(range),
    col_names = .(col_names),
    skip = .(skip),
    n_max = .(n_max)
  ))
}


#' Create Arrow file reading expression
#'
#' @param path Character. File path
#' @param ... Reading parameters (currently unused)
#'
#' @return Expression calling arrow::read_parquet (preferred) or
#'   nanoparquet::read_parquet for parquet, and arrow::read_feather /
#'   arrow::read_ipc_file for the Arrow-only feather and IPC formats
#' @keywords internal
read_expr_arrow <- function(path, ...) {
  ext <- tolower(tools::file_ext(path))

  path <- unname(path)

  # Parquet: prefer arrow when installed — it restores column/table label
  # attributes from the "r" key-value metadata, which nanoparquet ignores.
  # nanoparquet (tiny, zero-dependency) is the fallback for environments
  # without arrow. Feather and Arrow IPC are arrow-only formats.
  if (ext == "parquet") {
    read_parquet_expr(path)
  } else if (ext == "feather") {
    bquote(arrow::read_feather(.(path)))
  } else if (ext == "arrow") {
    bquote(arrow::read_ipc_file(.(path)))
  } else {
    # Default to parquet if extension unclear
    read_parquet_expr(path)
  }
}

# Pick the parquet reader expression based on what is installed: prefer
# arrow (preserves label attributes stored in parquet metadata), fall back
# to nanoparquet (lightweight, zero-dependency — but drops labels).
read_parquet_expr <- function(path) {
  if (requireNamespace("arrow", quietly = TRUE)) {
    bquote(arrow::read_parquet(.(path)))
  } else {
    bquote(nanoparquet::read_parquet(.(path)))
  }
}


#' Create rio import expression
#'
#' @param path Character. File path
#' @param ... Reading parameters (currently unused)
#'
#' @return Expression calling rio::import
#' @keywords internal
read_expr_rio <- function(path, ...) {
  path <- unname(path)
  bquote(rio::import(file = .(path)))
}
