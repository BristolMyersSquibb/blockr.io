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
  ...,
  .emit = NULL
) {
  # What the expression NAMES, when that is not what was read. A URL is
  # downloaded to a temp file before it is read, and the temp path is the
  # right thing to detect the format from and the wrong thing to write into
  # exported code. `.emit` carries the locations to embed instead; dispatch
  # still runs off `paths`. NULL means "the same", which is every caller
  # that is not the read block reading a URL.
  if (is.null(.emit)) {
    .emit <- paths
  }
  stopifnot(length(.emit) == length(paths))
  # Handle empty paths first (no file selected yet)
  if (length(paths) == 0) {
    return(NULL)
  }

  file_type <- match.arg(file_type)
  combine <- match.arg(combine)

  # Single file case - simple
  if (length(paths) == 1) {
    return(read_expr_single(paths[1], file_type, ..., .emit = .emit[1]))
  }

  # Multi-file case
  if (combine == "first") {
    # Just use first file
    return(read_expr_single(paths[1], file_type, ..., .emit = .emit[1]))
  }

  # Build expression for each file
  file_exprs <- lapply(seq_along(paths), function(i) {
    read_expr_single(paths[i], file_type, ..., .emit = .emit[i])
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
#' Dispatches through the format registry by extension. `file_type` is kept
#' for callers but no longer decides the reader: the registry's named entries
#' cover the same mapping `file_category()` used to hardcode, and per-path
#' dispatch means a mixed multi-file read no longer sends every file to the
#' first file's reader.
#'
#' @param path Character. Single file path
#' @param file_type Character. Type of file (unused, kept for callers)
#' @param ... Parameters for the reader function
#'
#' @return A language object (expression)
#' @keywords internal
read_expr_single <- function(path, file_type, ..., .emit = NULL) {
  # Extension off `path` (what is on disk), literal from `.emit` (what the
  # code should say). They differ only for a URL that was downloaded first.
  format_read_expr_impl(
    tolower(tools::file_ext(path)), .emit %||% path, ...
  )
}


# Is `v` the value the reader would have used anyway?
#
# Numeric comparison rather than `identical()` on purpose: a settings field
# hands back `0` where the default is `0L` (and `Inf` where the default is
# `Inf`), and those are the same instruction to the reader even though they
# are not the same R object.
is_reader_default <- function(v, d) {
  if (is.null(v) || is.null(d)) {
    return(is.null(v) && is.null(d))
  }
  if (is.numeric(v) && is.numeric(d)) {
    return(length(v) == 1L && length(d) == 1L && isTRUE(v == d))
  }
  identical(v, d)
}

# Drop the arguments left at their default, and any that are NULL.
#
# WHY THIS EXISTS. These expressions are not just what the block runs, they
# are what it EXPORTS -- into a saved script, into a report's chunk, into
# anything built off the block's code. A call that restates every default
# reads as machine output:
#
#   readr::read_csv(file = "x.csv", col_names = TRUE, skip = 0, n_max = Inf,
#       quote = "\"", locale = readr::locale(encoding = "UTF-8"),
#       show_col_types = FALSE)
#
# where what the reader would have typed, and what carries the same meaning,
# is `readr::read_csv("x.csv", show_col_types = FALSE)`. Set one of them to
# something else and it appears; that is the point of showing it.
prune_reader_args <- function(args, defaults) {
  keep <- vapply(
    names(args),
    function(nm) {
      v <- args[[nm]]
      !is.null(v) && !(nm %in% names(defaults) &&
                         is_reader_default(v, defaults[[nm]]))
    },
    logical(1)
  )
  args[keep]
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
  quote_char <- if (is.null(params$quote)) "\"" else params$quote
  encoding <- if (is.null(params$encoding)) "UTF-8" else params$encoding

  # Remove names from path vector to avoid potential issues
  path <- unname(path)

  # The delimiter picks the function, so it is only ever an ARGUMENT for
  # read_delim -- where it is required, and so never pruned.
  fn <- if (sep == ",") {
    quote(readr::read_csv)
  } else if (sep == "\t") {
    quote(readr::read_tsv)
  } else {
    quote(readr::read_delim)
  }

  args <- list(
    col_names = col_names,
    skip = skip,
    n_max = n_max,
    quote = quote_char,
    # readr's own default locale is UTF-8, so naming it adds a call that
    # does nothing. A different encoding is a real instruction and stays.
    locale = if (!is_reader_default(encoding, "UTF-8")) {
      bquote(readr::locale(encoding = .(encoding)))
    }
  )
  if (identical(fn, quote(readr::read_delim))) {
    args <- c(list(delim = sep), args)
  }

  args <- prune_reader_args(
    args,
    list(col_names = TRUE, skip = 0, n_max = Inf, quote = "\"")
  )

  # `show_col_types = FALSE` is NOT a default and stays: readr prints a
  # column spec on every read, which is noise in a block panel and noise in
  # a rendered document.
  as.call(c(fn, list(path), args, list(show_col_types = FALSE)))
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

  path <- unname(path)

  # Same pruning as the csv builder, and the same reason: an unconfigured
  # read should export as `readxl::read_excel("book.xlsx")`.
  args <- prune_reader_args(
    list(
      sheet = params$sheet,
      range = params$range,
      col_names = if (is.null(params$col_names)) TRUE else params$col_names,
      skip = if (is.null(params$skip)) 0 else params$skip,
      n_max = if (is.null(params$n_max)) Inf else params$n_max
    ),
    list(sheet = NULL, range = NULL, col_names = TRUE, skip = 0, n_max = Inf)
  )

  as.call(c(quote(readxl::read_excel), list(path), args))
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
