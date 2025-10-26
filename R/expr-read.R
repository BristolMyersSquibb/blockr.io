#' Build expression to read file(s)
#'
#' Creates an R expression that reads one or more files using standard R packages
#' (readr, readxl, arrow, rio). This is a pure function with no Shiny dependencies,
#' making it easily testable.
#'
#' @param paths Character vector of file paths to read
#' @param file_type Character. Type of file: "csv", "excel", "arrow", or "other"
#' @param combine Character. For multiple files: "first" (use first only),
#'   "rbind" (row bind), "cbind" (column bind), or "auto" (try rbind, fallback to first)
#' @param ... Additional parameters passed to the reader function. Common parameters:
#'   - For CSV: sep, col_names, skip, n_max, quote, encoding
#'   - For Excel: sheet, range, col_names, skip, n_max
#'
#' @return A language object (expression) that can be evaluated to read the file(s)
#'
#' @examples
#' \dontrun{
#' # Single CSV file
#' expr <- read_expr("data.csv", "csv", sep = ",", col_names = TRUE)
#' data <- eval(expr)
#'
#' # Multiple CSV files with rbind
#' expr <- read_expr(
#'   c("data1.csv", "data2.csv"),
#'   "csv",
#'   combine = "rbind",
#'   sep = ","
#' )
#' data <- eval(expr)
#' }
#'
#' @keywords internal
#' @export
read_expr <- function(paths,
                            file_type = c("csv", "excel", "arrow", "other"),
                            combine = c("first", "rbind", "cbind", "auto"),
                            ...) {

  file_type <- match.arg(file_type)
  combine <- match.arg(combine)

  # Handle empty paths
  if (length(paths) == 0) {
    return(NULL)
  }

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
  # Dispatch to appropriate builder
  if (file_type == "csv") {
    read_expr_csv(path, ...)
  } else if (file_type == "excel") {
    read_expr_excel(path, ...)
  } else if (file_type == "arrow") {
    read_expr_arrow(path, ...)
  } else {
    read_expr_rio(path, ...)
  }
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
#' @return Expression calling arrow::read_parquet, arrow::read_feather, or arrow::read_ipc_file
#' @keywords internal
read_expr_arrow <- function(path, ...) {
  # Detect arrow format from extension
  ext <- tolower(tools::file_ext(path))

  if (ext == "parquet") {
    bquote(arrow::read_parquet(.(path)))
  } else if (ext == "feather") {
    bquote(arrow::read_feather(.(path)))
  } else if (ext == "arrow") {
    bquote(arrow::read_ipc_file(.(path)))
  } else {
    # Default to parquet if extension unclear
    bquote(arrow::read_parquet(.(path)))
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
  bquote(rio::import(file = .(path)))
}

