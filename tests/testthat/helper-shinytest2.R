# Helper functions for shinytest2 integration tests

#' Create a temporary Shiny app for testing blocks
#'
#' @param block_code R code to create the block(s) as a character string
#' @param data_code R code to create test data/files as a character string (optional)
#' @return Path to the temporary app directory
create_test_app <- function(
  block_code,
  data_code = NULL
) {
  app_dir <- tempfile("blockr_test_")
  dir.create(app_dir)

  # Get the package root directory by finding DESCRIPTION file
  find_pkg_root <- function() {
    # Try testthat::test_path() first (available during test execution)
    if (requireNamespace("testthat", quietly = TRUE)) {
      tryCatch(
        {
          test_dir <- testthat::test_path()
          # Go up from tests/testthat to package root
          pkg_root <- normalizePath(file.path(test_dir, "..", ".."))
          if (file.exists(file.path(pkg_root, "DESCRIPTION"))) {
            return(pkg_root)
          }
        },
        error = function(e) {}
      )
    }

    # Fallback: search upward from current directory
    search_dir <- getwd()
    for (i in 1:5) {
      if (file.exists(file.path(search_dir, "DESCRIPTION"))) {
        return(normalizePath(search_dir))
      }
      search_dir <- dirname(search_dir)
    }

    return(getwd())
  }

  pkg_root <- find_pkg_root()

  # Build the app content
  app_lines <- c(
    "library(blockr.core)",
    "",
    "# Load blockr.io from development if available",
    sprintf("pkg_path <- '%s'", pkg_root),
    "if (requireNamespace('devtools', quietly = TRUE) && dir.exists(pkg_path) && file.exists(file.path(pkg_path, 'DESCRIPTION'))) {",
    "  tryCatch(",
    "    devtools::load_all(pkg_path, quiet = TRUE),",
    "    error = function(e) library(blockr.io)",
    "  )",
    "} else {",
    "  library(blockr.io)",
    "}",
    ""
  )

  # Add custom data/setup code if provided
  if (!is.null(data_code)) {
    app_lines <- c(app_lines, data_code, "")
  }

  # Add block code and serve
  app_lines <- c(
    app_lines,
    block_code,
    ""
  )

  app_content <- paste(app_lines, collapse = "\n")
  writeLines(app_content, file.path(app_dir, "app.R"))

  return(app_dir)
}

#' Create test data files in a directory
#'
#' @param dir Directory to create files in
#' @return Named list of file paths
create_test_files <- function(dir = tempdir()) {
  files <- list()

  # CSV file
  csv_file <- file.path(dir, "test_data.csv")
  write.csv(
    data.frame(x = 1:10, y = letters[1:10], z = 11:20),
    csv_file,
    row.names = FALSE
  )
  files$csv <- csv_file

  # TSV file
  tsv_file <- file.path(dir, "test_data.tsv")
  write.table(
    data.frame(a = 1:5, b = 6:10),
    tsv_file,
    sep = "\t",
    row.names = FALSE,
    quote = FALSE
  )
  files$tsv <- tsv_file

  # Multiple CSV files for combining
  csv1 <- file.path(dir, "data1.csv")
  csv2 <- file.path(dir, "data2.csv")
  write.csv(data.frame(id = 1:3, value = c(10, 20, 30)), csv1, row.names = FALSE)
  write.csv(data.frame(id = 4:6, value = c(40, 50, 60)), csv2, row.names = FALSE)
  files$csv_multi <- c(csv1, csv2)

  # Excel file (if writexl available)
  if (requireNamespace("writexl", quietly = TRUE)) {
    xlsx_file <- file.path(dir, "test_data.xlsx")
    writexl::write_xlsx(
      list(
        Sheet1 = data.frame(col1 = 1:5, col2 = 6:10),
        Sheet2 = data.frame(name = c("A", "B", "C"), score = c(95, 87, 92))
      ),
      xlsx_file
    )
    files$excel <- xlsx_file
  }

  # Parquet file (if arrow available)
  if (requireNamespace("arrow", quietly = TRUE)) {
    parquet_file <- file.path(dir, "test_data.parquet")
    arrow::write_parquet(
      data.frame(x = 1:100, y = rnorm(100)),
      parquet_file
    )
    files$parquet <- parquet_file
  }

  return(files)
}

#' Clean up test app directory
#'
#' @param app_dir Path to the app directory
#' @param app Optional AppDriver object to stop first
cleanup_test_app <- function(app_dir, app = NULL) {
  if (!is.null(app)) {
    tryCatch(
      app$stop(),
      error = function(e) {
        warning("Failed to stop app: ", e$message)
      }
    )
  }

  if (dir.exists(app_dir)) {
    unlink(app_dir, recursive = TRUE)
  }
}

#' Wait for block to be ready and return result data
#'
#' @param app AppDriver object
#' @param timeout Timeout in milliseconds
#' @param block_name Name of the block (default: "data")
#' @return The result data from the block
get_block_result <- function(app, timeout = 30000, block_name = "data") {
  app$wait_for_idle(timeout = timeout)

  values <- app$get_values(export = TRUE)

  # Standard serve() export - try this first
  if (!is.null(values$export$result)) {
    return(values$export$result)
  }

  # For DAG boards, result might be at export$<block_name>
  if (!is.null(values$export[[block_name]])) {
    return(values$export[[block_name]])
  }

  # For DAG boards with blockr.ui, result is at export$"main-res"
  if (!is.null(values$export[["main-res"]])) {
    return(values$export[["main-res"]])
  }

  # Debug: print available export keys
  available_keys <- names(values$export)
  stop(sprintf(
    "Could not find result in exported values. Available keys: %s",
    paste(available_keys, collapse = ", ")
  ))
}
