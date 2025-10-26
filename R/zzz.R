register_io_blocks <- function() {
  # nocov start
  register_blocks(
    "new_read_block",
    name = "Read data",
    description = "Read data from various file formats (CSV, Excel, SPSS, SAS, Stata, Parquet, JSON, etc.) with automatic format detection and smart adaptive UI",
    category = "data",
    package = utils::packageName(),
    overwrite = TRUE
  )
}

.onLoad <- function(libname, pkgname) {
  register_io_blocks()

  invisible(NULL)
} # nocov end
