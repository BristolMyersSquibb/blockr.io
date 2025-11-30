register_io_blocks <- function() {
  # nocov start
  register_blocks(
    "new_read_block",
    name = "Import Data",
    description = "Read and load data from files on your computer, server, or web. Supports CSV, Excel, SPSS, SAS, Stata, Parquet, and more.",
    category = "input",
    icon = "file-earmark-arrow-up",
    package = utils::packageName(),
    overwrite = TRUE
  )

  register_blocks(
    "new_write_block",
    name = "Export Data",
    description = "Write and save your data to CSV, Excel, Parquet, or Feather files. Download directly or save to server.",
    category = "output",
    icon = "save",
    package = utils::packageName(),
    overwrite = TRUE
  )
}

.onLoad <- function(libname, pkgname) {
  register_io_blocks()

  invisible(NULL)
} # nocov end
