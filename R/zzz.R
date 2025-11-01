register_io_blocks <- function() {
  # nocov start
  register_blocks(
    "new_read_block",
    name = "Read data",
    description = "Read data from various file formats (CSV, Excel, SPSS, SAS, Stata, Parquet, JSON, etc.) with automatic format detection and smart adaptive UI",
    category = "data",
    icon = "file-earmark-arrow-up",
    package = utils::packageName(),
    overwrite = TRUE
  )

  register_blocks(
    "new_write_block",
    name = "Write data",
    description = "Write dataframes to various file formats (CSV, Excel, Parquet, Feather). Supports single or multiple inputs with download or filesystem modes",
    category = "data",
    icon = "floppy",
    package = utils::packageName(),
    overwrite = TRUE
  )
}

.onLoad <- function(libname, pkgname) {
  register_io_blocks()

  invisible(NULL)
} # nocov end
