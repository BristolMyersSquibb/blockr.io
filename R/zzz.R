register_io_blocks <- function() { # nocov start
  register_blocks(
    c(
      "new_readxlsx_block",
      "new_writexlsx_block"
    ),
    name = c(
      "Read Excel data",
      "Write Excel data"
    ),
    description = c(
      "Read tabular data from an Excel file",
      "Write tabular data to an Excel file"
    ),
    category = c(
      "data",
      "transform"
    ),
    package = utils::packageName(),
    overwrite = TRUE
  )
}

.onLoad <- function(libname, pkgname) {

  register_io_blocks()

  invisible(NULL)
} # nocov end
