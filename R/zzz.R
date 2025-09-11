register_io_blocks <- function() {
  # nocov start
  register_blocks(
    c(
      "new_read_block",
      "new_readmulti_block",
      "new_pick_block",
      "new_readxlsx_block",
      "new_writexlsx_block",
      "new_readcsv_block",
      "new_writecsv_block",
      "new_readxpt_block",
      "new_writexpt_block"
    ),
    name = c(
      "Read data (unified)",
      "Read multiple files",
      "Pick from list",
      "Read Excel data",
      "Write Excel data",
      "Read csv data",
      "Write csv data",
      "Read xpt data",
      "Write xpt data"
    ),
    description = c(
      "Read data from various file formats with auto-detection",
      "Read and combine multiple files at once",
      "Pick a single item from a list",
      "Read tabular data from an Excel file",
      "Write tabular data to an Excel file",
      "Read tabular data from a csv file",
      "Write tabular data to a csv file",
      "Read tabular data from an xpt file",
      "Write tabular data to an xpt file"
    ),
    category = c(
      "data",
      "data",
      "transform",
      "data",
      "transform",
      "data",
      "transform",
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
