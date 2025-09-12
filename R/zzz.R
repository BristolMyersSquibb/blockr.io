register_io_blocks <- function() {
  # nocov start
  register_blocks(
    c(
      "new_readxlsx_block",
      "new_writexlsx_block",
      "new_readcsv_block",
      "new_writecsv_block",
      "new_readxpt_block",
      "new_writexpt_block",
      "new_readmulti_block",
      "new_pick_block"
    ),
    name = c(
      "Read Excel data",
      "Write Excel data",
      "Read csv data",
      "Write csv data",
      "Read xpt data",
      "Write xpt data",
      "Read multiple datasets",
      "Pick a dataset"
    ),
    description = c(
      "Read tabular data from an Excel file",
      "Write tabular data to an Excel file",
      "Read tabular data from a csv file",
      "Write tabular data to a csv file",
      "Read tabular data from an xpt file",
      "Write tabular data to an xpt file",
      "Select multiple files at once and parse using rio",
      "Pick a single dataset form a list of datasets"
    ),
    category = c(
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
