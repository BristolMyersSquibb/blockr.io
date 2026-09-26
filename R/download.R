#' Download block
#'
#' The write block ([new_write_block()]) with server saving off: at rest a
#' single "Download CSV" button (named after the chosen format), and the
#' format, the filename and the format's options in the gear. Server saving
#' can be switched on in the gear like in any write block.
#'
#' Kept as its own constructor and class for the boards and scripts that
#' use it: a board saved with the earlier download block restores into this
#' one with its filename, format and options.
#'
#' @param filename Character. Optional fixed filename (without extension).
#'   If empty (default), a timestamped filename is generated.
#' @param format Character. One of the values in [`write_formats()`]:
#'   `"csv"`, `"excel"`, `"parquet"`, or `"feather"`. Default: `"csv"`.
#' @param args Named list of format-specific writing parameters (same as
#'   [`new_write_block()`]). Only relevant values for the selected format are
#'   used.
#' @param directory,auto_write Server saving, off by default: see
#'   [`new_write_block()`]. They are the block's state once server saving is
#'   switched on in the gear, so a saved board restores them.
#' @param ... Forwarded to [`blockr.core::new_transform_block()`].
#'
#' @details
#' Multi-input behavior matches [`new_write_block()`]: multiple inputs produce
#' a multi-sheet Excel file for `format = "excel"`, or a ZIP archive for CSV,
#' Parquet, and Feather.
#'
#' @return A block of class `download_block` that works as a write block.
#'
#' @examples
#' new_download_block(args = list(sep = ",", quote = TRUE, na = ""))
#'
#' if (interactive()) {
#'   library(blockr.core)
#'   serve(new_download_block())
#' }
#'
#' @rdname download
#' @export
new_download_block <- function(
  filename = "",
  format = "csv",
  args = list(),
  directory = "",
  auto_write = FALSE,
  ...
) {
  write_block_impl(
    directory = directory,
    filename = filename,
    format = format,
    auto_write = auto_write,
    args = args,
    class = "download_block",
    default_ctor = "new_download_block",
    ...
  )
}
