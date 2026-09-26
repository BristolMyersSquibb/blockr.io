#' Gear button and settings tray
#'
#' The gear and the tray it opens, as blockr.io's blocks draw them. A thin
#' wrapper over [blockr.ui::gear_tray()], kept for packages that built their
#' settings on this function: the tray opens in flow under the gear, slides,
#' and closes on the gear or Escape. `input[[gear_id]]` is `TRUE` while it
#' is open.
#'
#' Each item in `...` takes a whole row of the tray's field grid. An item
#' that is itself a field of the grid (a `.blockr-settings__field`, such as
#' the controls of [blockr.ui::select_input()] and its siblings) is placed
#' as it is.
#'
#' @param gear_id The gear's id (namespace it with [shiny::NS()]). The
#'   tray's id is `<gear_id>_tray`.
#' @param band_id Ignored; the tray takes its id from `gear_id`. Kept so
#'   existing calls keep working.
#' @param ... Tray content.
#' @param band_label Accessible label for the tray.
#' @return A [htmltools::tagList()] with the gear row and the tray; place
#'   them together at the top of the block.
#' @export
gear_band_ui <- function(gear_id, band_id = NULL, ..., band_label = "Settings") {

  items <- Filter(Negate(is.null), list(...))

  fields <- lapply(items, function(x) {
    if (is_tray_field(x)) {
      return(x)
    }
    htmltools::div(
      class = "blockr-settings__field blockr-settings__field--full",
      x
    )
  })

  htmltools::tagList(
    io_block_deps(),
    do.call(
      blockr.ui::gear_tray,
      c(list(gear_id), fields, list(label = band_label))
    )
  )
}

is_tray_field <- function(x) {
  inherits(x, "shiny.tag") &&
    htmltools::tagHasAttribute(x, "class") &&
    grepl(
      "blockr-settings__field", htmltools::tagGetAttribute(x, "class"),
      fixed = TRUE
    )
}
