#' Gear button and settings band
#'
#' The gear-and-band affordance blockr.io's blocks use for advanced
#' settings: a small gear button, and an in-flow panel that opens beneath it
#' (class-driven via `blockrIoGearToggle()`, so it pushes content down
#' rather than overlaying it). Exported so sibling packages can mount the
#' same affordance with the same styling; the required JS and CSS ride along
#' as an html dependency.
#'
#' Lay fields out with `blockr-settings__grid` / `blockr-settings__field`
#' divs and `blockr-label` labels, matching this package's own bands.
#'
#' @param gear_id,band_id Element ids (namespace them with [shiny::NS()]).
#'   The two are wired: the button toggles the band and carries the
#'   `aria-controls` linkage.
#' @param ... Band content.
#' @param band_label Accessible label for the band region.
#' @return A [htmltools::tagList()] with the gear row and the band; place
#'   them together at the top of the block section they configure.
#' @export
gear_band_ui <- function(gear_id, band_id, ..., band_label = "Settings") {
  htmltools::tagList(
    io_block_deps(),
    htmltools::div(
      class = "blockr-gear-row",
      htmltools::tags$button(
        type = "button",
        id = gear_id,
        class = "blockr-gear-btn",
        title = "Advanced settings",
        `aria-label` = "Advanced settings",
        `aria-controls` = band_id,
        `aria-expanded` = "false",
        onclick = sprintf(
          "window.blockrIoGearToggle && window.blockrIoGearToggle('%s','%s');",
          gear_id, band_id
        ),
        htmltools::HTML(gear_icon_svg())
      )
    ),
    htmltools::div(
      id = band_id,
      class = "blockr-settings blockr-settings--beak",
      role = "region",
      `aria-label` = band_label,
      ...
    )
  )
}
