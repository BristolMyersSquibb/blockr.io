#' HTML dependencies for blockr.io block UIs
#'
#' The controls, the gear tray, the buttons and the tokens come from
#' blockr.ui ([blockr.ui::controls_dep()]). `io_blocks_dep()` adds the few
#' rules that belong to io's own markup (`io-blocks.css`, every rule scoped
#' to an `io-` class). `io_block_deps()` bundles both for a block's `ui`.
#'
#' @return An [htmltools::htmlDependency()], or a `tagList` of dependencies
#'   for `io_block_deps()`.
#' @noRd
io_blocks_dep <- memoise0(function() {
  htmltools::htmlDependency(
    name = "blockr-io-blocks",
    version = utils::packageVersion("blockr.io"),
    src = system.file("assets", package = "blockr.io"),
    stylesheet = "css/io-blocks.css"
  )
})

#' @noRd
io_block_deps <- function() {
  tagList(
    blockr.ui::controls_dep(),
    io_blocks_dep()
  )
}
