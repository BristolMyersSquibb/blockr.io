#' HTML dependencies for blockr.io block UIs
#'
#' Two dependencies feed the block UIs:
#'
#' - `io_settings_band_dep()`: the settings band + design-system checkbox,
#'   vendored verbatim from blockr.viz (`inst/{css,js}/settings-band.{css,js}`
#'   is the canonical source; it moves to blockr.ui with the shared layer).
#'   blockr.io only uses the CSS side — bands are hand-built R DOM using
#'   `.blockr-settings__grid` / `__field`, opened/closed via the
#'   `blockr-settings--open` class.
#' - `io_blocks_dep()`: io-owned block styling (`io-blocks.css`) and the
#'   gear-band toggle helper (`io-blocks.js`).
#'
#' `io_block_deps()` bundles both for inclusion in a block's `ui`.
#'
#' @return An [htmltools::htmlDependency()], or a `tagList` of both for
#'   `io_block_deps()`.
#' @noRd
io_settings_band_dep <- function() {
  htmltools::htmlDependency(
    name = "io-settings-band",
    # Bump the suffix on every settings-band.css/js edit (asset cache).
    version = paste0(utils::packageVersion("blockr.io"), ".1"),
    src = system.file("assets", package = "blockr.io"),
    script = "js/settings-band.js",
    stylesheet = "css/settings-band.css"
  )
}

#' @noRd
io_blocks_dep <- function() {
  htmltools::htmlDependency(
    name = "blockr-io-blocks",
    version = utils::packageVersion("blockr.io"),
    src = system.file("assets", package = "blockr.io"),
    script = "js/io-blocks.js",
    stylesheet = "css/io-blocks.css"
  )
}

#' @noRd
io_block_deps <- function() {
  tagList(
    io_settings_band_dep(),
    io_blocks_dep()
  )
}
