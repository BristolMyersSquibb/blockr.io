#' HTML dependencies for blockr.io block UIs
#'
#' Two dependencies feed the block UIs:
#'
#' - `io_settings_band_dep()`: the settings-band CSS only (`inst/css/
#'   settings-band.css`, vendored from blockr.viz; it moves to blockr.ui with
#'   the shared layer). blockr.io builds its bands as hand-written R DOM using
#'   `.blockr-settings__grid` / `__field`, opened/closed via the
#'   `blockr-settings--open` class, and uses native checkbox inputs — so it does
#'   NOT ship the settings-band.js `Blockr.checkbox` factory.
#' - `io_blocks_dep()`: io-owned block styling (`io-blocks.css`) and the
#'   gear-band toggle helper (`io-blocks.js`).
#'
#' `io_block_deps()` bundles both for inclusion in a block's `ui`.
#'
#' @return An [htmltools::htmlDependency()], or a `tagList` of both for
#'   `io_block_deps()`.
#' @noRd
io_settings_band_dep <- memoise0(function() {
  htmltools::htmlDependency(
    name = "io-settings-band",
    # CSS only. blockr.io builds its settings bands as hand-written R DOM
    # (.blockr-settings__grid / __field, toggled via blockr-settings--open by
    # blockrIoGearToggle in io-blocks.js) and uses native checkbox inputs, so
    # it never calls the vendored settings-band.js `Blockr.checkbox` factory.
    # That script was dead weight on every read/write/download page -- dropped,
    # and inst/assets/js/settings-band.js deleted with it. (The shared design-
    # system band belongs in blockr.ui; this just stops shipping a stale copy.)
    # Bump the suffix on every settings-band.css edit (asset cache).
    version = paste0(utils::packageVersion("blockr.io"), ".2"),
    src = system.file("assets", package = "blockr.io"),
    stylesheet = "css/settings-band.css"
  )
})

#' @noRd
io_blocks_dep <- memoise0(function() {
  htmltools::htmlDependency(
    name = "blockr-io-blocks",
    version = utils::packageVersion("blockr.io"),
    src = system.file("assets", package = "blockr.io"),
    script = "js/io-blocks.js",
    stylesheet = "css/io-blocks.css"
  )
})

#' @noRd
io_block_deps <- memoise0(function() {
  tagList(
    io_settings_band_dep(),
    io_blocks_dep()
  )
})
