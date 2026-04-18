# Minimal demo for new_download_block().
#
# Two-block workflow: dataset -> download. Loads packages via pkgload so
# edits to blockr.io take effect on restart without a reinstall.
#
# Run from workspace root:
#   Rscript blockr.io/dev/examples/download-block.R

# options(
#   blockr.dock_is_locked = FALSE,
#   blockr.html_table_preview = TRUE,
#   shiny.port = 3838L,
#   shiny.host = "0.0.0.0"
# )

pkgload::load_all("blockr.core")
pkgload::load_all("blockr.react")
pkgload::load_all("blockr.dock")
pkgload::load_all("blockr.io")

board <- new_dock_board(
  blocks = c(
    data     = blockr.core::new_dataset_block(dataset = "iris", package = "datasets"),
    download = new_download_block()
  ),
  links = links(from = "data", to = "download"),
  extensions = list(blockr.react::new_react_extension()),
  layout = dock_workspaces(
    Demo = dock_workspace(layout = list("data", "download"))
  )
)

serve(board)
