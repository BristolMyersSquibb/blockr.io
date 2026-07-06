# Demo: path-input commit-on-Enter rework (read + write block)
#
# Run from your local blockr checkout, either
#   - from the blockr.io package dir:  source("dev/demo-path-input.R")
#   - or from the workspace root:      source("blockr.io/dev/demo-path-input.R")
#
# Things to try:
#   * Read block: click the path field -> dropdown lists the demo data dir.
#     Type "car": list filters, the blue "Enter" chip appears, nothing loads
#     yet. Press Enter (or click a suggestion) -> file loads, chip fades to a
#     check, badge shows "CSV".
#   * Write block: type a new nested path (e.g. <out>/reports/2026) slowly —
#     no folders are created while typing. Enter commits ("New directory
#     (created on save)" badge); only "Save to Server" creates dir + file,
#     and the status message path matches the written file.

pkg <- if (file.exists("DESCRIPTION")) "." else "blockr.io"
pkgload::load_all(pkg)

# Demo data directory with a few files + a subdir for autocomplete
demo_root <- file.path(tempdir(), "blockr-io-demo")
data_dir <- file.path(demo_root, "data")
unlink(demo_root, recursive = TRUE)
dir.create(file.path(data_dir, "subdir"), recursive = TRUE)
write.csv(mtcars, file.path(data_dir, "cars.csv"), row.names = FALSE)
write.csv(iris, file.path(data_dir, "flowers.csv"), row.names = FALSE)
write.csv(airquality, file.path(data_dir, "subdir", "air.csv"), row.names = FALSE)

options(blockr.data_dir = data_dir)
message("Demo data dir: ", data_dir)
message("Write target suggestion: ", file.path(demo_root, "out"))

# serve() returns the shinyApp; runApp() so this also works via source()
shiny::runApp(
  blockr.core::serve(
    blockr.core::new_board(
      blocks = c(
        r = new_read_block(),
        w = new_write_block()
      ),
      links = c(blockr.core::new_link("r", "w", "1"))
    )
  )
)
