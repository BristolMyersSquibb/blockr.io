# Demo: full interactive dock app (blockr.dock + session + DAG) showcasing
# the path-input commit-on-Enter rework in blockr.io.
#
# Run from your blockr workspace root (the folder holding blockr.core,
# blockr.dock, ...):
#   source("blockr.io/dev/demo-path-input-dock.R")
# or from within blockr.io:
#   source("dev/demo-path-input-dock.R")
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
#   * Data directory: sidebar option, same commit model.

root <- if (file.exists("DESCRIPTION")) ".." else "."

# blockr.ui must come from source alongside blockr.dock (the "+" block
# browser calls blockr.ui internals; a stale installed version blanks panels)
pkgs <- c(
  "blockr.core", "blockr.ui", "blockr.dock", "blockr.session",
  "blockr.dag", "blockr.dplyr", "blockr.io"
)
for (pkg in pkgs) {
  dir <- file.path(root, pkg)
  if (dir.exists(dir)) {
    pkgload::load_all(dir, quiet = TRUE)
  } else {
    library(pkg, character.only = TRUE)
  }
}

# Demo data directory with a few files + a subdir for autocomplete
demo_root <- file.path(tempdir(), "blockr-io-demo")
data_dir <- file.path(demo_root, "data")
unlink(demo_root, recursive = TRUE)
dir.create(file.path(data_dir, "subdir"), recursive = TRUE)
write.csv(mtcars, file.path(data_dir, "cars.csv"), row.names = FALSE)
write.csv(iris, file.path(data_dir, "flowers.csv"), row.names = FALSE)
write.csv(airquality, file.path(data_dir, "subdir", "air.csv"), row.names = FALSE)

options(
  blockr.data_dir = data_dir,
  blockr.tabular_display = blockr.ui::html_table_display
)
message("Demo data dir: ", data_dir)
message("Write target suggestion: ", file.path(demo_root, "out"))

# serve() returns the shinyApp; runApp() so this also works via source()
shiny::runApp(
  serve(
    new_dock_board(
      blocks = c(
        data = new_read_block(),
        output = new_write_block()
      ),
      links = c(io = new_link("data", "output", "1")),
      extensions = new_dag_extension()
    ),
    plugins = custom_plugins(manage_project())
  )
)
