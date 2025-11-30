# Dock workflow example: Read block
#
# This example shows the read block in a dock board layout.
# Run with: source("dev/examples/dock-read.R")

library(blockr.core)
library(blockr.dock)
library(blockr.dag)

devtools::load_all(".")

# Create a sample CSV file
temp_csv <- tempfile(fileext = ".csv")
write.csv(mtcars, temp_csv, row.names = FALSE)

# Serve read block in dock board
serve(

  new_dock_board(
    blocks = c(
      data = new_read_block(
        path = temp_csv,
        source = "path",
        combine = "auto"
      )
    ),
    extensions = new_dag_extension()
  )
)
