# Dock workflow example: Read -> Write pipeline
#
# This example shows a complete read-to-write workflow in a dock board layout.
# Run with: source("dev/examples/dock-write.R")

library(blockr.core)
library(blockr.dock)
library(blockr.dag)

devtools::load_all(".")

# Create a sample CSV file
temp_csv <- tempfile(fileext = ".csv")
write.csv(mtcars, temp_csv, row.names = FALSE)

# Serve read -> write pipeline in dock board
serve(
  new_dock_board(
    blocks = c(
      data = new_dataset_block(dataset = "iris"),
      output = new_write_block(
        mode = "download",
        format = "csv",
        filename = ""
      )
    ),
    links = list(from = "data", to = "output", input = "data"),
    extensions = new_dag_extension()
  )
)
