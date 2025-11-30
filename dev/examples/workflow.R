# blockr.io Example Workflow
#
# Complete ETL pipeline: read -> transform -> write
# Run with: source("dev/examples/workflow.R")

library(blockr.core)
library(blockr.dock)
library(blockr.dag)
library(blockr.dplyr)

devtools::load_all(".")

# Create a sample CSV file
temp_csv <- tempfile(fileext = ".csv")
write.csv(mtcars, temp_csv, row.names = FALSE)

# Serve full ETL pipeline in dock board
serve(
  new_dock_board(
    blocks = c(
      # Read data
      data = new_read_block(
        path = temp_csv,
        source = "path"
      ),
      # Filter rows
      filtered = new_filter_block(
        conditions = list(
          list(column = "cyl", values = c(4, 6), mode = "include")
        )
      ),
      # Select columns
      selected = new_select_block(
        columns = c("mpg", "cyl", "hp", "wt")
      ),
      # Write output
      output = new_write_block(
        mode = "download",
        format = "excel",
        filename = "filtered_cars"
      )
    ),
    links = list(
      from = c("data", "filtered", "selected"),
      to = c("filtered", "selected", "output"),
      input = c("data", "data", "data")
    ),
    extensions = new_dag_extension()
  )
)
