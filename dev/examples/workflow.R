# blockr.io Example Workflow
#
# Complete ETL pipeline: read -> transform -> write
# Run with: source("dev/examples/workflow.R")

devtools::load_all("blockr.core")

# library(blockr.core)
# library(blockr.dock)
library(blockr.dag)
library(blockr.dplyr)
library(blockr.extra)


devtools::load_all("blockr.dock")
devtools::load_all("blockr.session")
devtools::load_all("blockr.io")

# Create a sample CSV file
temp_csv <- tempfile(fileext = ".csv")
write.csv(mtcars, temp_csv, row.names = FALSE)


options(
  blockr.tabular_display = blockr.ui::html_table_display
)


# Serve full ETL pipeline in dock board
serve(
  new_dock_board(
    blocks = c(
      # Read data
      #
      data = new_read_block(
        path = temp_csv,
        source = "path"
      ),
      # # Filter rows
      # filtered = new_filter_block(
      #   conditions = list(
      #     list(column = "cyl", values = c(4, 6), mode = "include")
      #   )
      # ),
      # # Select columns
      # selected = new_select_block(
      #   columns = c("mpg", "cyl", "hp", "wt")
      # ),
      # # Write output
      output = new_write_block(
        mode = "download",
        format = "excel",
        filename = "filtered_cars"
      )
    ),
    links = list(
      from = c("data"),
      to = c("output"),
      input = c("data")
    ),
    extensions = new_dag_extension()
  ),
  plugins = custom_plugins(manage_project())
)
