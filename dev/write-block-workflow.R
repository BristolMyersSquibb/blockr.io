# Write Block Example Workflow
#
# This workflow demonstrates the new_write_block functionality including:
# - Reading data from multiple sources
# - Transforming data
# - Writing results in various formats
# - Using fixed filenames vs auto-timestamp
# - Multi-dataframe outputs

library(blockr.core)
devtools::load_all()

# Make sure we have a temp directory for outputs
output_dir <- tempfile("blockr_write_demo_")
dir.create(output_dir, recursive = TRUE)
cat("Output directory:", output_dir, "\n")

# ==============================================================================
# EXAMPLE 1: Simple CSV write with fixed filename
# ==============================================================================
cat("\n=== Example 1: Write single CSV with fixed filename ===\n")

serve(
  new_board(
    blocks = c(
      data = new_dataset_block(selected_dataset = "mtcars"),
      writer = new_write_block(
        directory = output_dir,
        filename = "mtcars_export", # Fixed name - overwrites on changes
        format = "csv"
      )
    ),
    links = c(
      new_link("data", "writer", "1")
    )
  )
)
This mostly works nicely, so I'm gonna commit.