#!/usr/bin/env Rscript

# Generate screenshots for blockr.io blocks
#
# This script creates screenshots of the read block for use in pkgdown documentation.
# Screenshots are saved to man/figures/ directory.
#
# To run: source("dev/screenshots/generate_all.R")

# Set NOT_CRAN environment variable BEFORE loading any packages
# This is required for shinytest2 to work in non-interactive mode
Sys.setenv(NOT_CRAN = "true")

# Load required packages
library(shinytest2)
devtools::load_all(".")

cat("Generating screenshots for blockr.io blocks...\n")
cat("Output directory: man/figures/\n\n")

# =============================================================================
# READ BLOCK - File reading with CSV options visible
# =============================================================================
cat("1/1 - Read block\n")

# Create a CSV file in dev/screenshots directory (permanent location)
screenshots_dir <- normalizePath("dev/screenshots")
csv_file <- file.path(screenshots_dir, "example_sales_data.csv")

# Create example data
write.csv(
  data.frame(
    date = seq(as.Date("2024-01-01"), by = "month", length.out = 12),
    product = rep(c("Widget A", "Widget B", "Gadget C"), each = 4),
    revenue = c(1200, 1500, 1800, 2100, 950, 1100, 1250, 1400,
                2200, 2400, 2600, 2800),
    units = c(120, 150, 180, 210, 95, 110, 125, 140,
              220, 240, 260, 280)
  ),
  csv_file,
  row.names = FALSE
)

cat("Created example CSV at:", csv_file, "\n")

# Create temp app directory
app_dir <- tempfile("blockr_screenshot_")
dir.create(app_dir)

# Get package root
pkg_root <- normalizePath(".")

# Create app.R with block code as string (not serialized object)
app_content <- sprintf('
library(blockr.core)

# Load blockr.io from development
pkg_path <- "%s"
if (requireNamespace("devtools", quietly = TRUE)) {
  tryCatch(
    devtools::load_all(pkg_path, quiet = TRUE),
    error = function(e) library(blockr.io)
  )
} else {
  library(blockr.io)
}

# Create and serve the block
serve(new_read_block(
  path = "%s",
  csv_sep = ",",
  csv_quote = "\\"",
  csv_encoding = "UTF-8"
))
', pkg_root, csv_file)

writeLines(app_content, file.path(app_dir, "app.R"))

cat("Created test app at:", app_dir, "\n")
cat("Launching app and taking screenshot...\n")

# Launch app and take screenshot
tryCatch({
  app <- AppDriver$new(
    app_dir = app_dir,
    name = "read_block_screenshot",
    width = 800,
    height = 600
  )

  # Wait for app to load
  Sys.sleep(3)

  # Take screenshot
  screenshot_path <- file.path(normalizePath("man/figures"), "read-block.png")
  app$get_screenshot(screenshot_path)

  cat("Screenshot saved to:", screenshot_path, "\n")

  # Stop app
  app$stop()

  cat("\n✓ Screenshot generated successfully!\n")
}, error = function(e) {
  cat("\n[ERROR] Failed to create screenshot:", conditionMessage(e), "\n")
}, finally = {
  # Clean up temp directory
  unlink(app_dir, recursive = TRUE)
})
