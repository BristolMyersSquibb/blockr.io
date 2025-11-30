#!/usr/bin/env Rscript

# Generate screenshots for all blockr.io blocks
#
# This script creates screenshots of all blocks for use in pkgdown documentation.
# Screenshots are saved to man/figures/ directory.
#
# To run: source("dev/screenshots/generate_all.R")

# Set NOT_CRAN environment variable BEFORE loading any packages
# This is required for shinytest2 to work in non-interactive mode
Sys.setenv(NOT_CRAN = "true")

# Load package with devtools::load_all() to ensure latest changes are picked up
devtools::load_all(".")

# Source the validation function
source("dev/screenshots/validate-screenshot.R")

cat("Generating screenshots for all blockr.io blocks...\n")
cat("Output directory: man/figures/\n\n")

# Common screenshot settings
SCREENSHOT_WIDTH <- 1400
SCREENSHOT_HEIGHT <- 700
SCREENSHOT_DELAY <- 3

# =============================================================================
# 1. READ BLOCK - File reading with CSV file pre-loaded
# =============================================================================
cat("1/2 - Read block\n")

# Create a CSV file in dev/screenshots directory (permanent location)
screenshots_dir <- normalizePath("dev/screenshots")
csv_file <- file.path(screenshots_dir, "example_sales_data.csv")

# Create example data
write.csv(
  data.frame(
    date = seq(as.Date("2024-01-01"), by = "month", length.out = 12),
    product = rep(c("Widget A", "Widget B", "Gadget C"), each = 4),
    revenue = c(
      1200, 1500, 1800, 2100, 950, 1100, 1250, 1400,
      2200, 2400, 2600, 2800
    ),
    units = c(
      120, 150, 180, 210, 95, 110, 125, 140,
      220, 240, 260, 280
    )
  ),
  csv_file,
  row.names = FALSE
)

# Use block_code parameter to avoid serialization issues with file paths
validate_block_screenshot(
  block = new_read_block(
    path = csv_file,
    args = list(sep = ",", quote = "\"", encoding = "UTF-8")
  ),
  filename = "read-block.png",
  output_dir = "man/figures",
  width = SCREENSHOT_WIDTH,
  height = SCREENSHOT_HEIGHT,
  delay = SCREENSHOT_DELAY,
  verbose = FALSE,
  block_code = sprintf(
    'new_read_block(path = "%s", args = list(sep = ",", quote = "\\"", encoding = "UTF-8"))',
    csv_file
  )
)

# =============================================================================
# 2. WRITE BLOCK - File writing with download mode
# =============================================================================
cat("2/2 - Write block\n")
validate_block_screenshot(
  block = new_write_block(mode = "download", format = "csv", filename = ""),
  data = mtcars,
  filename = "write-block.png",
  output_dir = "man/figures",
  width = SCREENSHOT_WIDTH,
  height = SCREENSHOT_HEIGHT,
  delay = SCREENSHOT_DELAY,
  verbose = FALSE
)

cat("\n=== All screenshots generated ===\n")
cat("Screenshots saved to: man/figures/\n")
