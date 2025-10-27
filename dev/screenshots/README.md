# Screenshot Generation for blockr.io

This directory contains tools and example data for generating screenshots of blockr.io blocks for use in pkgdown documentation.

## Files

- `validate-screenshot.R`: Functions for generating screenshots of individual blocks
- `generate_all.R`: Script template for automated screenshot generation
- `example_sales_data.csv`: Example CSV file for demonstrations

## Manual Screenshot Generation (Recommended)

Due to the way read blocks store file paths in closures, manual screenshot generation is the most reliable approach:

### Steps:

1. **Start the app with the example data:**
   ```r
   library(blockr.core)
   library(blockr.io)

   # Use absolute path to the example CSV
   csv_path <- normalizePath("dev/screenshots/example_sales_data.csv")

   # Launch the block
   serve(new_read_block(
     path = csv_path,
     csv_sep = ",",
     csv_quote = "\"",
     csv_encoding = "UTF-8"
   ))
   ```

2. **Take a screenshot:**
   - Open the app in your browser
   - Wait for it to fully load and display the CSV options
   - Take a screenshot (width ~800px, height ~600px recommended)
   - Save as `man/figures/read-block.png`

3. **Verify the screenshot:**
   - Should show the read block interface
   - CSV-specific options should be visible (delimiter, quote, encoding, etc.)
   - Data preview should show the sales data

## Automated Screenshot Generation

The `generate_all.R` script is provided as a template, but may encounter issues with block serialization. If you want to try automated generation:

```r
source("dev/screenshots/generate_all.R")
```

Note: This may fail with "object not found" errors due to how block closures are serialized.

## Example Data

The `example_sales_data.csv` file contains sample sales data with:
- Date column (12 months of 2024)
- Product column (Widget A, Widget B, Gadget C)
- Revenue and units columns
- 12 rows total

This file can be used in examples, tests, and documentation.

## Output

Screenshots should be saved to `man/figures/` with the naming pattern:
- `read-block.png`

These can be referenced in pkgdown documentation and vignette files using:
```r
knitr::include_graphics("../man/figures/read-block.png")
```
