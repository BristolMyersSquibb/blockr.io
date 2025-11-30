#' Validate a blockr block by generating a screenshot
#'
#' This function creates a temporary Shiny app with the provided block,
#' takes a screenshot, and returns the result. It's designed to be a
#' simple, direct way to test whether a block implementation works correctly.
#'
#' @param block A blockr block object (e.g., from new_read_block())
#' @param data Data to use for the block (default: mtcars)
#' @param filename Name for the screenshot file (default: auto-generated)
#' @param output_dir Directory to save screenshot (default: "man/figures")
#' @param width Screenshot width in pixels (default: 800)
#' @param height Screenshot height in pixels (default: 600)
#' @param delay Seconds to wait for app to load (default: 5)
#' @param expand_advanced Logical. If TRUE, attempts to click "advanced options"
#'   toggle before taking screenshot (default: FALSE)
#' @param use_dock Logical. If TRUE, uses blockr.dock for improved styling
#'   and automatically crops to the block panel (default: TRUE)
#' @param dataset Character. Name of the dataset to use in dock mode
#'   (default: "mtcars"). Must be a dataset available via data().
#' @param dataset_package Character. Package containing the dataset
#'   (default: "datasets").
#' @param verbose Print progress messages (default: TRUE)
#' @param block_code Optional character string. R code to create the block inline
#'   instead of loading from RDS. Useful for blocks with file paths that don't
#'   serialize well. Example: 'new_read_block(path = "/path/to/file.csv")'
#'
#' @return A list with components:
#'   - success: Logical indicating if screenshot was created successfully
#'   - path: Full path to the screenshot file (NULL if failed)
#'   - error: Error message if failed (NULL if successful)
#'   - filename: Name of the screenshot file
#'
#' @examples
#' \dontrun{
#' # Simple usage with read block
#' result <- validate_block_screenshot(
#'   new_read_block(path = "data.csv")
#' )
#'
#' # With write block (needs data)
#' result <- validate_block_screenshot(
#'   new_write_block(mode = "download"),
#'   data = mtcars,
#'   filename = "write-block.png"
#' )
#'
#' # Check if successful
#' if (result$success) {
#'   cat("Screenshot saved to:", result$path)
#' } else {
#'   cat("Failed:", result$error)
#' }
#' }
#'
#' @export
validate_block_screenshot <- function(
  block,
  data = datasets::mtcars,
  filename = NULL,
  output_dir = "man/figures",
  width = 800,
  height = 600,
  delay = 5,
  expand_advanced = FALSE,
  use_dock = TRUE,
  dataset = "mtcars",
  dataset_package = "datasets",
  dataset_y = NULL,
  dataset_y_package = "datasets",
  input_names = c("x", "y"),
  verbose = TRUE,
  block_code = NULL
) {
  # Set NOT_CRAN environment variable for shinytest2
  old_not_cran <- Sys.getenv("NOT_CRAN", unset = NA)
  Sys.setenv(NOT_CRAN = "true")
  on.exit(
    {
      if (is.na(old_not_cran)) {
        Sys.unsetenv("NOT_CRAN")
      } else {
        Sys.setenv(NOT_CRAN = old_not_cran)
      }
    },
    add = TRUE
  )

  # Check dependencies
  if (!requireNamespace("shinytest2", quietly = TRUE)) {
    return(list(
      success = FALSE,
      path = NULL,
      error = paste(
        "shinytest2 package is required.",
        "Install with: install.packages('shinytest2')"
      ),
      filename = filename
    ))
  }

  if (!requireNamespace("blockr.core", quietly = TRUE)) {
    return(list(
      success = FALSE,
      path = NULL,
      error = "blockr.core package is required",
      filename = filename
    ))
  }

  # Auto-generate filename if not provided
  if (is.null(filename)) {
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    block_class <- class(block)[1]
    filename <- sprintf("%s_%s.png", block_class, timestamp)
  }

  # Ensure filename has .png extension
  if (!grepl("\\.png$", filename)) {
    filename <- paste0(filename, ".png")
  }

  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  # Full output path
  output_path <- file.path(output_dir, filename)

  if (verbose) {
    cat(sprintf(
      "Generating screenshot for block of class '%s'...\n",
      class(block)[1]
    ))
  }

  # Wrap data in the expected list format
  # Check if data is already a properly formatted list (e.g., for join blocks with x and y)
  if (is.list(data) && !is.data.frame(data)) {
    # Already a list - check if it has expected names like x, y, or data
    if (any(c("x", "y", "data") %in% names(data))) {
      data_list <- data
    } else {
      # List but not properly named - wrap it
      data_list <- list(data = data)
    }
  } else {
    # Single data frame or other object
    data_list <- list(data = data)
  }

  # Check for magick package if using dock mode (needed for cropping)
  if (use_dock && !requireNamespace("magick", quietly = TRUE)) {
    return(list(
      success = FALSE,
      path = NULL,
      error = paste(
        "magick package is required for dock mode cropping.",
        "Install with: install.packages('magick')"
      ),
      filename = filename
    ))
  }

  # Try to create the screenshot
  result <- tryCatch(
    {
      # Create temporary directory for the app
      temp_dir <- tempfile("blockr_validation_")
      dir.create(temp_dir)

      # Save data to RDS file to avoid deparse issues
      saveRDS(data_list, file.path(temp_dir, "data.rds"))

      # Only save block to RDS if not using inline block_code
      if (is.null(block_code)) {
        saveRDS(block, file.path(temp_dir, "block.rds"))
      }

      # Detect if this is a data block (doesn't need data parameter)
      is_data_block <- inherits(block, "data_block")

      # Determine how to load the block in app.R
      block_load_code <- if (!is.null(block_code)) {
        # Use inline code - more reliable for blocks with file paths
        sprintf("block <- %s", block_code)
      } else {
        # Load from RDS file
        'block <- readRDS("block.rds")'
      }

      # Create app.R file - different content based on use_dock
      if (use_dock) {
        if (is_data_block) {
          # Data blocks (like read_block) are sources - show them as the first block
          # with no upstream data source in dock
          app_content <- sprintf(
            '
library(blockr.core)
library(blockr.dock)

# Load the blockr.io package
# Try to load from development first, fall back to installed version
tryCatch(
  devtools::load_all("%s"),
  error = function(e) {
    library(blockr.io)
  }
)

# Create/load block
%s

# Run the app using dock board - data block as the only block
blockr.core::serve(
  blockr.dock::new_dock_board(
    blocks = c(
      a = block
    )
  )
)
            ',
            normalizePath("."),
            block_load_code
          )
        } else {
          # Transform blocks (like write_block) need a data source upstream
          app_content <- sprintf(
            '
library(blockr.core)
library(blockr.dock)

# Load the blockr.io package
# Try to load from development first, fall back to installed version
tryCatch(
  devtools::load_all("%s"),
  error = function(e) {
    library(blockr.io)
  }
)

# Load data
data <- readRDS("data.rds")

# Create/load block
%s

# Run the app using dock board with default layout
# serve() is from blockr.core, dock_board is from blockr.dock
# Default layout: extensions on left, blocks on right
# We will crop to the right panel (blocks) after taking screenshot
blockr.core::serve(
  blockr.dock::new_dock_board(
    blocks = c(
      a = blockr.core::new_dataset_block("%s", package = "%s"),
      b = block
    ),
    links = list(from = "a", to = "b", input = "data")
  )
)
            ',
            normalizePath("."),
            block_load_code,
            dataset,
            dataset_package
          )
        }
      } else {
        # Use blockr.core directly (original behavior)
        if (is_data_block) {
          # Data blocks don't take data parameter
          app_content <- sprintf(
            '
library(blockr.core)

# Load the blockr.io package
# Try to load from development first, fall back to installed version
tryCatch(
  devtools::load_all("%s"),
  error = function(e) {
    library(blockr.io)
  }
)

# Create/load block
%s

# Run the app (data blocks don\'t need data parameter)
blockr.core::serve(block)
            ',
            normalizePath("."),
            block_load_code
          )
        } else {
          # Transform blocks need data parameter
          app_content <- sprintf(
            '
library(blockr.core)

# Load the blockr.io package
# Try to load from development first, fall back to installed version
tryCatch(
  devtools::load_all("%s"),
  error = function(e) {
    library(blockr.io)
  }
)

# Load data
data <- readRDS("data.rds")

# Create/load block
%s

# Run the app
blockr.core::serve(
  block,
  data = data
)
            ',
            normalizePath("."),
            block_load_code
          )
        }
      }

      writeLines(app_content, file.path(temp_dir, "app.R"))

      # Use shinytest2 for screenshot with ability to interact
      app <- shinytest2::AppDriver$new(
        app_dir = temp_dir,
        name = "block_screenshot"
      )

      # Set viewport size
      app$set_window_size(width = width, height = height)

      # Wait for app to load - use simple sleep instead of wait_for_idle
      # which can be unreliable
      Sys.sleep(delay)

      # Try to expand advanced options if requested
      if (expand_advanced) {
        tryCatch(
          {
            # Click all advanced toggles on the page
            app$run_js(
              "
              var toggles = document.querySelectorAll('.block-advanced-toggle');
              toggles.forEach(function(toggle) {
                toggle.click();
              });
              "
            )
            # Wait for animation/expansion
            Sys.sleep(0.5)
          },
          error = function(e) {
            # Block doesn't have advanced options - that's fine
            if (verbose) {
              cat("  (No advanced options found - continuing)\n")
            }
          }
        )
      }

      # Remove existing file if it exists (to allow overwriting)
      if (file.exists(output_path)) {
        file.remove(output_path)
      }

      # Take screenshot
      app$get_screenshot(output_path)

      # If using dock mode, crop to just the panel content
      if (use_dock && file.exists(output_path)) {
        # Get the bounding box of the panel using JavaScript
        # Try multiple selectors in order of preference
        # Note: use get_js instead of run_js to get the return value
        panel_bounds <- tryCatch(
          {
            app$get_js(
              "
              (function() {
                // Find the panel that contains actual block content (the right panel)
                // In default dock layout: left = extensions (empty), right = blocks
                var groupViews = document.querySelectorAll('.dv-groupview');

                // Find the groupview that contains block content
                // Look for the one with actual shiny content inside
                for (var i = 0; i < groupViews.length; i++) {
                  var panel = groupViews[i];
                  // Check if this panel has actual content (not just empty toolbar)
                  var hasContent = panel.querySelector('.shiny-html-output') ||
                                   panel.querySelector('.block-container') ||
                                   panel.querySelector('[class*=\"blockr\"]') ||
                                   panel.querySelector('.form-group') ||
                                   panel.querySelector('.selectize-control');

                  if (hasContent && panel.offsetWidth > 100) {
                    var rect = panel.getBoundingClientRect();
                    return {
                      x: Math.round(rect.left),
                      y: Math.round(rect.top),
                      width: Math.round(rect.width),
                      height: Math.round(rect.height),
                      selector: '.dv-groupview (with content)'
                    };
                  }
                }

                // Fallback: get the last (rightmost) groupview
                if (groupViews.length > 0) {
                  var lastPanel = groupViews[groupViews.length - 1];
                  var rect = lastPanel.getBoundingClientRect();
                  return {
                    x: Math.round(rect.left),
                    y: Math.round(rect.top),
                    width: Math.round(rect.width),
                    height: Math.round(rect.height),
                    selector: '.dv-groupview (last)'
                  };
                }

                return null;
              })()
              "
            )
          },
          error = function(e) NULL
        )

        if (!is.null(panel_bounds) && !is.null(panel_bounds$width)) {
          if (verbose) {
            selector_info <- if (!is.null(panel_bounds$selector)) {
              paste0(" (selector: ", panel_bounds$selector, ")")
            } else {
              ""
            }
            cat(sprintf(
              "  Cropping to panel bounds: x=%d, y=%d, w=%d, h=%d%s\n",
              panel_bounds$x, panel_bounds$y,
              panel_bounds$width, panel_bounds$height,
              selector_info
            ))
          }

          # Use magick to crop the image
          img <- magick::image_read(output_path)
          # Add small padding around the panel
          padding <- 0
          crop_geometry <- sprintf(
            "%dx%d+%d+%d",
            panel_bounds$width + padding * 2,
            panel_bounds$height + padding * 2,
            max(0, panel_bounds$x - padding),
            max(0, panel_bounds$y - padding)
          )
          img_cropped <- magick::image_crop(img, crop_geometry)
          magick::image_write(img_cropped, output_path)
        } else if (verbose) {
          cat("  Warning: Could not detect panel bounds for cropping\n")
        }
      }

      # Stop the app and cleanup
      app$stop()
      unlink(temp_dir, recursive = TRUE)

      # Check if file was created
      if (file.exists(output_path)) {
        if (verbose) {
          cat(sprintf("[SUCCESS] Screenshot saved to: %s\n", output_path))
        }

        list(
          success = TRUE,
          path = normalizePath(output_path),
          error = NULL,
          filename = filename
        )
      } else {
        list(
          success = FALSE,
          path = NULL,
          error = "Screenshot file was not created",
          filename = filename
        )
      }
    },
    error = function(e) {
      # Cleanup on error
      if (exists("temp_dir") && dir.exists(temp_dir)) {
        unlink(temp_dir, recursive = TRUE)
      }

      if (verbose) {
        cat(sprintf("[ERROR] Failed to create screenshot: %s\n", e$message))
        # Print traceback for debugging
        if (!is.null(e$trace)) {
          cat("Traceback:\n")
          print(e$trace)
        }
      }

      list(
        success = FALSE,
        path = NULL,
        error = e$message,
        filename = filename
      )
    }
  )

  return(result)
}

#' Batch validate multiple blocks with screenshots
#'
#' Convenience function to validate multiple blocks at once and generate
#' a summary report of which blocks work and which don't.
#'
#' @param blocks Named list of blocks to validate (can also be a list of lists
#'   with 'block' and 'expand_advanced' elements)
#' @param data Data to use for all blocks (can also be a named list
#'   matching block names)
#' @param output_dir Directory to save screenshots (default: "man/figures")
#' @param verbose Print progress messages (default: TRUE)
#'
#' @return A data frame with validation results for each block
#'
#' @examples
#' \dontrun{
#' # Test multiple blocks
#' blocks <- list(
#'   read = new_read_block(path = "data.csv"),
#'   write = new_write_block(mode = "download")
#' )
#'
#' results <- validate_blocks_batch(blocks)
#' print(results)
#' }
#'
#' @export
validate_blocks_batch <- function(
  blocks,
  data = datasets::mtcars,
  output_dir = "man/figures",
  verbose = TRUE
) {
  if (!is.list(blocks)) {
    stop("blocks must be a list")
  }

  # Get block names
  block_names <- names(blocks)
  if (is.null(block_names)) {
    block_names <- paste0("block_", seq_along(blocks))
    names(blocks) <- block_names
  }

  # Prepare data for each block
  if (is.list(data) && !is.data.frame(data)) {
    # data is a named list
    data_list <- data
  } else {
    # data is a single dataset, use for all blocks
    data_list <- stats::setNames(
      rep(list(data), length(blocks)),
      block_names
    )
  }

  # Validate each block
  results <- lapply(block_names, function(name) {
    if (verbose) {
      cat(sprintf("\nValidating block '%s'...\n", name))
    }

    block_data <- if (name %in% names(data_list)) {
      data_list[[name]]
    } else {
      data # fallback to default data
    }

    # Extract block and expand_advanced flag
    block_item <- blocks[[name]]
    if (is.list(block_item) && "block" %in% names(block_item)) {
      # Block is wrapped with options
      block_obj <- block_item$block
      expand_adv <- isTRUE(block_item$expand_advanced)
    } else {
      # Block is standalone
      block_obj <- block_item
      expand_adv <- FALSE
    }

    result <- validate_block_screenshot(
      block = block_obj,
      data = block_data,
      filename = paste0(name, ".png"),
      output_dir = output_dir,
      expand_advanced = expand_adv,
      verbose = verbose
    )

    data.frame(
      block_name = name,
      success = result$success,
      screenshot = ifelse(result$success, result$filename, NA),
      error = ifelse(is.null(result$error), "", result$error),
      stringsAsFactors = FALSE
    )
  })

  # Combine results
  results_df <- do.call(rbind, results)

  if (verbose) {
    cat("\n=== Validation Summary ===\n")
    cat(sprintf("Total blocks: %d\n", nrow(results_df)))
    cat(sprintf("Successful: %d\n", sum(results_df$success)))
    cat(sprintf("Failed: %d\n", sum(!results_df$success)))

    if (any(!results_df$success)) {
      cat("\nFailed blocks:\n")
      failed <- results_df[!results_df$success, ]
      for (i in seq_len(nrow(failed))) {
        cat(sprintf("  - %s: %s\n", failed$block_name[i], failed$error[i]))
      }
    }
  }

  return(results_df)
}
