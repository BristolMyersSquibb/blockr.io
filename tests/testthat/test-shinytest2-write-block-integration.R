library(shinytest2)

# TRUE end-to-end integration test for write_block
#
# This test verifies integration aspects that CANNOT be tested with testServer:
# - UI interactions (clicking the submit button when auto_write=FALSE)
# - Submit button wrapped in conditionalPanel (doesn't render in testServer)
#
# Tests that CAN be done with testServer (expression generation, file writing
# with auto_write=TRUE, etc.) are in test-write-block-server.R

test_that("Write block integration: Submit button with auto_write=FALSE", {
  skip_if_not_installed("shinytest2")
  skip_on_cran()

  # This tests a TRUE user workflow that testServer CANNOT simulate:
  # 1. User has auto_write=FALSE (wants manual control)
  # 2. User sees data preview but NO file written yet
  # 3. User clicks submit button
  # 4. App writes file to disk
  #
  # This tests:
  # - UI button interaction (conditionalPanel-wrapped button)
  # - Real user workflow with manual file writing
  # - Integration between UI click → reactive → file system write

  output_dir <- tempfile("blockr_write_test_")
  dir.create(output_dir, recursive = TRUE)

  # Use single-block pattern (following blockr.dplyr pattern)
  # Provide data via data parameter instead of using board with links
  app_dir <- create_test_app(
    block_code = sprintf(
      'serve(
        new_write_block(
          directory = "%s",
          filename = "iris_submit",
          format = "csv",
          mode = "browse",
          auto_write = FALSE
        ),
        data = list(data = iris),
        id = "block"
      )',
      output_dir
    )
  )

  app <- shinytest2::AppDriver$new(
    app_dir,
    timeout = 30000,
    name = "write_block_submit"
  )

  # Wait for app to initialize
  app$wait_for_idle(duration = 1000)

  # STEP 1: Verify NO file exists yet (auto_write is FALSE)
  files_before <- list.files(output_dir, pattern = "iris_submit\\.csv$")
  expect_equal(length(files_before), 0,
    info = sprintf("Files should not exist before submit. Found: %s",
      paste(list.files(output_dir), collapse = ", ")))

  # Ensure Browse tab is selected (it should be by default since mode="browse")
  # But explicitly set it to make sure the submit button is rendered
  app$set_inputs(`block-expr-mode_pills` = "browse")
  app$wait_for_idle(duration = 500)

  # STEP 2: User clicks the submit button
  # The submit button is in the block's UI with id "block-expr-submit_write"
  # (note: it's wrapped in the expr module, so ID is block-expr-submit_write)
  app$click("block-expr-submit_write")

  # Wait for the write operation to complete
  app$wait_for_idle(duration = 2000)

  # STEP 3: Verify file was created after clicking submit
  files_after <- list.files(output_dir, pattern = "iris_submit\\.csv$", full.names = TRUE)
  expect_equal(length(files_after), 1,
    info = sprintf("Output dir: %s, Files found: %s",
      output_dir, paste(list.files(output_dir), collapse = ", ")))

  # STEP 4: Verify file content is complete (not truncated)
  result_data <- readr::read_csv(files_after[1], show_col_types = FALSE)
  expect_equal(nrow(result_data), 150)  # iris has 150 rows
  expect_equal(ncol(result_data), 5)    # iris has 5 columns
  expect_true(all(c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species") %in% names(result_data)))

  cleanup_test_app(app_dir, app)
  unlink(output_dir, recursive = TRUE)
})
