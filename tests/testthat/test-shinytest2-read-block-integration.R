# TRUE end-to-end integration tests for read_block
#
# These tests verify integration aspects that CANNOT be tested with testServer:
# - User workflows with UI interactions
# - Full app lifecycle (serve() → UI → reactive → exportTestValues)
#
# Note: We don't need a separate smoke test for serve() - if the integration
# test passes, it proves serve() works correctly.

test_that("user changes CSV delimiter in UI and data updates", {
  skip_if_not_installed("shinytest2")
  skip_on_cran()

  # This tests a TRUE user workflow that testServer CANNOT simulate:
  # 1. User loads file with wrong delimiter (sees malformed data)
  # 2. User changes delimiter input in UI
  # 3. App reactively updates and shows correct data
  #
  # This tests:
  # - UI input → reactive update → data output pipeline
  # - Real user interaction workflow
  # - Integration between Shiny inputs and block reactives

  test_dir <- tempfile("blockr_files_")
  dir.create(test_dir)
  csv_file <- file.path(test_dir, "test.csv")

  # Create semicolon-delimited file
  write.table(
    data.frame(a = 1:5, b = 6:10),
    csv_file,
    sep = ";",
    row.names = FALSE,
    quote = FALSE
  )

  # Start app with WRONG delimiter (default comma)
  # This simulates user selecting a file without knowing its delimiter
  app_dir <- create_test_app(
    block_code = sprintf(
      'serve(new_read_block(path = "%s"), id = "block")',
      csv_file
    )
  )

  app <- shinytest2::AppDriver$new(
    app_dir,
    timeout = 30000,
    name = "delimiter_change_workflow"
  )

  # STEP 1: Verify initial state with wrong delimiter
  # readr will parse semicolon file with comma delimiter incorrectly
  # It will see each line as a single column with malformed data
  initial_result <- get_block_result(app)

  # Initial data MUST be malformed with wrong delimiter
  # The test scenario is: user sees broken data, then fixes it
  expect_true(
    ncol(initial_result) != 2 || !all(c("a", "b") %in% names(initial_result)),
    info = "Initial data should be malformed with wrong delimiter (comma instead of semicolon)"
  )

  # STEP 2: User changes delimiter in UI
  # Input ID discovered via debug: "block-expr-csv_sep"
  app$set_inputs(`block-expr-csv_sep` = ";")

  # Wait for reactive update
  app$wait_for_idle(timeout = 10000)

  # STEP 3: Verify data is now correct after UI change
  updated_result <- get_block_result(app)

  # NOW the data should be correct
  expect_true(is.data.frame(updated_result))
  expect_equal(nrow(updated_result), 5)
  expect_equal(ncol(updated_result), 2)
  expect_true("a" %in% names(updated_result))
  expect_true("b" %in% names(updated_result))
  expect_equal(updated_result$a, 1:5)
  expect_equal(updated_result$b, 6:10)

  cleanup_test_app(app_dir, app)
  unlink(test_dir, recursive = TRUE)
})
