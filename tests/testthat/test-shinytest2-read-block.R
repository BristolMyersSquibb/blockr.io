test_that("read_block loads CSV file with default settings", {
  skip_if_not_installed("shinytest2")
  skip_on_cran()

  # Create test CSV file
  test_dir <- tempfile("blockr_files_")
  dir.create(test_dir)
  csv_file <- file.path(test_dir, "test.csv")
  write.csv(
    data.frame(x = 1:5, y = letters[1:5]),
    csv_file,
    row.names = FALSE
  )

  # Create app with pre-loaded CSV
  app_dir <- create_test_app(
    block_code = sprintf(
      'serve(new_read_block(path = "%s"))',
      csv_file
    )
  )

  app <- shinytest2::AppDriver$new(
    app_dir,
    timeout = 30000,
    name = "read_csv_default"
  )

  result_data <- get_block_result(app)

  # Verify data loaded correctly
  expect_true(is.data.frame(result_data) || is.list(result_data))

  # If it's a list, extract the actual data
  if (is.list(result_data) && !is.data.frame(result_data)) {
    # Try to find the data frame in the list
    result_data <- result_data[[1]]
  }

  expect_true(is.data.frame(result_data))
  expect_equal(nrow(result_data), 5)
  expect_equal(ncol(result_data), 2)
  expect_equal(result_data$x, 1:5)
  expect_equal(result_data$y, letters[1:5])

  cleanup_test_app(app_dir, app)
  unlink(test_dir, recursive = TRUE)
})

test_that("read_block handles multiple CSV files with rbind", {
  skip_if_not_installed("shinytest2")
  skip_on_cran()

  # Create multiple CSV files
  test_dir <- tempfile("blockr_files_")
  dir.create(test_dir)

  csv1 <- file.path(test_dir, "data1.csv")
  csv2 <- file.path(test_dir, "data2.csv")

  write.csv(
    data.frame(id = 1:3, value = c(10, 20, 30)),
    csv1,
    row.names = FALSE
  )
  write.csv(
    data.frame(id = 4:6, value = c(40, 50, 60)),
    csv2,
    row.names = FALSE
  )

  # Create app with multiple files and rbind
  app_dir <- create_test_app(
    block_code = sprintf(
      'serve(new_read_block(path = c("%s", "%s"), combine = "rbind"))',
      csv1,
      csv2
    )
  )

  app <- shinytest2::AppDriver$new(
    app_dir,
    timeout = 30000,
    name = "read_csv_rbind"
  )

  result_data <- get_block_result(app)

  # Verify files were combined
  expect_equal(nrow(result_data), 6)
  expect_equal(result_data$id, 1:6)
  expect_equal(result_data$value, c(10, 20, 30, 40, 50, 60))

  cleanup_test_app(app_dir, app)
  unlink(test_dir, recursive = TRUE)
})

test_that("read_block handles CSV with custom delimiter", {
  skip_if_not_installed("shinytest2")
  skip_on_cran()

  # Create semicolon-delimited file
  test_dir <- tempfile("blockr_files_")
  dir.create(test_dir)
  csv_file <- file.path(test_dir, "test.csv")

  write.table(
    data.frame(a = 1:5, b = 6:10),
    csv_file,
    sep = ";",
    row.names = FALSE,
    quote = FALSE
  )

  # Create app with custom separator
  app_dir <- create_test_app(
    block_code = sprintf(
      'serve(new_read_block(path = "%s", csv_sep = ";"))',
      csv_file
    )
  )

  app <- shinytest2::AppDriver$new(
    app_dir,
    timeout = 30000,
    name = "read_csv_delimiter"
  )

  result_data <- get_block_result(app)

  # Verify data loaded correctly with custom delimiter
  expect_equal(nrow(result_data), 5)
  expect_equal(ncol(result_data), 2)
  expect_equal(result_data$a, 1:5)
  expect_equal(result_data$b, 6:10)

  cleanup_test_app(app_dir, app)
  unlink(test_dir, recursive = TRUE)
})
