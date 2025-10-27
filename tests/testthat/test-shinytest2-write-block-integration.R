library(shinytest2)

test_that("Write block integration: Simple CSV write with fixed filename (browse mode)", {
  skip_if_not_installed("shinytest2")
  skip_on_cran()

  # Create temp output directory
  output_dir <- tempfile("blockr_write_test_")
  dir.create(output_dir, recursive = TRUE)

  # Create test app
  app_dir <- create_test_app(
    block_code = sprintf(
      'serve(new_board(
        blocks = c(
          data = new_dataset_block(selected_dataset = "mtcars"),
          writer = new_write_block(
            directory = "%s",
            filename = "mtcars_export",
            format = "csv",
            mode = "browse"
          )
        ),
        links = c(
          new_link("data", "writer", "1")
        )
      ))',
      output_dir
    )
  )

  app <- shinytest2::AppDriver$new(
    app_dir,
    timeout = 30000,
    name = "write_block_csv"
  )

  # Give it time to initialize and write
  app$wait_for_idle(duration = 3000)

  # Verify file was written
  files <- list.files(output_dir, pattern = "mtcars_export\\.csv$", full.names = TRUE)
  expect_equal(length(files), 1, info = sprintf("Output dir: %s, Files found: %s", output_dir, paste(list.files(output_dir), collapse = ", ")))

  # Verify file content
  result_data <- readr::read_csv(files[1], show_col_types = FALSE)
  expect_equal(nrow(result_data), 32)  # mtcars has 32 rows
  expect_equal(ncol(result_data), 11)  # mtcars has 11 columns

  # Cleanup
  cleanup_test_app(app_dir, app)
  unlink(output_dir, recursive = TRUE)
})

test_that("Write block integration: Auto-timestamped Excel export (browse mode)", {
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("writexl")
  skip_if_not_installed("readxl")
  skip_on_cran()

  output_dir <- tempfile("blockr_write_test_")
  dir.create(output_dir, recursive = TRUE)

  app_dir <- create_test_app(
    block_code = sprintf(
      'serve(new_board(
        blocks = c(
          data = new_dataset_block(selected_dataset = "iris"),
          writer = new_write_block(
            directory = "%s",
            filename = "",
            format = "excel",
            mode = "browse"
          )
        ),
        links = c(
          new_link("data", "writer", "1")
        )
      ))',
      output_dir
    )
  )

  app <- shinytest2::AppDriver$new(
    app_dir,
    timeout = 30000,
    name = "write_block_excel"
  )

  app$wait_for_idle(duration = 3000)

  # Verify timestamped file was created
  files <- list.files(output_dir, pattern = "data_[0-9]{8}_[0-9]{6}\\.xlsx$", full.names = TRUE)
  expect_equal(length(files), 1, info = sprintf("Output dir: %s, Files found: %s", output_dir, paste(list.files(output_dir), collapse = ", ")))

  # Verify file content
  result_data <- readxl::read_excel(files[1])
  expect_equal(nrow(result_data), 150)  # iris has 150 rows

  cleanup_test_app(app_dir, app)
  unlink(output_dir, recursive = TRUE)
})

test_that("Write block integration: CSV with custom delimiter (browse mode)", {
  skip_if_not_installed("shinytest2")
  skip_on_cran()

  output_dir <- tempfile("blockr_write_test_")
  dir.create(output_dir, recursive = TRUE)

  app_dir <- create_test_app(
    block_code = sprintf(
      'serve(new_board(
        blocks = c(
          data = new_dataset_block(selected_dataset = "cars"),
          writer = new_write_block(
            directory = "%s",
            filename = "cars_semicolon",
            format = "csv",
            mode = "browse",
            args = list(sep = ";")
          )
        ),
        links = c(
          new_link("data", "writer", "1")
        )
      ))',
      output_dir
    )
  )

  app <- shinytest2::AppDriver$new(
    app_dir,
    timeout = 30000,
    name = "write_block_csv_semicolon"
  )

  app$wait_for_idle(duration = 3000)

  # Verify file was written
  files <- list.files(output_dir, pattern = "cars_semicolon\\.csv$", full.names = TRUE)
  expect_equal(length(files), 1)

  # Verify file uses semicolon delimiter
  result_data <- readr::read_delim(files[1], delim = ";", show_col_types = FALSE)
  expect_equal(nrow(result_data), 50)  # cars has 50 rows

  cleanup_test_app(app_dir, app)
  unlink(output_dir, recursive = TRUE)
})

test_that("Write block integration: Parquet format (browse mode)", {
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("arrow")
  skip_on_cran()

  output_dir <- tempfile("blockr_write_test_")
  dir.create(output_dir, recursive = TRUE)

  app_dir <- create_test_app(
    block_code = sprintf(
      'serve(new_board(
        blocks = c(
          data = new_dataset_block(selected_dataset = "mtcars"),
          writer = new_write_block(
            directory = "%s",
            filename = "mtcars_compressed",
            format = "parquet",
            mode = "browse"
          )
        ),
        links = c(
          new_link("data", "writer", "1")
        )
      ))',
      output_dir
    )
  )

  app <- shinytest2::AppDriver$new(
    app_dir,
    timeout = 30000,
    name = "write_block_parquet"
  )

  app$wait_for_idle(duration = 3000)

  # Verify file was written
  files <- list.files(output_dir, pattern = "mtcars_compressed\\.parquet$", full.names = TRUE)
  expect_equal(length(files), 1)

  # Verify file content
  result_data <- arrow::read_parquet(files[1])
  expect_equal(nrow(result_data), 32)

  cleanup_test_app(app_dir, app)
  unlink(output_dir, recursive = TRUE)
})

test_that("Write block integration: Submit button with auto_write=FALSE (browse mode)", {
  skip_if_not_installed("shinytest2")
  skip_on_cran()

  output_dir <- tempfile("blockr_write_test_")
  dir.create(output_dir, recursive = TRUE)

  # Create app with auto_write=FALSE - requires submit button click
  app_dir <- create_test_app(
    block_code = sprintf(
      'serve(new_board(
        blocks = c(
          data = new_dataset_block(selected_dataset = "iris"),
          writer = new_write_block(
            directory = "%s",
            filename = "iris_submit",
            format = "csv",
            mode = "browse",
            auto_write = FALSE
          )
        ),
        links = c(
          new_link("data", "writer", "1")
        )
      ))',
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

  # Verify NO file exists yet (auto_write is FALSE)
  files_before <- list.files(output_dir, pattern = "iris_submit\\.csv$")
  expect_equal(length(files_before), 0,
    info = sprintf("Files should not exist before submit. Found: %s",
      paste(list.files(output_dir), collapse = ", ")))

  # Find and click the submit button
  # The submit button is in the writer block's UI with id "writer-submit_write"
  app$click("writer-submit_write")

  # Wait for the write operation to complete
  app$wait_for_idle(duration = 2000)

  # Verify file was created after clicking submit
  files_after <- list.files(output_dir, pattern = "iris_submit\\.csv$", full.names = TRUE)
  expect_equal(length(files_after), 1,
    info = sprintf("Output dir: %s, Files found: %s",
      output_dir, paste(list.files(output_dir), collapse = ", ")))

  # Verify file content
  result_data <- readr::read_csv(files_after[1], show_col_types = FALSE)
  expect_equal(nrow(result_data), 150)  # iris has 150 rows
  expect_equal(ncol(result_data), 5)    # iris has 5 columns

  cleanup_test_app(app_dir, app)
  unlink(output_dir, recursive = TRUE)
})
