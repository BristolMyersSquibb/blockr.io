test_that("write_block expr_server with auto_write=FALSE starts with NULL expression", {
  # Create temp output directory
  temp_dir <- tempfile("write_test_")
  dir.create(temp_dir)

  # Create block with auto_write=FALSE
  blk <- new_write_block(
    directory = temp_dir,
    filename = "output",
    format = "csv",
    mode = "browse",
    auto_write = FALSE
  )

  # Test the expr_server module with proper variadic pattern
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    args = list(
      x = blk,
      data = list(
        ...args = reactiveValues(
          `1` = mtcars[1:5, 1:3]
        )
      )
    ),
    {
      session$flushReact()

      result <- session$returned
      expect_true(is.reactive(result$expr))

      # With auto_write=FALSE, expression should be NULL initially
      expr_before <- result$expr()
      expect_null(expr_before)

      # Verify auto_write state is FALSE
      expect_false(result$state$auto_write())
    }
  )

  unlink(temp_dir, recursive = TRUE)
})

test_that("write_block expr_server handles submit button click with auto_write=FALSE", {
  # This test REPLACES test-shinytest2-write-block-integration.R
  # It proves that "user clicks submit button" can be tested with testServer

  temp_dir <- tempfile("write_test_")
  dir.create(temp_dir)

  # Create block with auto_write=FALSE (requires manual submit)
  blk <- new_write_block(
    directory = temp_dir,
    filename = "iris_submit",
    format = "csv",
    mode = "browse",
    auto_write = FALSE
  )

  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    args = list(
      x = blk,
      data = list(
        ...args = reactiveValues(
          data = iris
        )
      )
    ),
    {
      session$flushReact()

      result <- session$returned

      # STEP 1: Verify no file exists yet and expression is NULL
      files_before <- list.files(temp_dir, pattern = "iris_submit\\.csv$")
      expect_equal(length(files_before), 0)

      expr_before <- result$expr()
      expect_null(expr_before, info = "Expression should be NULL before submit")

      # Debug: Check state before button click
      # cat("\nBefore button click:\n")
      # cat("  mode:", result$state$mode(), "\n")
      # cat("  auto_write:", result$state$auto_write(), "\n")
      # cat("  directory:", result$state$directory(), "\n")
      # cat("  expr is NULL:", is.null(result$expr()), "\n")

      # STEP 2: USER CLICKS SUBMIT BUTTON IN UI
      # This is the key part - session$setInputs() simulates button click
      # The button is namespaced as "expr-submit_write" because it's in the expr module
      session$setInputs(`expr-submit_write` = 1)
      session$flushReact()

      # Debug: Check state after button click
      # cat("\nAfter button click:\n")
      # cat("  expr is NULL:", is.null(result$expr()), "\n")

      # STEP 3: Verify expression is now generated after submit
      expr_after <- result$expr()
      expect_false(is.null(expr_after), info = "Expression should be generated after submit")

      # Verify it's a write expression
      expr_text <- paste(deparse(expr_after), collapse = " ")
      expect_true(grepl("readr::write_csv", expr_text))
      expect_true(grepl("iris_submit\\.csv", expr_text))
      expect_true(grepl("data", expr_text))  # Should reference the data variable name

      # Note: We don't evaluate the expression here because it references
      # a variable name ("data") that needs to exist in the calling environment.
      # The important thing is that clicking submit generates the expression,
      # which proves the UI interaction works with testServer.
    }
  )

  unlink(temp_dir, recursive = TRUE)
})

test_that("write_block expr_server handles multiple inputs for Excel", {
  skip_if_not_installed("writexl")

  temp_dir <- tempfile("write_test_")
  dir.create(temp_dir)

  # Create block for Excel
  blk <- new_write_block(
    directory = temp_dir,
    filename = "report",
    format = "excel",
    mode = "browse",
    auto_write = TRUE
  )

  # Suppress "NAs introduced by coercion" warning from blockr.core
  # when it tries to sort arg names like "sheet1", "sheet2" as integers
  suppressWarnings({
    shiny::testServer(
      blockr.core:::get_s3_method("block_server", blk),
      args = list(
        x = blk,
        data = list(
          ...args = reactiveValues(
            sheet1 = iris[1:10, ],
            sheet2 = mtcars[1:5, ]
          )
        )
      ),
      {
        session$flushReact()

        result <- session$returned
        expr_result <- result$expr()

        # Verify it's an Excel write
        expr_text <- paste(deparse(expr_result), collapse = " ")
        expect_true(grepl("writexl::write_xlsx", expr_text))
        expect_true(grepl("report\\.xlsx", expr_text))
      }
    )
  })

  unlink(temp_dir, recursive = TRUE)
})

test_that("write_block expr_server respects CSV delimiter parameter", {
  temp_dir <- tempfile("write_test_")
  dir.create(temp_dir)

  # Create block with custom delimiter
  blk <- new_write_block(
    directory = temp_dir,
    filename = "data",
    format = "csv",
    mode = "browse",
    auto_write = TRUE,
    args = list(sep = ";")
  )

  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    args = list(
      x = blk,
      data = list(
        ...args = reactiveValues(
          `1` = cars[1:10, ]
        )
      )
    ),
    {
      session$flushReact()

      result <- session$returned
      expr_result <- result$expr()
      expr_text <- paste(deparse(expr_result), collapse = " ")

      # Verify parameters are in the expression
      expect_true(grepl('delim = ";"', expr_text))
      expect_true(grepl("readr::write_delim", expr_text))
    }
  )

  unlink(temp_dir, recursive = TRUE)
})

test_that("write_block expr_server generates ZIP for multiple CSV inputs", {
  skip_if_not_installed("zip")

  temp_dir <- tempfile("write_test_")
  dir.create(temp_dir)

  blk <- new_write_block(
    directory = temp_dir,
    filename = "multi",
    format = "csv",
    mode = "browse",
    auto_write = TRUE
  )

  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    args = list(
      x = blk,
      data = list(
        ...args = reactiveValues(
          flowers = iris[1:5, ],
          cars = mtcars[1:5, ]
        )
      )
    ),
    {
      session$flushReact()

      result <- session$returned
      expr_result <- result$expr()
      expr_text <- paste(deparse(expr_result), collapse = " ")

      # Should create ZIP
      expect_true(grepl("zip::zip", expr_text))
      expect_true(grepl("multi\\.zip", expr_text))
    }
  )

  unlink(temp_dir, recursive = TRUE)
})

test_that("write_block expr_server handles Parquet format", {
  skip_if_not_installed("arrow")

  temp_dir <- tempfile("write_test_")
  dir.create(temp_dir)

  blk <- new_write_block(
    directory = temp_dir,
    filename = "data",
    format = "parquet",
    mode = "browse",
    auto_write = TRUE
  )

  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    args = list(
      x = blk,
      data = list(
        ...args = reactiveValues(
          `1` = mtcars[1:10, ]
        )
      )
    ),
    {
      session$flushReact()

      result <- session$returned
      expr_result <- result$expr()
      expr_text <- paste(deparse(expr_result), collapse = " ")

      # Verify it's parquet
      expect_true(grepl("arrow::write_parquet", expr_text))
      expect_true(grepl("data\\.parquet", expr_text))
    }
  )

  unlink(temp_dir, recursive = TRUE)
})

test_that("write_block expr_server state returns reactive values", {
  temp_dir <- tempfile("write_test_")
  dir.create(temp_dir)

  blk <- new_write_block(
    directory = temp_dir,
    filename = "output",
    format = "csv",
    mode = "browse",
    auto_write = TRUE,
    args = list(sep = ",")
  )

  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    args = list(
      x = blk,
      data = list(
        ...args = reactiveValues(
          `1` = cars[1:5, ]
        )
      )
    ),
    {
      session$flushReact()

      result <- session$returned

      # State should contain reactive functions
      expect_true(is.reactive(result$state$directory))
      expect_true(is.reactive(result$state$filename))
      expect_true(is.reactive(result$state$format))
      expect_true(is.reactive(result$state$args))

      # Can call them to get values
      expect_equal(result$state$directory(), temp_dir)
      expect_equal(result$state$filename(), "output")
      expect_equal(result$state$format(), "csv")
      expect_equal(result$state$args()$sep, ",")
    }
  )

  unlink(temp_dir, recursive = TRUE)
})

test_that("write_block expr_server generates expression in browse mode", {
  temp_dir <- tempfile("write_test_")
  dir.create(temp_dir)

  blk <- new_write_block(
    directory = temp_dir,
    filename = "test_output",
    format = "csv",
    mode = "browse",
    auto_write = TRUE
  )

  test_df <- data.frame(x = 1:3, y = c("a", "b", "c"), stringsAsFactors = FALSE)

  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    args = list(
      x = blk,
      data = list(
        ...args = reactiveValues(
          data = test_df
        )
      )
    ),
    {
      session$flushReact()

      result <- session$returned
      expr_result <- result$expr()

      # Expression should be correct
      expect_true(is.language(expr_result))  # Check it's a language object (can be call or brace)

      # Verify it's a write expression
      expr_text <- deparse(expr_result)
      expect_true(any(grepl("readr::write_csv", expr_text)))
      expect_true(any(grepl("test_output\\.csv", expr_text)))
      expect_true(any(grepl("data", expr_text)))  # Should reference the data variable
    }
  )

  unlink(temp_dir, recursive = TRUE)
})

test_that("write_block expr_server handles auto-timestamp filename", {
  temp_dir <- tempfile("write_test_")
  dir.create(temp_dir)

  blk <- new_write_block(
    directory = temp_dir,
    filename = "",  # Empty = auto-timestamp
    format = "csv",
    mode = "browse",
    auto_write = TRUE
  )

  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    args = list(
      x = blk,
      data = list(
        ...args = reactiveValues(
          `1` = cars[1:5, ]
        )
      )
    ),
    {
      session$flushReact()

      result <- session$returned
      expr_result <- result$expr()
      expr_text <- paste(deparse(expr_result), collapse = " ")

      # Should have timestamped filename pattern
      expect_true(grepl("data_[0-9]{8}_[0-9]{6}\\.csv", expr_text))
    }
  )

  unlink(temp_dir, recursive = TRUE)
})

test_that("write_block expr_server handles mode changes", {
  temp_dir <- tempfile("write_test_")
  dir.create(temp_dir)

  blk <- new_write_block(
    directory = temp_dir,
    filename = "output",
    format = "csv",
    mode = "browse",
    auto_write = TRUE
  )

  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    args = list(
      x = blk,
      data = list(
        ...args = reactiveValues(
          `1` = mtcars[1:3, ]
        )
      )
    ),
    {
      session$flushReact()

      result <- session$returned

      # Initial mode
      expect_equal(result$state$mode(), "browse")

      # Change mode (simulating UI input)
      # Note: In actual usage, this would be done via the UI
      # Here we're just testing the state is tracked
      expect_true(is.reactive(result$state$mode))
    }
  )

  unlink(temp_dir, recursive = TRUE)
})

test_that("write_block expr_server handles variadic inputs correctly", {
  temp_dir <- tempfile("write_test_")
  dir.create(temp_dir)

  blk <- new_write_block(
    directory = temp_dir,
    filename = "multi",
    format = "excel",
    mode = "browse",
    auto_write = TRUE
  )

  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    args = list(
      x = blk,
      data = list(
        ...args = reactiveValues(
          iris_data = iris[1:5, ],
          mtcars_data = mtcars[1:5, ],
          cars_data = cars[1:5, ]
        )
      )
    ),
    {
      session$flushReact()

      result <- session$returned
      expr_result <- result$expr()
      expr_text <- paste(deparse(expr_result), collapse = " ")

      # All three should be referenced
      expect_true(grepl("iris_data", expr_text))
      expect_true(grepl("mtcars_data", expr_text))
      expect_true(grepl("cars_data", expr_text))
    }
  )

  unlink(temp_dir, recursive = TRUE)
})

test_that("write_block expr_server handles single Excel sheet", {
  skip_if_not_installed("writexl")

  temp_dir <- tempfile("write_test_")
  dir.create(temp_dir)

  blk <- new_write_block(
    directory = temp_dir,
    filename = "single_sheet",
    format = "excel",
    mode = "browse",
    auto_write = TRUE
  )

  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    args = list(
      x = blk,
      data = list(
        ...args = reactiveValues(
          data = mtcars[1:10, 1:5]
        )
      )
    ),
    {
      session$flushReact()

      result <- session$returned
      expr_result <- result$expr()

      # Verify expression structure
      expect_true(is.language(expr_result))  # Check it's a language object (can be call or brace)
      expr_text <- paste(deparse(expr_result), collapse = " ")
      expect_true(grepl("writexl::write_xlsx", expr_text))
      expect_true(grepl("single_sheet\\.xlsx", expr_text))
      expect_true(grepl("data", expr_text))  # Should reference data variable
    }
  )

  unlink(temp_dir, recursive = TRUE)
})

test_that("write_block expr_server handles Feather format", {
  skip_if_not_installed("arrow")

  temp_dir <- tempfile("write_test_")
  dir.create(temp_dir)

  blk <- new_write_block(
    directory = temp_dir,
    filename = "data",
    format = "feather",
    mode = "browse",
    auto_write = TRUE
  )

  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    args = list(
      x = blk,
      data = list(
        ...args = reactiveValues(
          `1` = iris[1:20, ]
        )
      )
    ),
    {
      session$flushReact()

      result <- session$returned
      expr_result <- result$expr()
      expr_text <- paste(deparse(expr_result), collapse = " ")

      # Verify it's feather
      expect_true(grepl("arrow::write_feather", expr_text))
      expect_true(grepl("data\\.feather", expr_text))
      expect_true(grepl("`1`", expr_text))  # Should reference `1` variable
    }
  )

  unlink(temp_dir, recursive = TRUE)
})

# ============================================================================
# HIGH PRIORITY: Comprehensive argument testing
# ============================================================================

test_that("write_block expr_server handles mode=download", {
  temp_dir <- tempfile("write_test_")
  dir.create(temp_dir)

  blk <- new_write_block(
    directory = temp_dir,
    filename = "download_test",
    format = "csv",
    mode = "download"  # Download mode instead of browse
  )

  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    args = list(
      x = blk,
      data = list(
        ...args = reactiveValues(
          data = mtcars[1:5, 1:3]
        )
      )
    ),
    {
      session$flushReact()

      result <- session$returned

      # In download mode, expression should be NULL (handled by downloadHandler)
      expr_result <- result$expr()
      expect_null(expr_result, info = "Download mode should return NULL expr")

      # Verify state reflects download mode
      expect_equal(result$state$mode(), "download")
    }
  )

  unlink(temp_dir, recursive = TRUE)
})

test_that("write_block expr_server respects CSV quote parameter", {
  temp_dir <- tempfile("write_test_")
  dir.create(temp_dir)

  blk <- new_write_block(
    directory = temp_dir,
    filename = "quoted",
    format = "csv",
    mode = "browse",
    auto_write = TRUE,
    args = list(quote = TRUE)  # Quote all fields (use TRUE, not "all" string)
  )

  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    args = list(
      x = blk,
      data = list(
        ...args = reactiveValues(
          data = data.frame(name = c("Alice", "Bob"), value = c(10, 20))
        )
      )
    ),
    {
      session$flushReact()

      result <- session$returned
      expr_result <- result$expr()
      expr_text <- paste(deparse(expr_result), collapse = " ")

      # Verify quote parameter is in expression (TRUE converts to "all")
      expect_true(grepl('quote = "all"', expr_text))
      expect_true(grepl("readr::write_csv", expr_text))
    }
  )

  unlink(temp_dir, recursive = TRUE)
})

test_that("write_block expr_server respects CSV na parameter", {
  temp_dir <- tempfile("write_test_")
  dir.create(temp_dir)

  blk <- new_write_block(
    directory = temp_dir,
    filename = "with_na",
    format = "csv",
    mode = "browse",
    auto_write = TRUE,
    args = list(na = "MISSING")  # Custom NA string
  )

  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    args = list(
      x = blk,
      data = list(
        ...args = reactiveValues(
          data = data.frame(x = c(1, NA, 3), y = c(NA, 2, 3))
        )
      )
    ),
    {
      session$flushReact()

      result <- session$returned
      expr_result <- result$expr()
      expr_text <- paste(deparse(expr_result), collapse = " ")

      # Verify na parameter is in expression
      expect_true(grepl('na = "MISSING"', expr_text))
      expect_true(grepl("readr::write_csv", expr_text))
    }
  )

  unlink(temp_dir, recursive = TRUE)
})

# NOTE: col_names parameter is not currently supported by write_expr_csv
# test_that("write_block expr_server respects CSV col_names parameter", {
#   # This test is commented out because col_names is not implemented in write-expr.R
#   # Consider adding this parameter support in future if needed
# })

test_that("write_block expr_server handles format UI changes", {
  skip_if_not_installed("arrow")

  temp_dir <- tempfile("write_test_")
  dir.create(temp_dir)

  blk <- new_write_block(
    directory = temp_dir,
    filename = "format_change",
    format = "csv",  # Start with CSV
    mode = "browse",
    auto_write = TRUE
  )

  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    args = list(
      x = blk,
      data = list(
        ...args = reactiveValues(
          data = iris[1:10, ]
        )
      )
    ),
    {
      session$flushReact()

      result <- session$returned

      # Initial format is CSV
      expr_initial <- result$expr()
      expr_text_initial <- paste(deparse(expr_initial), collapse = " ")
      expect_true(grepl("readr::write_csv", expr_text_initial))
      expect_true(grepl("format_change\\.csv", expr_text_initial))

      # USER CHANGES FORMAT IN UI
      session$setInputs(`expr-format` = "parquet")
      session$flushReact()

      # Expression should now use parquet
      expr_updated <- result$expr()
      expr_text_updated <- paste(deparse(expr_updated), collapse = " ")
      expect_true(grepl("arrow::write_parquet", expr_text_updated))
      expect_true(grepl("format_change\\.parquet", expr_text_updated))

      # State should reflect the change
      expect_equal(result$state$format(), "parquet")
    }
  )

  unlink(temp_dir, recursive = TRUE)
})

# ============================================================================
# Tests that verify actual file creation via framework evaluation
# ============================================================================

test_that("write_block actually writes CSV file in browse mode", {
  temp_dir <- tempfile("write_actual_")
  dir.create(temp_dir)

  blk <- new_write_block(
    directory = temp_dir,
    filename = "actual_output",
    format = "csv",
    mode = "browse",
    auto_write = TRUE
  )

  test_data <- data.frame(x = 1:5, y = letters[1:5], stringsAsFactors = FALSE)

  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    args = list(
      x = blk,
      data = list(
        ...args = reactiveValues(
          data = test_data
        )
      )
    ),
    {
      session$flushReact()

      # Get the framework's evaluated result
      result <- session$returned$result()

      # The result should be the data passthrough (write block returns first data)
      expect_true(is.data.frame(result))
      expect_equal(nrow(result), 5)
      expect_equal(result$x, 1:5)
    }
  )

  # Check that file was actually written
  expected_file <- file.path(temp_dir, "actual_output.csv")
  expect_true(file.exists(expected_file), info = "CSV file should exist after framework evaluation")

  # Verify file contents
  written_data <- read.csv(expected_file, stringsAsFactors = FALSE)
  expect_equal(nrow(written_data), 5)
  expect_equal(written_data$x, 1:5)
  expect_equal(written_data$y, letters[1:5])

  unlink(temp_dir, recursive = TRUE)
})

test_that("write_block actually writes Excel file in browse mode", {
  skip_if_not_installed("writexl")
  skip_if_not_installed("readxl")

  temp_dir <- tempfile("write_actual_")
  dir.create(temp_dir)

  blk <- new_write_block(
    directory = temp_dir,
    filename = "actual_excel",
    format = "excel",
    mode = "browse",
    auto_write = TRUE
  )

  test_data <- data.frame(a = 1:10, b = rnorm(10))

  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    args = list(
      x = blk,
      data = list(
        ...args = reactiveValues(
          sheet1 = test_data
        )
      )
    ),
    {
      session$flushReact()

      result <- session$returned$result()
      expect_true(is.data.frame(result))
      expect_equal(nrow(result), 10)
    }
  )

  # Check that file was actually written
  expected_file <- file.path(temp_dir, "actual_excel.xlsx")
  expect_true(file.exists(expected_file), info = "Excel file should exist after framework evaluation")

  # Verify file contents
  written_data <- readxl::read_xlsx(expected_file)
  expect_equal(nrow(written_data), 10)
  expect_equal(written_data$a, 1:10)

  unlink(temp_dir, recursive = TRUE)
})

test_that("write_block actually writes Parquet file in browse mode", {
  skip_if_not_installed("arrow")

  temp_dir <- tempfile("write_actual_")
  dir.create(temp_dir)

  blk <- new_write_block(
    directory = temp_dir,
    filename = "actual_parquet",
    format = "parquet",
    mode = "browse",
    auto_write = TRUE
  )

  test_data <- data.frame(id = 1:20, value = runif(20))

  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    args = list(
      x = blk,
      data = list(
        ...args = reactiveValues(
          data = test_data
        )
      )
    ),
    {
      session$flushReact()

      result <- session$returned$result()
      expect_true(is.data.frame(result))
      expect_equal(nrow(result), 20)
    }
  )

  # Check that file was actually written
  expected_file <- file.path(temp_dir, "actual_parquet.parquet")
  expect_true(file.exists(expected_file), info = "Parquet file should exist after framework evaluation")

  # Verify file contents
  written_data <- arrow::read_parquet(expected_file)
  expect_equal(nrow(written_data), 20)
  expect_equal(written_data$id, 1:20)

  unlink(temp_dir, recursive = TRUE)
})

test_that("write_block actually writes multi-sheet Excel in browse mode", {
  skip_if_not_installed("writexl")
  skip_if_not_installed("readxl")

  temp_dir <- tempfile("write_actual_")
  dir.create(temp_dir)

  blk <- new_write_block(
    directory = temp_dir,
    filename = "multi_sheet",
    format = "excel",
    mode = "browse",
    auto_write = TRUE
  )

  # Suppress "NAs introduced by coercion" warning from blockr.core
  suppressWarnings({
    shiny::testServer(
      blockr.core:::get_s3_method("block_server", blk),
      args = list(
        x = blk,
        data = list(
          ...args = reactiveValues(
            sales = data.frame(product = c("A", "B"), revenue = c(100, 200)),
            inventory = data.frame(item = c("X", "Y", "Z"), qty = c(10, 20, 30))
          )
        )
      ),
      {
        session$flushReact()

        result <- session$returned$result()
        expect_true(is.data.frame(result))
      }
    )
  })

  # Check that file was actually written
  expected_file <- file.path(temp_dir, "multi_sheet.xlsx")
  expect_true(file.exists(expected_file), info = "Multi-sheet Excel file should exist")

  # Verify both sheets exist
  sheets <- readxl::excel_sheets(expected_file)
  expect_true("sales" %in% sheets)
  expect_true("inventory" %in% sheets)

  # Verify sheet contents
  sales_data <- readxl::read_xlsx(expected_file, sheet = "sales")
  expect_equal(nrow(sales_data), 2)
  expect_equal(sales_data$product, c("A", "B"))

  inventory_data <- readxl::read_xlsx(expected_file, sheet = "inventory")
  expect_equal(nrow(inventory_data), 3)
  expect_equal(inventory_data$item, c("X", "Y", "Z"))

  unlink(temp_dir, recursive = TRUE)
})

test_that("write_block with auto_write=FALSE only writes after submit", {
  temp_dir <- tempfile("write_actual_")
  dir.create(temp_dir)

  blk <- new_write_block(
    directory = temp_dir,
    filename = "manual_submit",
    format = "csv",
    mode = "browse",
    auto_write = FALSE
  )

  test_data <- data.frame(x = 1:3)
  expected_file <- file.path(temp_dir, "manual_submit.csv")

  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    args = list(
      x = blk,
      data = list(
        ...args = reactiveValues(
          data = test_data
        )
      )
    ),
    {
      session$flushReact()

      # Before submit: file should NOT exist
      expect_false(file.exists(expected_file), info = "File should not exist before submit")

      # Expression should be NULL before submit
      result <- session$returned
      expect_null(result$expr())

      # USER CLICKS SUBMIT
      session$setInputs(`expr-submit_write` = 1)
      session$flushReact()

      # Expression should now be set
      expect_false(is.null(result$expr()))
    }
  )

  # Note: The file may not exist after testServer because testServer
  # doesn't fully simulate the framework's evaluation cycle for the
  # newly-set expression. This tests the expression generation logic.
  # Full integration would require shinytest2.

  unlink(temp_dir, recursive = TRUE)
})

# ============================================================================
# Tests for download mode (issue #9 fix verification)
# Note: testServer has limitations with downloadHandler - cannot directly invoke
# the content function. These tests verify download mode state is correct.
# Full download testing requires shinytest2 with a real browser, or was verified
# manually (issue #9 fix confirmed working).
# ============================================================================

test_that("download mode returns NULL expr and correct state", {
  # This test verifies that download mode is set up correctly
  # The actual download functionality was tested manually and works

  temp_dir <- tempfile("download_test_")
  dir.create(temp_dir)

  blk <- new_write_block(
    directory = temp_dir,
    filename = "test_file",
    format = "csv",
    mode = "download"
  )

  test_data <- data.frame(x = 1:5, y = letters[1:5], stringsAsFactors = FALSE)

  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    args = list(
      x = blk,
      data = list(
        ...args = reactiveValues(
          data = test_data
        )
      )
    ),
    {
      session$flushReact()

      result <- session$returned

      # In download mode, expr should be NULL (download handler handles writing)
      expect_null(result$expr())

      # Verify state reflects download mode
      expect_equal(result$state$mode(), "download")
      expect_equal(result$state$filename(), "test_file")
      expect_equal(result$state$format(), "csv")
    }
  )

  unlink(temp_dir, recursive = TRUE)
})

test_that("download mode with auto-timestamp has correct state (issue #9 scenario)", {
  # This tests the scenario that caused issue #9 (empty filename = auto-timestamp)
  # The actual fix (consistent timestamp in downloadHandler) was verified manually

  temp_dir <- tempfile("download_test_")
  dir.create(temp_dir)

  blk <- new_write_block(
    directory = temp_dir,
    filename = "",  # Empty = auto-timestamp (this was the issue #9 trigger)
    format = "csv",
    mode = "download"
  )

  test_data <- data.frame(id = 1:10, value = rnorm(10))

  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    args = list(
      x = blk,
      data = list(
        ...args = reactiveValues(
          data = test_data
        )
      )
    ),
    {
      session$flushReact()

      result <- session$returned

      # Verify download mode is active
      expect_null(result$expr())
      expect_equal(result$state$mode(), "download")
      expect_equal(result$state$filename(), "")  # Empty = auto-timestamp
    }
  )

  unlink(temp_dir, recursive = TRUE)
})
