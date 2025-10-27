test_that("write_block expr_server generates correct CSV expression", {
  # Create temp output directory
  temp_dir <- tempfile("write_test_")
  dir.create(temp_dir)

  # Create block
  blk <- new_write_block(
    directory = temp_dir,
    filename = "output",
    format = "csv",
    mode = "browse"
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

      # Expression should be NULL before submit
      expr_before <- result$expr()
      expect_null(expr_before)

      # Simulate submit button click
      session$setInputs(submit_write = 1)
      session$flushReact()

      # Get the expression after submit
      expr_result <- result$expr()
      expect_true(inherits(expr_result, "call"))

      # Verify it's a write expression
      expr_text <- deparse(expr_result)
      expect_true(any(grepl("readr::write_csv", expr_text)))
      expect_true(any(grepl("output\\.csv", expr_text)))
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
    format = "excel"
  )

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
    format = "csv"
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
    format = "parquet"
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
    mode = "browse"
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
      expect_true(inherits(expr_result, "call"))

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
    format = "csv"
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
    mode = "browse"
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
    format = "excel"
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
    format = "excel"
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
      expect_true(inherits(expr_result, "call"))
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
    format = "feather"
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
