test_that("read_block expr_server generates correct CSV expression", {
  # Create test CSV file
  temp_csv <- tempfile(fileext = ".csv")
  write.csv(
    data.frame(x = 1:5, y = letters[1:5]),
    temp_csv,
    row.names = FALSE
  )

  # Create block
  blk <- new_read_block(path = temp_csv)

  # Test the expr_server module directly
  shiny::testServer(
    blk$expr_server,
    args = list(), # Data blocks don't need data argument
    {
      session$flushReact()

      result <- session$returned
      expect_true(is.reactive(result$expr))

      # Get the expression
      expr_result <- result$expr()
      expect_true(inherits(expr_result, "call"))

      # Verify it's a readr::read_csv call
      expr_text <- deparse(expr_result)
      expect_true(any(grepl("readr::read_csv", expr_text)))
      expect_true(any(grepl(basename(temp_csv), expr_text)))

      # NEW: Evaluate expression and verify data output
      data <- eval(expr_result)
      expect_true(is.data.frame(data))
      expect_equal(nrow(data), 5)
      expect_equal(ncol(data), 2)
      expect_equal(data$x, 1:5)
      expect_equal(data$y, letters[1:5])
    }
  )

  unlink(temp_csv)
})

test_that("read_block expr_server handles multiple files with rbind", {
  # Create test CSV files
  temp_csv1 <- tempfile(fileext = ".csv")
  temp_csv2 <- tempfile(fileext = ".csv")
  write.csv(
    data.frame(id = 1:3, value = c(10, 20, 30)),
    temp_csv1,
    row.names = FALSE
  )
  write.csv(
    data.frame(id = 4:6, value = c(40, 50, 60)),
    temp_csv2,
    row.names = FALSE
  )

  # Create block with rbind
  blk <- new_read_block(
    path = c(temp_csv1, temp_csv2),
    combine = "rbind"
  )

  shiny::testServer(
    blk$expr_server,
    args = list(),
    {
      session$flushReact()

      result <- session$returned
      expr_result <- result$expr()

      # Verify it's an rbind expression
      expr_text <- paste(deparse(expr_result), collapse = " ")
      expect_true(grepl("rbind", expr_text))
      expect_true(grepl("readr::read_csv", expr_text))

      # NEW: Evaluate expression and verify data output
      data <- eval(expr_result)
      expect_true(is.data.frame(data))
      expect_equal(nrow(data), 6)
      expect_equal(data$id, 1:6)
      expect_equal(data$value, c(10, 20, 30, 40, 50, 60))
    }
  )

  unlink(c(temp_csv1, temp_csv2))
})

test_that("read_block expr_server respects CSV parameters", {
  # Create test file with semicolon delimiter
  temp_csv <- tempfile(fileext = ".csv")
  write.table(
    data.frame(a = 1:5, b = 6:10),
    temp_csv,
    sep = ";",
    row.names = FALSE,
    quote = FALSE
  )

  # Create block with custom delimiter
  blk <- new_read_block(
    path = temp_csv,
    csv_sep = ";"
  )

  shiny::testServer(
    blk$expr_server,
    args = list(),
    {
      session$flushReact()

      result <- session$returned
      expr_result <- result$expr()
      expr_text <- paste(deparse(expr_result), collapse = " ")

      # Verify parameters are in the expression
      expect_true(grepl('delim = ";"', expr_text))

      # NEW: Evaluate expression and verify data output
      data <- eval(expr_result)
      expect_true(is.data.frame(data))
      expect_equal(nrow(data), 5)
      expect_equal(ncol(data), 2)
      expect_equal(data$a, 1:5)
      expect_equal(data$b, 6:10)
    }
  )

  unlink(temp_csv)
})

test_that("read_block expr_server handles Excel files", {
  skip_if_not_installed("writexl")
  skip_if_not_installed("readxl")

  # Create test Excel file
  temp_xlsx <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(
    list(Sheet1 = data.frame(x = 1:5), Sheet2 = data.frame(y = 6:10)),
    temp_xlsx
  )

  # Create block reading specific sheet
  blk <- new_read_block(
    path = temp_xlsx,
    excel_sheet = "Sheet2"
  )

  shiny::testServer(
    blk$expr_server,
    args = list(),
    {
      session$flushReact()

      result <- session$returned
      expr_result <- result$expr()
      expr_text <- paste(deparse(expr_result), collapse = " ")

      # Verify it's readxl and has sheet parameter
      expect_true(grepl("readxl::read_excel", expr_text))
      expect_true(grepl('sheet = "Sheet2"', expr_text))
    }
  )

  unlink(temp_xlsx)
})

test_that("read_block expr_server state returns reactive values", {
  temp_csv <- tempfile(fileext = ".csv")
  write.csv(mtcars[1:5, ], temp_csv, row.names = FALSE)

  blk <- new_read_block(
    path = temp_csv,
    csv_sep = ",",
    csv_skip = 2
  )

  shiny::testServer(
    blk$expr_server,
    args = list(),
    {
      session$flushReact()

      result <- session$returned

      # State should contain reactive functions
      expect_true(is.reactive(result$state$path))
      expect_true(is.reactive(result$state$csv_sep))
      expect_true(is.reactive(result$state$csv_skip))

      # Can call them to get values
      expect_equal(length(result$state$path()), 1)
      expect_equal(result$state$csv_sep(), ",")
      expect_equal(result$state$csv_skip(), 2)
    }
  )

  unlink(temp_csv)
})

test_that("read_block expr_server evaluates expression correctly", {
  # Create test CSV
  temp_csv <- tempfile(fileext = ".csv")
  test_data <- data.frame(x = 1:3, y = c("a", "b", "c"))
  write.csv(test_data, temp_csv, row.names = FALSE)

  blk <- new_read_block(path = temp_csv)

  shiny::testServer(
    blk$expr_server,
    args = list(),
    {
      session$flushReact()

      result <- session$returned
      expr_result <- result$expr()

      # Actually evaluate the expression
      data_result <- eval(expr_result)

      # Verify the data matches
      expect_equal(nrow(data_result), 3)
      expect_equal(ncol(data_result), 2)
      expect_equal(data_result$x, 1:3)
      expect_equal(data_result$y, c("a", "b", "c"))
    }
  )

  unlink(temp_csv)
})
