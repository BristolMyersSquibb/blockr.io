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
    args = list(sep = ";")
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
    args = list(sheet = "Sheet2")
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
    args = list(sep = ",", skip = 2)
  )

  shiny::testServer(
    blk$expr_server,
    args = list(),
    {
      session$flushReact()

      result <- session$returned

      # State should contain reactive functions
      expect_true(is.reactive(result$state$path))
      expect_true(is.reactive(result$state$args))

      # Can call them to get values
      expect_equal(length(result$state$path()), 1)
      expect_equal(result$state$args()$sep, ",")
      expect_equal(result$state$args()$skip, 2)
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

test_that("read_block expr_server handles UI input changes for delimiter", {
  # This test REPLACES test-shinytest2-read-block-integration.R
  # It proves that "user changes delimiter in UI" can be tested with testServer

  # Create semicolon-delimited file
  temp_csv <- tempfile(fileext = ".csv")
  write.table(
    data.frame(a = 1:5, b = 6:10),
    temp_csv,
    sep = ";",
    row.names = FALSE,
    quote = FALSE
  )

  # Create block with WRONG delimiter (default comma)
  # This simulates user selecting a file without knowing its delimiter
  blk <- new_read_block(path = temp_csv)

  shiny::testServer(
    blk$expr_server,
    args = list(),
    {
      session$flushReact()

      result <- session$returned

      # STEP 1: Verify initial state with wrong delimiter
      expr_initial <- result$expr()
      expr_text_initial <- paste(deparse(expr_initial), collapse = " ")

      # Should default to comma delimiter (readr default)
      # This will produce malformed data when evaluated
      data_wrong <- tryCatch(
        eval(expr_initial),
        error = function(e) NULL
      )

      # Data should be malformed (readr will read it as single column or error)
      if (!is.null(data_wrong)) {
        expect_true(
          ncol(data_wrong) != 2 || !all(c("a", "b") %in% names(data_wrong)),
          info = "Initial data should be malformed with wrong delimiter"
        )
      }

      # STEP 2: USER CHANGES DELIMITER IN UI
      # This is the key part - session$setInputs() simulates UI interaction
      session$setInputs(csv_sep = ";")
      session$flushReact()

      # STEP 3: Verify expression updated with new delimiter
      expr_updated <- result$expr()
      expr_text_updated <- paste(deparse(expr_updated), collapse = " ")
      expect_true(grepl('delim = ";"', expr_text_updated))

      # STEP 4: Verify data is now correct after UI change
      data_correct <- eval(expr_updated)
      expect_true(is.data.frame(data_correct))
      expect_equal(nrow(data_correct), 5)
      expect_equal(ncol(data_correct), 2)
      expect_true("a" %in% names(data_correct))
      expect_true("b" %in% names(data_correct))
      expect_equal(data_correct$a, 1:5)
      expect_equal(data_correct$b, 6:10)
    }
  )

  unlink(temp_csv)
})

# ============================================================================
# HIGH PRIORITY: Comprehensive argument testing
# ============================================================================

test_that("read_block expr_server handles source=url", {
  skip_on_cran()  # Don't hit external URLs on CRAN

  # Use a stable public data URL
  url <- "https://raw.githubusercontent.com/datasets/co2-ppm/master/data/co2-mm-mlo.csv"

  blk <- new_read_block(
    path = url,
    source = "url"  # URL source
  )

  shiny::testServer(
    blk$expr_server,
    args = list(),
    {
      session$flushReact()

      result <- session$returned
      expr_result <- result$expr()

      # Verify it's a read_csv call
      # Note: URL is downloaded to temp file, so expression contains temp path not URL
      expr_text <- paste(deparse(expr_result), collapse = " ")
      expect_true(grepl("readr::read_csv", expr_text))

      # Verify state reflects URL source (this is where URL is stored)
      expect_equal(result$state$source(), "url")
      expect_equal(result$state$path(), url)

      # Verify the expression can be evaluated (URL was downloaded successfully)
      data <- eval(expr_result)
      expect_true(is.data.frame(data))
      expect_true(nrow(data) > 0)
    }
  )
})

test_that("read_block expr_server handles combine=cbind", {
  # Create two CSV files with different columns
  temp_csv1 <- tempfile(fileext = ".csv")
  temp_csv2 <- tempfile(fileext = ".csv")

  write.csv(
    data.frame(a = 1:5),
    temp_csv1,
    row.names = FALSE
  )
  write.csv(
    data.frame(b = 6:10),
    temp_csv2,
    row.names = FALSE
  )

  blk <- new_read_block(
    path = c(temp_csv1, temp_csv2),
    combine = "cbind"  # Combine columns side-by-side
  )

  shiny::testServer(
    blk$expr_server,
    args = list(),
    {
      session$flushReact()

      result <- session$returned
      expr_result <- result$expr()

      # Verify it's a cbind expression
      expr_text <- paste(deparse(expr_result), collapse = " ")
      expect_true(grepl("cbind", expr_text))
      expect_true(grepl("readr::read_csv", expr_text))

      # Evaluate and verify data structure
      data <- eval(expr_result)
      expect_equal(nrow(data), 5)
      expect_equal(ncol(data), 2)  # Two columns combined
      expect_true("a" %in% names(data))
      expect_true("b" %in% names(data))
      expect_equal(data$a, 1:5)
      expect_equal(data$b, 6:10)
    }
  )

  unlink(c(temp_csv1, temp_csv2))
})

test_that("read_block expr_server handles combine=first", {
  # Create multiple CSV files
  temp_csv1 <- tempfile(fileext = ".csv")
  temp_csv2 <- tempfile(fileext = ".csv")

  write.csv(
    data.frame(x = 1:3, y = c("a", "b", "c")),
    temp_csv1,
    row.names = FALSE
  )
  write.csv(
    data.frame(x = 4:6, y = c("d", "e", "f")),
    temp_csv2,
    row.names = FALSE
  )

  blk <- new_read_block(
    path = c(temp_csv1, temp_csv2),
    combine = "first"  # Only use first file
  )

  shiny::testServer(
    blk$expr_server,
    args = list(),
    {
      session$flushReact()

      result <- session$returned
      expr_result <- result$expr()

      # Should only read first file (no rbind/cbind)
      expr_text <- paste(deparse(expr_result), collapse = " ")
      expect_true(grepl("readr::read_csv", expr_text))
      expect_false(grepl("rbind", expr_text))
      expect_false(grepl("cbind", expr_text))

      # Evaluate and verify only first file data
      data <- eval(expr_result)
      expect_equal(nrow(data), 3)  # Only first file's 3 rows
      expect_equal(data$x, 1:3)
      expect_equal(data$y, c("a", "b", "c"))
    }
  )

  unlink(c(temp_csv1, temp_csv2))
})

test_that("read_block expr_server respects CSV n_max parameter", {
  # Create CSV with many rows
  temp_csv <- tempfile(fileext = ".csv")
  write.csv(
    data.frame(id = 1:100, value = rnorm(100)),
    temp_csv,
    row.names = FALSE
  )

  blk <- new_read_block(
    path = temp_csv,
    args = list(n_max = 10)  # Only read first 10 rows
  )

  shiny::testServer(
    blk$expr_server,
    args = list(),
    {
      session$flushReact()

      result <- session$returned
      expr_result <- result$expr()

      # Verify n_max parameter is in expression
      expr_text <- paste(deparse(expr_result), collapse = " ")
      expect_true(grepl("n_max = 10", expr_text))

      # Evaluate and verify only 10 rows loaded
      data <- eval(expr_result)
      expect_equal(nrow(data), 10)
      expect_equal(data$id, 1:10)
    }
  )

  unlink(temp_csv)
})

test_that("read_block expr_server respects Excel range parameter", {
  skip_if_not_installed("writexl")
  skip_if_not_installed("readxl")

  # Create Excel file
  temp_xlsx <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(
    list(Data = data.frame(x = 1:20, y = letters[1:20])),
    temp_xlsx
  )

  blk <- new_read_block(
    path = temp_xlsx,
    args = list(sheet = "Data", range = "A1:B5")  # Only read first 5 rows
  )

  shiny::testServer(
    blk$expr_server,
    args = list(),
    {
      session$flushReact()

      result <- session$returned
      expr_result <- result$expr()

      # Verify range parameter is in expression
      expr_text <- paste(deparse(expr_result), collapse = " ")
      expect_true(grepl('range = "A1:B5"', expr_text))
      expect_true(grepl("readxl::read_excel", expr_text))
    }
  )

  unlink(temp_xlsx)
})

test_that("read_block expr_server handles TSV files", {
  # Create TSV file
  temp_tsv <- tempfile(fileext = ".tsv")
  write.table(
    data.frame(name = c("Alice", "Bob", "Charlie"), age = c(25, 30, 35)),
    temp_tsv,
    sep = "\t",
    row.names = FALSE,
    quote = FALSE
  )

  # TSV files need explicit sep parameter (auto-detection not implemented yet)
  blk <- new_read_block(
    path = temp_tsv,
    args = list(sep = "\t")  # Specify tab delimiter
  )

  shiny::testServer(
    blk$expr_server,
    args = list(),
    {
      session$flushReact()

      result <- session$returned
      expr_result <- result$expr()

      # Verify it uses read_tsv (tab delimiter)
      expr_text <- paste(deparse(expr_result), collapse = " ")
      expect_true(grepl("readr::read_tsv", expr_text))

      # Evaluate and verify data
      data <- eval(expr_result)
      expect_equal(nrow(data), 3)
      expect_equal(data$name, c("Alice", "Bob", "Charlie"))
      expect_equal(data$age, c(25, 30, 35))
    }
  )

  unlink(temp_tsv)
})

test_that("read_block expr_server handles Parquet files", {
  skip_if_not_installed("arrow")

  # Create Parquet file
  temp_parquet <- tempfile(fileext = ".parquet")
  arrow::write_parquet(
    data.frame(x = 1:50, y = rnorm(50)),
    temp_parquet
  )

  blk <- new_read_block(path = temp_parquet)

  shiny::testServer(
    blk$expr_server,
    args = list(),
    {
      session$flushReact()

      result <- session$returned
      expr_result <- result$expr()

      # Verify it's arrow::read_parquet
      expr_text <- paste(deparse(expr_result), collapse = " ")
      expect_true(grepl("arrow::read_parquet", expr_text))

      # Evaluate and verify data
      data <- eval(expr_result)
      expect_equal(nrow(data), 50)
      expect_equal(data$x, 1:50)
    }
  )

  unlink(temp_parquet)
})

test_that("read_block expr_server handles Feather files", {
  skip_if_not_installed("arrow")

  # Create Feather file
  temp_feather <- tempfile(fileext = ".feather")
  arrow::write_feather(
    data.frame(id = 1:30, category = rep(c("A", "B", "C"), 10)),
    temp_feather
  )

  blk <- new_read_block(path = temp_feather)

  shiny::testServer(
    blk$expr_server,
    args = list(),
    {
      session$flushReact()

      result <- session$returned
      expr_result <- result$expr()

      # Verify it's arrow::read_feather
      expr_text <- paste(deparse(expr_result), collapse = " ")
      expect_true(grepl("arrow::read_feather", expr_text))

      # Evaluate and verify data
      data <- eval(expr_result)
      expect_equal(nrow(data), 30)
      expect_equal(unique(data$category), c("A", "B", "C"))
    }
  )

  unlink(temp_feather)
})
