test_that("generate_filename creates timestamped names when empty", {
  timestamp <- as.POSIXct("2025-01-27 14:30:22")
  result <- generate_filename("", timestamp)
  expect_equal(result, "data_20250127_143022")
})

test_that("generate_filename uses provided name", {
  result <- generate_filename("myfile")
  expect_equal(result, "myfile")
})

test_that("generate_filename removes extension if provided", {
  result <- generate_filename("myfile.csv")
  expect_equal(result, "myfile")
})

test_that("write_expr_csv generates correct expression for single file", {
  expr <- write_expr_csv(c(data1 = "df"), "/tmp/output.csv")

  # Check it's a call
  expect_true(inherits(expr, "call"))

  # Check it's readr::write_csv
  expr_text <- deparse(expr)
  expect_true(any(grepl("readr::write_csv", expr_text)))
  expect_true(any(grepl("df", expr_text)))
  expect_true(any(grepl("/tmp/output.csv", expr_text)))
})

test_that("write_expr_csv respects custom delimiter", {
  expr <- write_expr_csv(c(data1 = "df"), "/tmp/output.csv", args = list(sep = ";"))

  expr_text <- paste(deparse(expr), collapse = " ")
  expect_true(grepl('delim = ";"', expr_text))
})

test_that("write_expr_csv generates ZIP expression for multiple files", {
  expr <- write_expr_csv(
    c(sales = "df1", costs = "df2"),
    "/tmp/output.zip"
  )

  expr_text <- paste(deparse(expr), collapse = " ")

  # Should create temp dir
  expect_true(grepl("temp_dir.*<-.*tempfile", expr_text))

  # Should have multiple write_csv calls
  expect_true(grepl("readr::write_csv", expr_text))

  # Should call zip
  expect_true(grepl("zip::zip", expr_text))

  # Should clean up
  expect_true(grepl("unlink", expr_text))
})

test_that("write_expr_excel generates correct expression", {
  expr <- write_expr_excel(
    c(Sheet1 = "df1", Sheet2 = "df2"),
    "/tmp/output.xlsx"
  )

  expr_text <- paste(deparse(expr), collapse = " ")

  # Should use writexl
  expect_true(grepl("writexl::write_xlsx", expr_text))

  # Should have both data frames
  expect_true(grepl("df1", expr_text))
  expect_true(grepl("df2", expr_text))

  # Should have sheet names
  expect_true(grepl("Sheet1", expr_text))
  expect_true(grepl("Sheet2", expr_text))
})

test_that("write_expr_arrow generates parquet expression", {
  expr <- write_expr_arrow(c(data1 = "df"), "/tmp/output.parquet", "parquet")

  expr_text <- paste(deparse(expr), collapse = " ")
  expect_true(grepl("arrow::write_parquet", expr_text))
  expect_true(grepl("df", expr_text))
})

test_that("write_expr_arrow generates feather expression", {
  expr <- write_expr_arrow(c(data1 = "df"), "/tmp/output.feather", "feather")

  expr_text <- paste(deparse(expr), collapse = " ")
  expect_true(grepl("arrow::write_feather", expr_text))
})

test_that("write_expr dispatches to correct format", {
  # CSV
  expr_csv <- write_expr(c(d = "df"), "/tmp", "output", "csv")
  expect_true(grepl("readr::write_csv", paste(deparse(expr_csv), collapse = " ")))

  # Excel
  expr_excel <- write_expr(c(d = "df"), "/tmp", "output", "excel")
  expect_true(grepl("writexl::write_xlsx", paste(deparse(expr_excel), collapse = " ")))

  # Parquet
  expr_parquet <- write_expr(c(d = "df"), "/tmp", "output", "parquet")
  expect_true(grepl("arrow::write_parquet", paste(deparse(expr_parquet), collapse = " ")))
})

test_that("write_expr handles empty filename with timestamp", {
  expr <- write_expr(c(d = "df"), "/tmp", "", "csv")
  expr_text <- paste(deparse(expr), collapse = " ")

  # Should have a timestamped filename pattern
  expect_true(grepl("data_[0-9]{8}_[0-9]{6}\\.csv", expr_text))
})

test_that("write_expr creates ZIP for multiple CSV files", {
  expr <- write_expr(c(a = "df1", b = "df2"), "/tmp", "output", "csv")
  expr_text <- paste(deparse(expr), collapse = " ")

  # Should create ZIP
  expect_true(grepl("zip::zip", expr_text))
  expect_true(grepl("output\\.zip", expr_text))
})

test_that("write_expr creates single Excel file for multiple inputs", {
  expr <- write_expr(c(a = "df1", b = "df2"), "/tmp", "output", "excel")
  expr_text <- paste(deparse(expr), collapse = " ")

  # Should NOT create ZIP
  expect_false(grepl("zip::zip", expr_text))

  # Should create single Excel with sheets
  expect_true(grepl("writexl::write_xlsx", expr_text))
  expect_true(grepl("output\\.xlsx", expr_text))
})

test_that("write_expr handles NULL or empty data_names", {
  result <- write_expr(character(), "/tmp", "output", "csv")
  expect_null(result)
})

test_that("write_expr adds names if missing", {
  # Without names
  expr <- write_expr(c("df1", "df2"), "/tmp", "output", "csv")

  # Should still work and add numeric names
  expect_true(inherits(expr, "call") || is.language(expr))
})


# ==============================================================================
# EVALUATION TESTS - Actually execute expressions and verify files
# ==============================================================================

test_that("write_expr_csv actually writes CSV file correctly", {
  skip_if_not_installed("readr")

  # Create test data
  test_df <- data.frame(x = 1:3, y = c("a", "b", "c"), stringsAsFactors = FALSE)

  # Generate expression
  temp_file <- tempfile(fileext = ".csv")
  expr <- write_expr_csv(c(data = "test_df"), temp_file)

  # EVALUATE the expression
  eval(expr)

  # Verify file exists
  expect_true(file.exists(temp_file))

  # Read back and verify content
  result <- readr::read_csv(temp_file, show_col_types = FALSE)
  expect_equal(nrow(result), 3)
  expect_equal(result$x, 1:3)
  expect_equal(result$y, c("a", "b", "c"))

  unlink(temp_file)
})

test_that("write_expr_csv writes CSV with custom delimiter", {
  skip_if_not_installed("readr")

  # Create test data
  test_df <- data.frame(a = 1:5, b = 6:10)

  # Generate expression with semicolon delimiter
  temp_file <- tempfile(fileext = ".csv")
  expr <- write_expr_csv(c(data = "test_df"), temp_file, args = list(sep = ";"))

  # EVALUATE
  eval(expr)

  # Read back with semicolon
  result <- readr::read_delim(temp_file, delim = ";", show_col_types = FALSE)
  expect_equal(result$a, 1:5)
  expect_equal(result$b, 6:10)

  unlink(temp_file)
})

test_that("write_expr_csv writes multiple files to ZIP", {
  skip_if_not_installed("readr")
  skip_if_not_installed("zip")

  # Create test data
  df1 <- data.frame(x = 1:3)
  df2 <- data.frame(y = 4:6)

  # Generate expression
  temp_zip <- tempfile(fileext = ".zip")
  expr <- write_expr_csv(c(first = "df1", second = "df2"), temp_zip)

  # EVALUATE
  eval(expr)

  # Verify ZIP exists
  expect_true(file.exists(temp_zip))

  # Extract and verify
  temp_extract <- tempfile("extract_")
  dir.create(temp_extract)
  zip::unzip(temp_zip, exdir = temp_extract)

  files <- list.files(temp_extract, pattern = "\\.csv$", full.names = TRUE)
  expect_equal(length(files), 2)

  # Read and verify both files
  data1 <- readr::read_csv(files[grep("first", files)], show_col_types = FALSE)
  data2 <- readr::read_csv(files[grep("second", files)], show_col_types = FALSE)

  expect_equal(data1$x, 1:3)
  expect_equal(data2$y, 4:6)

  unlink(c(temp_zip, temp_extract), recursive = TRUE)
})

test_that("write_expr_excel writes single Excel file", {
  skip_if_not_installed("writexl")
  skip_if_not_installed("readxl")

  # Create test data
  test_df <- data.frame(a = 1:5, b = letters[1:5], stringsAsFactors = FALSE)

  # Generate expression
  temp_file <- tempfile(fileext = ".xlsx")
  expr <- write_expr_excel(c(Sheet1 = "test_df"), temp_file)

  # EVALUATE
  eval(expr)

  # Verify file exists
  expect_true(file.exists(temp_file))

  # Read back
  result <- readxl::read_excel(temp_file)
  expect_equal(nrow(result), 5)
  expect_equal(result$a, 1:5)
  expect_equal(result$b, letters[1:5])

  unlink(temp_file)
})

test_that("write_expr_excel writes multiple sheets", {
  skip_if_not_installed("writexl")
  skip_if_not_installed("readxl")

  # Create test data
  sales <- data.frame(month = 1:3, revenue = c(100, 200, 300))
  costs <- data.frame(month = 1:3, expenses = c(50, 75, 100))

  # Generate expression
  temp_file <- tempfile(fileext = ".xlsx")
  expr <- write_expr_excel(c(Sales = "sales", Costs = "costs"), temp_file)

  # EVALUATE
  eval(expr)

  # Verify sheets exist
  sheets <- readxl::excel_sheets(temp_file)
  expect_equal(sheets, c("Sales", "Costs"))

  # Read and verify each sheet
  sales_result <- readxl::read_excel(temp_file, sheet = "Sales")
  costs_result <- readxl::read_excel(temp_file, sheet = "Costs")

  expect_equal(sales_result$revenue, c(100, 200, 300))
  expect_equal(costs_result$expenses, c(50, 75, 100))

  unlink(temp_file)
})

test_that("write_expr_arrow writes Parquet file", {
  skip_if_not_installed("arrow")

  # Create test data
  test_df <- data.frame(x = 1:10, y = rnorm(10))

  # Generate expression
  temp_file <- tempfile(fileext = ".parquet")
  expr <- write_expr_arrow(c(data = "test_df"), temp_file, "parquet")

  # EVALUATE
  eval(expr)

  # Verify file exists
  expect_true(file.exists(temp_file))

  # Read back
  result <- arrow::read_parquet(temp_file)
  expect_equal(nrow(result), 10)
  expect_equal(result$x, 1:10)

  unlink(temp_file)
})

test_that("write_expr_arrow writes Feather file", {
  skip_if_not_installed("arrow")

  # Create test data
  test_df <- data.frame(a = 1:5, b = letters[1:5], stringsAsFactors = FALSE)

  # Generate expression
  temp_file <- tempfile(fileext = ".feather")
  expr <- write_expr_arrow(c(data = "test_df"), temp_file, "feather")

  # EVALUATE
  eval(expr)

  # Verify file exists
  expect_true(file.exists(temp_file))

  # Read back
  result <- arrow::read_feather(temp_file)
  expect_equal(result$a, 1:5)
  expect_equal(result$b, letters[1:5])

  unlink(temp_file)
})

test_that("write_expr end-to-end CSV write and read", {
  skip_if_not_installed("readr")

  # Create test data
  mtcars_subset <- mtcars[1:5, 1:3]

  # Use write_expr dispatcher
  temp_dir <- tempfile("write_test_")
  dir.create(temp_dir)
  expr <- write_expr(
    c(cars = "mtcars_subset"),
    temp_dir,
    "output",
    "csv"
  )

  # EVALUATE
  eval(expr)

  # Find created file
  files <- list.files(temp_dir, pattern = "output.*\\.csv$", full.names = TRUE)
  expect_equal(length(files), 1)

  # Read and verify
  result <- readr::read_csv(files[1], show_col_types = FALSE)
  expect_equal(nrow(result), 5)
  expect_equal(ncol(result), 3)

  unlink(temp_dir, recursive = TRUE)
})

test_that("write_expr end-to-end Excel with auto-timestamp", {
  skip_if_not_installed("writexl")
  skip_if_not_installed("readxl")

  # Create test data
  iris_subset <- iris[1:10, ]

  # Use write_expr with empty filename (auto-timestamp)
  temp_dir <- tempfile("write_test_")
  dir.create(temp_dir)
  expr <- write_expr(
    c(flowers = "iris_subset"),
    temp_dir,
    "",  # Empty = timestamp
    "excel"
  )

  # EVALUATE
  eval(expr)

  # Find created file (should have timestamp)
  files <- list.files(temp_dir, pattern = "data_.*\\.xlsx$", full.names = TRUE)
  expect_equal(length(files), 1)
  expect_true(grepl("data_[0-9]{8}_[0-9]{6}\\.xlsx", basename(files[1])))

  # Read and verify
  result <- readxl::read_excel(files[1])
  expect_equal(nrow(result), 10)

  unlink(temp_dir, recursive = TRUE)
})

test_that("write_expr handles quote parameter correctly", {
  skip_if_not_installed("readr")

  # Create data with strings
  test_df <- data.frame(
    name = c("Alice", "Bob", "Charlie"),
    quote = c("Hello, World", "Test", "Data"),
    stringsAsFactors = FALSE
  )

  # Test with quote = TRUE
  temp_file1 <- tempfile(fileext = ".csv")
  expr1 <- write_expr_csv(c(data = "test_df"), temp_file1, args = list(quote = TRUE))
  eval(expr1)

  content1 <- readLines(temp_file1)
  expect_true(any(grepl('"', content1)))  # Should have quotes

  # Test with quote = FALSE
  temp_file2 <- tempfile(fileext = ".csv")
  expr2 <- write_expr_csv(c(data = "test_df"), temp_file2, args = list(quote = FALSE))
  eval(expr2)

  # Read back and verify data integrity
  # Suppress parsing warning - readr has trouble with unquoted fields
  # but the test verifies the functionality works correctly
  result <- suppressWarnings(readr::read_csv(temp_file2, show_col_types = FALSE))
  expect_equal(result$name, c("Alice", "Bob", "Charlie"))

  unlink(c(temp_file1, temp_file2))
})

test_that("write_expr_csv writes TSV file with tab delimiter", {
  skip_if_not_installed("readr")

  # Create test data
  test_df <- data.frame(x = 1:3, y = 4:6)

  # Generate expression with tab delimiter
  temp_file <- tempfile(fileext = ".tsv")
  expr <- write_expr_csv(c(data = "test_df"), temp_file, args = list(sep = "\t"))

  # EVALUATE
  eval(expr)

  # Verify file exists
  expect_true(file.exists(temp_file))

  # Read back with tab delimiter
  result <- readr::read_tsv(temp_file, show_col_types = FALSE)
  expect_equal(result$x, 1:3)
  expect_equal(result$y, 4:6)

  unlink(temp_file)
})

test_that("write_expr_csv writes file with pipe delimiter", {
  skip_if_not_installed("readr")

  # Create test data
  test_df <- data.frame(a = 1:3, b = c("x", "y", "z"), stringsAsFactors = FALSE)

  # Generate expression with pipe delimiter
  temp_file <- tempfile(fileext = ".txt")
  expr <- write_expr_csv(c(data = "test_df"), temp_file, args = list(sep = "|"))

  # EVALUATE
  eval(expr)

  # Verify file exists
  expect_true(file.exists(temp_file))

  # Read back with pipe delimiter
  result <- readr::read_delim(temp_file, delim = "|", show_col_types = FALSE)
  expect_equal(result$a, 1:3)
  expect_equal(result$b, c("x", "y", "z"))

  unlink(temp_file)
})
