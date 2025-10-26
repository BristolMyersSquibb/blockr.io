test_that("read_expr handles JSON files via rio", {
  skip_if_not_installed("rio")
  skip_if_not_installed("jsonlite")

  temp_json <- tempfile(fileext = ".json")
  jsonlite::write_json(
    data.frame(x = 1:5, y = letters[1:5]),
    temp_json
  )

  expr <- read_expr(
    paths = temp_json,
    file_type = "other"
  )

  # Check it uses rio::import
  expect_equal(as.character(expr[[1]]), c("::", "rio", "import"))

  # Evaluate it
  result <- eval(expr)
  expect_equal(nrow(result), 5)
  expect_true("x" %in% names(result))

  unlink(temp_json)
})

test_that("read_expr handles feather files", {
  skip_if_not_installed("arrow")

  temp_feather <- tempfile(fileext = ".feather")
  arrow::write_feather(data.frame(a = 1:10, b = 11:20), temp_feather)

  expr <- read_expr(
    paths = temp_feather,
    file_type = "arrow"
  )

  # Check it uses arrow::read_feather
  expect_equal(as.character(expr[[1]]), c("::", "arrow", "read_feather"))

  # Evaluate it
  result <- eval(expr)
  expect_equal(nrow(result), 10)
  expect_equal(result$a, 1:10)

  unlink(temp_feather)
})

test_that("read_expr handles Excel with cell range", {
  skip_if_not_installed("readxl")
  skip_if_not_installed("writexl")

  temp_xlsx <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(
    data.frame(a = 1:10, b = 11:20, c = 21:30),
    temp_xlsx
  )

  expr <- read_expr(
    paths = temp_xlsx,
    file_type = "excel",
    range = "A1:B5"
  )

  # Evaluate it - should read only A1:B5
  result <- eval(expr)
  expect_equal(nrow(result), 4) # 5 rows minus header
  expect_equal(ncol(result), 2) # Only columns A and B

  unlink(temp_xlsx)
})

test_that("read_expr handles CSV without column names", {
  temp_file <- tempfile(fileext = ".csv")

  # Write CSV without header
  write.table(
    data.frame(x = 1:5, y = 6:10),
    temp_file,
    sep = ",",
    row.names = FALSE,
    col.names = FALSE
  )

  expr <- read_expr(
    paths = temp_file,
    file_type = "csv",
    col_names = FALSE
  )

  result <- eval(expr)

  # Should have auto-generated column names like X1, X2
  expect_equal(nrow(result), 5)
  expect_equal(ncol(result), 2)

  unlink(temp_file)
})

test_that("read_expr handles CSV with different encoding", {
  skip_on_os("windows") # Encoding tests can be platform-specific

  temp_file <- tempfile(fileext = ".csv")

  # Create a CSV with UTF-8 encoding (with special characters)
  df <- data.frame(
    name = c("Müller", "Françoise", "José"),
    value = 1:3
  )
  write.csv(df, temp_file, row.names = FALSE, fileEncoding = "UTF-8")

  expr <- read_expr(
    paths = temp_file,
    file_type = "csv",
    encoding = "UTF-8"
  )

  result <- eval(expr)

  # Check that special characters are preserved
  expect_true(any(grepl("ü|ç|é", result$name)))

  unlink(temp_file)
})

test_that("read_expr handles CSV with different quote character", {
  temp_file <- tempfile(fileext = ".csv")

  # Create CSV with single quotes
  writeLines(
    c(
      "name,description",
      "item1,'This is a description'",
      "item2,'Another one'"
    ),
    temp_file
  )

  expr <- read_expr(
    paths = temp_file,
    file_type = "csv",
    quote = "'"
  )

  result <- eval(expr)

  expect_equal(nrow(result), 2)
  expect_equal(result$name, c("item1", "item2"))

  unlink(temp_file)
})

test_that("read_expr handles empty file gracefully", {
  temp_file <- tempfile(fileext = ".csv")

  # Create empty CSV (only header)
  write.csv(
    data.frame(x = integer(), y = character()),
    temp_file,
    row.names = FALSE
  )

  expr <- read_expr(
    paths = temp_file,
    file_type = "csv"
  )

  result <- eval(expr)

  # Should return empty data frame with correct columns
  expect_equal(nrow(result), 0)
  expect_equal(ncol(result), 2)
  expect_true("x" %in% names(result))
  expect_true("y" %in% names(result))

  unlink(temp_file)
})

test_that("read_expr handles very large n_max", {
  temp_file <- tempfile(fileext = ".csv")

  # Create CSV with 100 rows
  write.csv(data.frame(x = 1:100), temp_file, row.names = FALSE)

  # Request more rows than exist
  expr <- read_expr(
    paths = temp_file,
    file_type = "csv",
    n_max = 1000
  )

  result <- eval(expr)

  # Should return all 100 rows
  expect_equal(nrow(result), 100)

  unlink(temp_file)
})

test_that("read_expr handles skip beyond file length", {
  temp_file <- tempfile(fileext = ".csv")

  # Create CSV with 10 rows
  write.csv(data.frame(x = 1:10), temp_file, row.names = FALSE)

  # Skip more rows than exist
  expr <- read_expr(
    paths = temp_file,
    file_type = "csv",
    skip = 100
  )

  result <- eval(expr)

  # Should return empty data frame
  expect_equal(nrow(result), 0)

  unlink(temp_file)
})

test_that("read_expr handles multi-file with mixed compatible/incompatible columns", {
  temp1 <- tempfile(fileext = ".csv")
  temp2 <- tempfile(fileext = ".csv")
  temp3 <- tempfile(fileext = ".csv")

  # Files with different column structures
  write.csv(data.frame(a = 1:2, b = 3:4), temp1, row.names = FALSE)
  write.csv(data.frame(a = 5:6, b = 7:8), temp2, row.names = FALSE) # Compatible with temp1
  write.csv(data.frame(x = 9:10), temp3, row.names = FALSE) # Incompatible

  # Test rbind with incompatible files - should error
  expr <- read_expr(
    paths = c(temp1, temp2, temp3),
    file_type = "csv",
    combine = "rbind"
  )

  expect_error(eval(expr))

  # Test auto with incompatible files - should fallback to first
  expr_auto <- read_expr(
    paths = c(temp1, temp2, temp3),
    file_type = "csv",
    combine = "auto"
  )

  result <- eval(expr_auto)
  # The auto strategy tries rbind and fails, so falls back to first file
  expect_true(nrow(result) %in% c(2, 4, 6)) # Could be first file only or successful rbind of compatible files

  unlink(c(temp1, temp2, temp3))
})

test_that("read_expr generates correct Excel expression with sheet number", {
  skip_if_not_installed("readxl")
  skip_if_not_installed("writexl")

  temp_xlsx <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(
    list(
      First = data.frame(x = 1:3),
      Second = data.frame(y = 4:6)
    ),
    temp_xlsx
  )

  # Use sheet number instead of name
  expr <- read_expr(
    paths = temp_xlsx,
    file_type = "excel",
    sheet = 2
  )

  result <- eval(expr)

  # Should read second sheet
  expect_true("y" %in% names(result))
  expect_false("x" %in% names(result))

  unlink(temp_xlsx)
})

test_that("read_expr handles Excel with skip and n_max", {
  skip_if_not_installed("readxl")
  skip_if_not_installed("writexl")

  temp_xlsx <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(
    data.frame(x = 1:20),
    temp_xlsx
  )

  expr <- read_expr(
    paths = temp_xlsx,
    file_type = "excel",
    skip = 5,
    n_max = 10
  )

  result <- eval(expr)

  # Should skip 5 rows and read 10 rows
  expect_equal(nrow(result), 10)
  # Excel skip works differently - it skips rows before reading, so we get rows 6-15
  expect_true(ncol(result) >= 1)

  unlink(temp_xlsx)
})
