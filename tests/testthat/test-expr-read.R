test_that("read_expr generates correct single CSV expression", {
  expr <- read_expr(
    paths = "data.csv",
    file_type = "csv",
    sep = ",",
    col_names = TRUE,
    skip = 0
  )

  # Check it's a call to readr::read_csv
  expect_equal(as.character(expr[[1]]), c("::", "readr", "read_csv"))
  expect_equal(expr$file, "data.csv")
  expect_true(expr$col_names)
})

test_that("read_expr generates rbind for multiple CSV files", {
  expr <- read_expr(
    paths = c("data1.csv", "data2.csv"),
    file_type = "csv",
    combine = "rbind",
    sep = ","
  )

  # Check it's rbind(readr::read_csv(...), readr::read_csv(...))
  expect_equal(expr[[1]], quote(rbind))
  expect_length(expr, 3) # rbind + 2 file expressions
})

test_that("read_expr actually reads single CSV file", {
  # Create temp CSV
  temp_file <- tempfile(fileext = ".csv")
  write.csv(data.frame(x = 1:3, y = 4:6), temp_file, row.names = FALSE)

  # Generate expression
  expr <- read_expr(
    paths = temp_file,
    file_type = "csv",
    sep = ",",
    col_names = TRUE
  )

  # ACTUALLY EVALUATE IT
  result <- eval(expr)

  # Verify it worked
  expect_s3_class(result, "tbl_df") # tibble from readr
  expect_equal(nrow(result), 3)
  expect_equal(ncol(result), 2)
  expect_equal(result$x, 1:3)
  expect_equal(result$y, 4:6)

  unlink(temp_file)
})

test_that("read_expr rbinds multiple CSV files correctly", {
  # Create 2 temp CSVs
  temp1 <- tempfile(fileext = ".csv")
  temp2 <- tempfile(fileext = ".csv")

  write.csv(data.frame(x = 1:2, y = 3:4), temp1, row.names = FALSE)
  write.csv(data.frame(x = 5:6, y = 7:8), temp2, row.names = FALSE)

  # Generate rbind expression
  expr <- read_expr(
    paths = c(temp1, temp2),
    file_type = "csv",
    combine = "rbind",
    sep = ","
  )

  # EVALUATE IT
  result <- eval(expr)

  # Should have 4 rows (2 + 2)
  expect_equal(nrow(result), 4)
  expect_equal(result$x, c(1:2, 5:6))
  expect_equal(result$y, c(3:4, 7:8))

  unlink(c(temp1, temp2))
})

test_that("read_expr auto strategy falls back on incompatible files", {
  temp1 <- tempfile(fileext = ".csv")
  temp2 <- tempfile(fileext = ".csv")

  # Different columns - can't rbind
  write.csv(data.frame(x = 1:2), temp1, row.names = FALSE)
  write.csv(data.frame(y = 3:4), temp2, row.names = FALSE)

  expr <- read_expr(
    paths = c(temp1, temp2),
    file_type = "csv",
    combine = "auto" # Should try rbind, fall back to first
  )

  # EVALUATE - should return just first file
  result <- eval(expr)

  expect_equal(nrow(result), 2)
  expect_true("x" %in% names(result))
  expect_false("y" %in% names(result))

  unlink(c(temp1, temp2))
})

test_that("read_expr handles TSV files", {
  temp_tsv <- tempfile(fileext = ".tsv")
  write.table(
    data.frame(a = 1:3, b = 4:6),
    temp_tsv,
    sep = "\t",
    row.names = FALSE,
    quote = FALSE
  )

  expr <- read_expr(
    paths = temp_tsv,
    file_type = "csv",
    sep = "\t"
  )

  # Check it uses read_tsv
  expect_equal(as.character(expr[[1]]), c("::", "readr", "read_tsv"))

  # Evaluate it
  result <- eval(expr)
  expect_equal(nrow(result), 3)
  expect_equal(names(result), c("a", "b"))

  unlink(temp_tsv)
})

test_that("read_expr handles custom delimiters", {
  temp_file <- tempfile(fileext = ".txt")
  write.table(
    data.frame(x = 1:3),
    temp_file,
    sep = ";",
    row.names = FALSE,
    quote = FALSE
  )

  expr <- read_expr(
    paths = temp_file,
    file_type = "csv",
    sep = ";"
  )

  # Check it uses read_delim
  expect_equal(as.character(expr[[1]]), c("::", "readr", "read_delim"))

  # Evaluate it
  result <- eval(expr)
  expect_equal(nrow(result), 3)

  unlink(temp_file)
})

test_that("read_expr handles Excel files", {
  skip_if_not_installed("readxl")
  skip_if_not_installed("writexl")

  # Create temp Excel file
  temp_xlsx <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(data.frame(a = 1:3, b = 4:6), temp_xlsx)

  expr <- read_expr(
    paths = temp_xlsx,
    file_type = "excel",
    col_names = TRUE
  )

  # Check structure
  expect_equal(as.character(expr[[1]]), c("::", "readxl", "read_excel"))

  # Evaluate it
  result <- eval(expr)
  expect_equal(nrow(result), 3)
  expect_equal(names(result), c("a", "b"))

  unlink(temp_xlsx)
})

test_that("read_expr handles parquet files", {
  skip_if_not_installed("arrow")

  temp_parquet <- tempfile(fileext = ".parquet")
  arrow::write_parquet(data.frame(x = 1:5, y = 6:10), temp_parquet)

  expr <- read_expr(
    paths = temp_parquet,
    file_type = "arrow"
  )

  # Check structure
  expect_equal(as.character(expr[[1]]), c("::", "arrow", "read_parquet"))

  # Evaluate it
  result <- eval(expr)
  expect_equal(nrow(result), 5)

  unlink(temp_parquet)
})

test_that("read_expr cbinds files correctly", {
  temp1 <- tempfile(fileext = ".csv")
  temp2 <- tempfile(fileext = ".csv")

  write.csv(data.frame(x = 1:3), temp1, row.names = FALSE)
  write.csv(data.frame(y = 4:6), temp2, row.names = FALSE)

  expr <- read_expr(
    paths = c(temp1, temp2),
    file_type = "csv",
    combine = "cbind"
  )

  # Evaluate it
  result <- eval(expr)

  # Should have 3 rows, 2 columns
  expect_equal(nrow(result), 3)
  expect_equal(ncol(result), 2)
  expect_true("x" %in% names(result))
  expect_true("y" %in% names(result))

  unlink(c(temp1, temp2))
})

test_that("read_expr first strategy uses only first file", {
  temp1 <- tempfile(fileext = ".csv")
  temp2 <- tempfile(fileext = ".csv")

  write.csv(data.frame(x = 1:2), temp1, row.names = FALSE)
  write.csv(data.frame(x = 99:100), temp2, row.names = FALSE)

  expr <- read_expr(
    paths = c(temp1, temp2),
    file_type = "csv",
    combine = "first"
  )

  # Evaluate it
  result <- eval(expr)

  # Should only have data from first file
  expect_equal(nrow(result), 2)
  expect_equal(result$x, 1:2)

  unlink(c(temp1, temp2))
})

test_that("read_expr handles empty paths", {
  expr <- read_expr(
    paths = character(),
    file_type = "csv"
  )

  expect_null(expr)
})

test_that("read_expr respects CSV parameters", {
  temp_file <- tempfile(fileext = ".csv")
  write.csv(data.frame(a = 1:10), temp_file, row.names = FALSE)

  expr <- read_expr(
    paths = temp_file,
    file_type = "csv",
    skip = 0,
    n_max = 5 # Read only 5 rows
  )

  result <- eval(expr)

  # Should have 5 rows
  expect_equal(nrow(result), 5)
  expect_equal(result$a, 1:5)

  unlink(temp_file)
})
