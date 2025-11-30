# Tests for read_block using blockr.core framework evaluation
# These tests verify the actual framework behavior, not manual eval()

test_that("read_block returns correct CSV data via framework", {

  temp_csv <- tempfile(fileext = ".csv")
  write.csv(
    data.frame(x = 1:5, y = letters[1:5]),
    temp_csv,
    row.names = FALSE
  )

  block <- new_read_block(path = temp_csv)

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_true(is.data.frame(result))
      expect_equal(nrow(result), 5)
      expect_equal(ncol(result), 2)
      expect_equal(result$x, 1:5)
      expect_equal(result$y, letters[1:5])
    },
    args = list(x = block, data = list())
  )

  unlink(temp_csv)
})

test_that("read_block rbinds multiple CSV files via framework", {
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

  block <- new_read_block(
    path = c(temp_csv1, temp_csv2),
    combine = "rbind"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_true(is.data.frame(result))
      expect_equal(nrow(result), 6)
      expect_equal(result$id, 1:6)
      expect_equal(result$value, c(10, 20, 30, 40, 50, 60))
    },
    args = list(x = block, data = list())
  )

  unlink(c(temp_csv1, temp_csv2))
})

test_that("read_block respects custom delimiter via framework", {
  temp_csv <- tempfile(fileext = ".csv")
  write.table(
    data.frame(a = 1:5, b = 6:10),
    temp_csv,
    sep = ";",
    row.names = FALSE,
    quote = FALSE
  )

  block <- new_read_block(
    path = temp_csv,
    args = list(sep = ";")
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_true(is.data.frame(result))
      expect_equal(nrow(result), 5)
      expect_equal(ncol(result), 2)
      expect_equal(result$a, 1:5)
      expect_equal(result$b, 6:10)
    },
    args = list(x = block, data = list())
  )

  unlink(temp_csv)
})

test_that("read_block handles Excel files via framework", {
  skip_if_not_installed("writexl")
  skip_if_not_installed("readxl")

  temp_xlsx <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(
    list(Sheet1 = data.frame(x = 1:5), Sheet2 = data.frame(y = 6:10)),
    temp_xlsx
  )

  block <- new_read_block(
    path = temp_xlsx,
    args = list(sheet = "Sheet2")
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_true(is.data.frame(result))
      expect_equal(nrow(result), 5)
      expect_true("y" %in% names(result))
      expect_equal(result$y, 6:10)
    },
    args = list(x = block, data = list())
  )

  unlink(temp_xlsx)
})

test_that("read_block handles cbind combine via framework", {
  temp_csv1 <- tempfile(fileext = ".csv")
  temp_csv2 <- tempfile(fileext = ".csv")

  write.csv(data.frame(a = 1:5), temp_csv1, row.names = FALSE)
  write.csv(data.frame(b = 6:10), temp_csv2, row.names = FALSE)

  block <- new_read_block(
    path = c(temp_csv1, temp_csv2),
    combine = "cbind"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_equal(nrow(result), 5)
      expect_equal(ncol(result), 2)
      expect_true("a" %in% names(result))
      expect_true("b" %in% names(result))
      expect_equal(result$a, 1:5)
      expect_equal(result$b, 6:10)
    },
    args = list(x = block, data = list())
  )

  unlink(c(temp_csv1, temp_csv2))
})

test_that("read_block handles first combine via framework", {
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

  block <- new_read_block(
    path = c(temp_csv1, temp_csv2),
    combine = "first"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_equal(nrow(result), 3)
      expect_equal(result$x, 1:3)
      expect_equal(result$y, c("a", "b", "c"))
    },
    args = list(x = block, data = list())
  )

  unlink(c(temp_csv1, temp_csv2))
})

test_that("read_block respects n_max parameter via framework", {
  temp_csv <- tempfile(fileext = ".csv")
  write.csv(
    data.frame(id = 1:100, value = rnorm(100)),
    temp_csv,
    row.names = FALSE
  )

  block <- new_read_block(
    path = temp_csv,
    args = list(n_max = 10)
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_equal(nrow(result), 10)
      expect_equal(result$id, 1:10)
    },
    args = list(x = block, data = list())
  )

  unlink(temp_csv)
})

test_that("read_block handles TSV files via framework", {
  temp_tsv <- tempfile(fileext = ".tsv")
  write.table(
    data.frame(name = c("Alice", "Bob", "Charlie"), age = c(25, 30, 35)),
    temp_tsv,
    sep = "\t",
    row.names = FALSE,
    quote = FALSE
  )

  block <- new_read_block(
    path = temp_tsv,
    args = list(sep = "\t")
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_equal(nrow(result), 3)
      expect_equal(result$name, c("Alice", "Bob", "Charlie"))
      expect_equal(result$age, c(25, 30, 35))
    },
    args = list(x = block, data = list())
  )

  unlink(temp_tsv)
})

test_that("read_block handles Parquet files via framework", {
  skip_if_not_installed("arrow")

  temp_parquet <- tempfile(fileext = ".parquet")
  arrow::write_parquet(
    data.frame(x = 1:50, y = rnorm(50)),
    temp_parquet
  )

  block <- new_read_block(path = temp_parquet)

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_equal(nrow(result), 50)
      expect_equal(result$x, 1:50)
    },
    args = list(x = block, data = list())
  )

  unlink(temp_parquet)
})

test_that("read_block handles Feather files via framework", {
  skip_if_not_installed("arrow")

  temp_feather <- tempfile(fileext = ".feather")
  arrow::write_feather(
    data.frame(id = 1:30, category = rep(c("A", "B", "C"), 10)),
    temp_feather
  )

  block <- new_read_block(path = temp_feather)

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_equal(nrow(result), 30)
      expect_equal(unique(result$category), c("A", "B", "C"))
    },
    args = list(x = block, data = list())
  )

  unlink(temp_feather)
})

test_that("read_block handles URL source via framework", {
  skip_on_cran()

  url <- "https://raw.githubusercontent.com/datasets/co2-ppm/master/data/co2-mm-mlo.csv"

  block <- new_read_block(
    path = url,
    source = "url"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_true(is.data.frame(result))
      expect_true(nrow(result) > 0)
    },
    args = list(x = block, data = list())
  )
})

test_that("read_block auto-detects file type correctly", {
  # CSV

  temp_csv <- tempfile(fileext = ".csv")
  write.csv(data.frame(x = 1:3), temp_csv, row.names = FALSE)

  block <- new_read_block(path = temp_csv)

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      expect_true(is.data.frame(result))
      expect_equal(result$x, 1:3)
    },
    args = list(x = block, data = list())
  )

  unlink(temp_csv)
})

test_that("read_block handles empty path gracefully", {
  block <- new_read_block(path = character())

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      # Empty path should return NULL or empty data frame
      expect_true(is.null(result) || (is.data.frame(result) && nrow(result) == 0))
    },
    args = list(x = block, data = list())
  )
})
