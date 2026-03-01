test_that("read_expr handles empty paths", {
  expr <- read_expr(
    paths = character(),
    file_type = "csv"
  )

  expect_null(expr)
})

test_that("read_file_expr errors on invalid input", {
  expect_error(read_file_expr(""))
  expect_error(read_file_expr(123))
  expect_error(read_file_expr(c("a.csv", "b.csv")))
})
