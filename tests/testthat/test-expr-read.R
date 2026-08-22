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

# These expressions are exported into scripts and report chunks, so a call
# that restates every default is the block's output reading as machine
# output. Each option appears only when it is set to something the reader
# would not have done anyway.

test_that("an unconfigured csv read exports as the call you would type", {
  expect_equal(
    deparse1(read_expr_csv("data.csv")),
    'readr::read_csv("data.csv", show_col_types = FALSE)'
  )
  expect_equal(
    deparse1(read_expr_excel("book.xlsx")),
    'readxl::read_excel("book.xlsx")'
  )
})

test_that("a non-default option appears, a default one does not", {
  expect_match(deparse1(read_expr_csv("d.csv", skip = 2)), "skip = 2",
               fixed = TRUE)
  expect_false(grepl("skip", deparse1(read_expr_csv("d.csv", skip = 0))))

  expect_match(deparse1(read_expr_csv("d.csv", col_names = FALSE)),
               "col_names = FALSE", fixed = TRUE)
  expect_false(grepl("col_names",
                     deparse1(read_expr_csv("d.csv", col_names = TRUE))))

  # readr's own locale is UTF-8, so naming it adds a call that does nothing
  expect_false(grepl("locale", deparse1(read_expr_csv("d.csv"))))
  expect_match(deparse1(read_expr_csv("d.csv", encoding = "Latin-1")),
               'readr::locale(encoding = "Latin-1")', fixed = TRUE)
})

test_that("0 and 0L are the same instruction to the reader", {
  expect_false(grepl("skip", deparse1(read_expr_csv("d.csv", skip = 0L))))
  expect_false(grepl("n_max", deparse1(read_expr_csv("d.csv", n_max = Inf))))
})

test_that("the delimiter picks the function and is kept where required", {
  expect_match(deparse1(read_expr_csv("d.tsv", sep = "\t")), "read_tsv",
               fixed = TRUE)
  delim <- deparse1(read_expr_csv("d.csv", sep = ";"))
  expect_match(delim, "read_delim", fixed = TRUE)
  expect_match(delim, 'delim = ";"', fixed = TRUE)
})

test_that("pruning does not change what the expression reads", {
  path <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(data.frame(a = 1:3, b = c("x", "y", "z")), path,
                   row.names = FALSE)

  verbose <- bquote(readr::read_csv(
    file = .(path), col_names = TRUE, skip = 0, n_max = Inf, quote = "\"",
    locale = readr::locale(encoding = "UTF-8"), show_col_types = FALSE
  ))
  expect_equal(
    as.data.frame(eval(read_expr_csv(path))),
    as.data.frame(eval(verbose))
  )
})
