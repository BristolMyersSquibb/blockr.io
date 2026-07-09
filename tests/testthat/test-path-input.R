# Tests for path input widget

test_that("path_input_dep returns htmlDependency object", {
  dep <- blockr.io:::path_input_dep()

  expect_s3_class(dep, "html_dependency")
  expect_equal(dep$name, "blockr-path-input")
  expect_equal(dep$version, "0.3.0")
})

test_that("required path_input_ui carries the amber required-empty affordance", {
  req_html <- as.character(htmltools::tagList(
    path_input_ui("test_id", required = TRUE)
  ))
  expect_true(grepl("data-required=\"true\"", req_html, fixed = TRUE))
  expect_true(grepl("blockr-field--required-empty", req_html, fixed = TRUE))

  # Optional fields (the default) carry neither.
  opt_html <- as.character(htmltools::tagList(path_input_ui("test_id")))
  expect_false(grepl("data-required", opt_html, fixed = TRUE))
  expect_false(grepl("blockr-field--required-empty", opt_html, fixed = TRUE))
})

test_that("path_input_ui returns a tagList", {
  ui <- path_input_ui("test_id")

  # Should be a tag list
  expect_true(inherits(ui, "shiny.tag.list"))
})

test_that("path_input_ui honors custom placeholder", {
  ui <- path_input_ui("test_id", placeholder = "Enter directory path...")
  html <- as.character(htmltools::tagList(ui))
  expect_true(grepl("Enter directory path...", html, fixed = TRUE))
})

test_that("path_input_server returns input value", {
  shiny::testServer(
    path_input_server,
    args = list(
      data_dir = reactive(""),
      mode = "file"
    ),
    {
      # Initially empty
      result <- session$returned()
      expect_equal(result, "")

      # Set path value (simulates a commit: Enter/blur/dropdown selection)
      session$setInputs(path_text = "data.csv")
      session$flushReact()

      result <- session$returned()
      expect_equal(result, "data.csv")
    }
  )
})

# ---------------------------------------------------------------------------
# list_dir_response(): shared autocomplete listing endpoint
# ---------------------------------------------------------------------------

parse_listing <- function(resp) {
  expect_equal(resp$status, 200L)
  jsonlite::fromJSON(resp$content, simplifyDataFrame = FALSE)
}

with_listing_dir <- function(code) {
  root <- tempfile("listing_")
  dir.create(file.path(root, "adir"), recursive = TRUE)
  dir.create(file.path(root, "zdir"))
  writeLines("x", file.path(root, "data.csv"))
  writeLines("x", file.path(root, "notes.txt"))
  writeLines("x", file.path(root, "script.R")) # not a rio extension
  on.exit(unlink(root, recursive = TRUE))
  force(code)(root)
}

test_that("list_dir_response lists directories first, then files", {
  with_listing_dir(function(root) {
    resp <- blockr.io:::list_dir_response(paste0(root, "/"), mode = "file")
    data <- parse_listing(resp)

    names <- vapply(data$items, function(x) x$name, character(1))
    isdir <- vapply(data$items, function(x) isTRUE(x$isdir), logical(1))

    expect_equal(names, c("adir", "zdir", "data.csv", "notes.txt"))
    expect_equal(isdir, c(TRUE, TRUE, FALSE, FALSE))
    expect_equal(data$total, 4L)

    # Unsupported extension filtered out
    expect_false("script.R" %in% names)

    # Files carry sizes, directories don't
    expect_null(data$items[[1]]$size)
    expect_true(data$items[[3]]$size > 0)
  })
})

test_that("list_dir_response in directory mode lists only directories", {
  with_listing_dir(function(root) {
    resp <- blockr.io:::list_dir_response(paste0(root, "/"), mode = "directory")
    data <- parse_listing(resp)

    names <- vapply(data$items, function(x) x$name, character(1))
    expect_equal(names, c("adir", "zdir"))
  })
})

test_that("list_dir_response ranks prefix matches before substring matches", {
  root <- tempfile("listing_")
  dir.create(root)
  writeLines("x", file.path(root, "mydata.csv"))
  writeLines("x", file.path(root, "data.csv"))
  on.exit(unlink(root, recursive = TRUE))

  resp <- blockr.io:::list_dir_response(file.path(root, "data"), mode = "file")
  data <- parse_listing(resp)

  names <- vapply(data$items, function(x) x$name, character(1))
  # both contain "data"; the prefix match must come first
  expect_equal(names, c("data.csv", "mydata.csv"))
})

test_that("list_dir_response resolves relative paths against dir_root", {
  with_listing_dir(function(root) {
    resp <- blockr.io:::list_dir_response(
      "adir/", dir_root = root, mode = "file"
    )
    data <- parse_listing(resp)
    expect_equal(data$base, "adir/")
    expect_equal(length(data$items), 0L)
  })
})

test_that("list_dir_response caps at 50 and reports the total", {
  root <- tempfile("listing_")
  dir.create(root)
  for (i in seq_len(60)) {
    writeLines("x", file.path(root, sprintf("f%02d.csv", i)))
  }
  on.exit(unlink(root, recursive = TRUE))

  resp <- blockr.io:::list_dir_response(paste0(root, "/"), mode = "file")
  data <- parse_listing(resp)

  expect_equal(length(data$items), 50L)
  expect_equal(data$total, 60L)
})

test_that("list_dir_response honors the file-access policy", {
  base <- tempfile("listing_policy_")
  allowed <- file.path(base, "open")
  secret <- file.path(base, "secret")
  dir.create(allowed, recursive = TRUE)
  dir.create(secret)
  writeLines("x", file.path(allowed, "ok.csv"))
  writeLines("x", file.path(secret, "hidden.csv"))

  old <- options(blockr.verify_read_path = within_dirs(allowed))
  on.exit({
    options(old)
    unlink(base, recursive = TRUE)
  })

  # Allowed root lists normally
  resp_ok <- blockr.io:::list_dir_response(
    paste0(allowed, "/"), mode = "file", policy = "read"
  )
  data_ok <- parse_listing(resp_ok)
  expect_equal(
    vapply(data_ok$items, function(x) x$name, character(1)),
    "ok.csv"
  )

  # Outside the allowlist: nothing is enumerable
  resp_no <- blockr.io:::list_dir_response(
    paste0(secret, "/"), mode = "file", policy = "read"
  )
  data_no <- parse_listing(resp_no)
  expect_equal(length(data_no$items), 0L)
})

test_that("list_dir_response returns empty listing for missing directories", {
  resp <- blockr.io:::list_dir_response(
    file.path(tempfile("nope_"), ""), mode = "file"
  )
  data <- parse_listing(resp)
  expect_equal(length(data$items), 0L)
  expect_equal(data$total, 0L)
})
