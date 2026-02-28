# Tests for utility functions

test_that("get_supported_extensions returns character vector with expected extensions", {
  exts <- get_supported_extensions()

  expect_type(exts, "character")
  expect_true(length(exts) > 0)
  expect_true("csv" %in% exts)
  expect_true("xlsx" %in% exts)
  expect_true("parquet" %in% exts)
  expect_true("json" %in% exts)
  expect_true("rds" %in% exts)
})

test_that("cleanup_uploads removes old files and keeps recent ones", {
  upload_dir <- tempfile("upload_test_")
  dir.create(upload_dir)

  # Create an "old" file and a "recent" file
  old_file <- file.path(upload_dir, "old_data.csv")
  recent_file <- file.path(upload_dir, "recent_data.csv")

  writeLines("old", old_file)
  writeLines("recent", recent_file)

  # Backdate the old file's mtime to 60 days ago
  old_time <- Sys.time() - as.difftime(60, units = "days")
  Sys.setFileTime(old_file, old_time)

  # Both files should exist before cleanup

  expect_true(file.exists(old_file))
  expect_true(file.exists(recent_file))

  # Cleanup with 30-day max age
  cleanup_uploads(upload_dir, max_age_days = 30)

  # Old file should be removed, recent should remain
  expect_false(file.exists(old_file))
  expect_true(file.exists(recent_file))

  unlink(upload_dir, recursive = TRUE)
})

test_that("cleanup_uploads handles missing directory gracefully", {
  expect_silent(cleanup_uploads("/nonexistent/path"))
})

test_that("cleanup_uploads handles empty directory", {
  upload_dir <- tempfile("upload_test_")
  dir.create(upload_dir)

  expect_silent(cleanup_uploads(upload_dir))

  unlink(upload_dir, recursive = TRUE)
})
