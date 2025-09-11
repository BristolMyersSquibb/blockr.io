test_that("new_read_block constructor works", {
  # Test basic constructor
  blk <- new_read_block()
  expect_s3_class(blk, c("read_block", "data_block", "block"))
  
  # Test with upload source
  blk_upload <- new_read_block(source = "upload")
  expect_s3_class(blk_upload, c("read_block", "data_block", "block"))
  
  # Test with path source
  blk_path <- new_read_block(source = "path")
  expect_s3_class(blk_path, c("read_block", "data_block", "block"))
  
  # Test with different combine strategies
  strategies <- c("auto", "rbind", "cbind", "first", "error")
  for (strategy in strategies) {
    blk_strategy <- new_read_block(combine = strategy)
    expect_s3_class(blk_strategy, c("read_block", "data_block", "block"))
  }
})

test_that("new_read_block parameter validation", {
  # Test invalid source
  expect_error(new_read_block(source = "invalid"))
  
  # Test invalid combine strategy
  expect_error(new_read_block(combine = "invalid"))
})

test_that("new_read_block with custom volumes", {
  custom_volumes <- c(temp = tempdir())
  blk <- new_read_block(source = "path", volumes = custom_volumes)
  expect_s3_class(blk, c("read_block", "data_block", "block"))
})