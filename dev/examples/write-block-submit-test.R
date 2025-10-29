# Test workflow for write block with auto_write=FALSE
# This demonstrates the submit button behavior that we're trying to test with testServer
#
# Expected behavior:
# 1. Block starts with auto_write=FALSE
# 2. User sees data preview but NO file is written
# 3. User clicks "Submit" button
# 4. File is written to disk
#
# Run this and click the Submit button to verify the workflow works

library(blockr.io)
library(blockr.core)

# Create temp directory for output
temp_dir <- tempfile("write_test_")
dir.create(temp_dir)
cat("Output directory:", temp_dir, "\n")

# Create write block with auto_write=FALSE (requires manual submit)
write_blk <- new_write_block(
  directory = temp_dir,
  filename = "iris_test",
  format = "csv",
  mode = "browse",
  auto_write = FALSE  # This is the key - requires submit button click
)

# Serve with data
serve(
  write_blk,
  data = list(data = iris),
  id = "test_block"
)

# After running:
# 1. Check that NO file exists initially: list.files(temp_dir)
# 2. Click the "Submit" button in the UI
# 3. Check that file was created: list.files(temp_dir)
# 4. Read and verify: readr::read_csv(file.path(temp_dir, "iris_test.csv"))

# Cleanup instructions:
# unlink(temp_dir, recursive = TRUE)
