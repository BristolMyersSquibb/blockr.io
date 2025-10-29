# Debug script v2: Test the module server directly
# Maybe the issue is testing block_server vs the actual module server

library(blockr.io)
library(blockr.core)
library(shiny)

temp_dir <- tempfile("write_test_")
dir.create(temp_dir)
cat("Output directory:", temp_dir, "\n")

blk <- new_write_block(
  directory = temp_dir,
  filename = "iris_submit",
  format = "csv",
  mode = "browse",
  auto_write = FALSE
)

cat("\n=== Testing write block expr_server directly ===\n")

# Try testing the expr_server field directly
# Transform blocks receive variadic args
testServer(
  blk$expr_server,  # Test the module server directly, not block_server
  args = list(
    data = iris  # Just pass data directly as named arg
  ),
  {
    session$flushReact()

    result <- session$returned

    cat("\n--- BEFORE submit button click ---\n")
    cat("Mode:", result$state$mode(), "\n")
    cat("Auto-write:", result$state$auto_write(), "\n")
    cat("Directory:", result$state$directory(), "\n")
    cat("Filename:", result$state$filename(), "\n")

    expr_before <- result$expr()
    cat("Expression is NULL:", is.null(expr_before), "\n")

    cat("\n--- CLICKING submit button ---\n")
    # Try clicking the button
    session$setInputs(submit_write = 1)
    session$flushReact()

    cat("\n--- AFTER submit button click ---\n")
    expr_after <- result$expr()
    cat("Expression is NULL:", is.null(expr_after), "\n")

    if (!is.null(expr_after)) {
      cat("\n SUCCESS: Expression was generated!\n")
      print(expr_after)
    } else {
      cat("\n STILL NULL: Button click didn't work\n")
    }
  }
)

cat("\n=== Test complete ===\n")
unlink(temp_dir, recursive = TRUE)
