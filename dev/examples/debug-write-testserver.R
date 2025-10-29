# Debug script for write block testServer behavior
# This helps us understand why session$setInputs(submit_write = 1) might not work

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

cat("\n=== Testing write block with auto_write=FALSE ===\n")

testServer(
  blockr.core:::get_s3_method("block_server", blk),
  args = list(
    x = blk,
    data = list(
      ...args = reactiveValues(
        data = iris
      )
    )
  ),
  {
    session$flushReact()

    result <- session$returned

    cat("\n--- BEFORE submit button click ---\n")
    cat("Mode:", result$state$mode(), "\n")
    cat("Auto-write:", result$state$auto_write(), "\n")
    cat("Directory:", result$state$directory(), "\n")
    cat("Filename:", result$state$filename(), "\n")

    # Check observeEvent requirements from R/write.R:242-247
    cat("\n--- Checking observeEvent requirements ---\n")
    # The observeEvent has these requirements:
    # req(length(arg_names()) > 0)
    # req(r_directory())
    # req(r_mode() == "browse")
    # req(!r_auto_write())

    # We can't access arg_names() directly, but we know data was passed
    cat("Mode == 'browse':", result$state$mode() == "browse", "\n")
    cat("!auto_write:", !result$state$auto_write(), "\n")
    cat("Directory exists:", !is.null(result$state$directory()) && result$state$directory() != "", "\n")

    expr_before <- result$expr()
    cat("\nExpression is NULL:", is.null(expr_before), "\n")

    cat("\n--- Files before submit ---\n")
    files_before <- list.files(temp_dir)
    cat("Files:", paste(files_before, collapse = ", "), "\n")
    cat("Count:", length(files_before), "\n")

    cat("\n--- CLICKING submit button ---\n")
    cat("Trying different input IDs...\n")

    # The button might be namespaced!
    # In shinytest2, it's "block-expr-submit_write"
    # When testing block_server, the namespace structure might be different

    # Try option 1: Direct ID
    cat("Try 1: submit_write\n")
    session$setInputs(submit_write = 1)
    session$flushReact()

    expr_try1 <- result$expr()
    cat("After try 1, expr is NULL:", is.null(expr_try1), "\n")

    if (is.null(expr_try1)) {
      # Try option 2: With expr namespace
      cat("Try 2: expr-submit_write\n")
      session$setInputs(`expr-submit_write` = 1)
      session$flushReact()

      expr_try2 <- result$expr()
      cat("After try 2, expr is NULL:", is.null(expr_try2), "\n")
    }

    cat("\n--- AFTER submit button click ---\n")
    expr_after <- result$expr()
    cat("Expression is NULL:", is.null(expr_after), "\n")

    if (!is.null(expr_after)) {
      cat("\nExpression generated:\n")
      print(expr_after)

      cat("\n--- Evaluating expression ---\n")
      eval(expr_after)

      cat("\n--- Files after submit ---\n")
      files_after <- list.files(temp_dir)
      cat("Files:", paste(files_after, collapse = ", "), "\n")
      cat("Count:", length(files_after), "\n")
    } else {
      cat("\n!!! PROBLEM: Expression is still NULL after button click !!!\n")
      cat("This means the observeEvent for submit_write didn't trigger\n")
      cat("or didn't set r_write_expression_set() properly\n")
    }
  }
)

cat("\n=== Test complete ===\n")
unlink(temp_dir, recursive = TRUE)
