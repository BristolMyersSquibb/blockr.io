# Debug script for TSV test failure

library(blockr.io)
library(shiny)

# Create TSV file
temp_tsv <- tempfile(fileext = ".tsv")
write.table(
  data.frame(name = c("Alice", "Bob", "Charlie"), age = c(25, 30, 35)),
  temp_tsv,
  sep = "\t",
  row.names = FALSE,
  quote = FALSE
)

cat("TSV file created:", temp_tsv, "\n")

cat("File contents:\n")
cat(readLines(temp_tsv), sep = "\n")

blk <- new_read_block(path = temp_tsv)

testServer(
  blk$expr_server,
  args = list(),
  {
    session$flushReact()

    result <- session$returned
    expr_result <- result$expr()

    cat("\n=== Expression ===\n")
    print(expr_result)

    cat("\n=== Evaluating expression ===\n")
    data <- eval(expr_result)

    cat("\n=== Data structure ===\n")
    print(str(data))

    cat("\n=== Column names ===\n")
    print(names(data))

    cat("\n=== Data preview ===\n")
    print(head(data))

    cat("\n=== Check columns ===\n")
    cat("Has 'name' column:", "name" %in% names(data), "\n")
    cat("Has 'age' column:", "age" %in% names(data), "\n")

    if ("name" %in% names(data)) {
      cat("data$name:", data$name, "\n")
    }
    if ("age" %in% names(data)) {
      cat("data$age:", data$age, "\n")
    }
  }
)

unlink(temp_tsv)
