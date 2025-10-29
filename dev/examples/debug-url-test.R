# Debug script for URL test failure

library(blockr.io)
library(shiny)

url <- "https://raw.githubusercontent.com/datasets/co2-ppm/master/data/co2-mm-mlo.csv"

blk <- new_read_block(
  path = url,
  source = "url"
)

testServer(
  blk$expr_server,
  args = list(),
  {
    session$flushReact()

    result <- session$returned
    expr_result <- result$expr()

    cat("\n=== Expression result ===\n")
    print(expr_result)

    cat("\n=== Expression text ===\n")
    expr_text <- paste(deparse(expr_result), collapse = " ")
    cat(expr_text, "\n")

    cat("\n=== URL check ===\n")
    cat("URL:", url, "\n")
    cat("URL in expr_text:", grepl(url, expr_text, fixed = TRUE), "\n")

    cat("\n=== State ===\n")
    cat("source:", result$state$source(), "\n")
    cat("path:", result$state$path(), "\n")
  }
)
