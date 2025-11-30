# Testing Guide for blockr.io

## Overview

blockr.io uses a **two-tier testing strategy** for fast, maintainable tests.

## Testing Strategy

| Tier | Tool | Use For | Speed |
|------|------|---------|-------|
| 1 | Unit tests | Pure R functions (expression builders, helpers) | Very fast |
| 2 | testServer | ALL Shiny reactivity and UI interactions | Fast |

---

## Tier 1: Unit Tests

**Files**: `test-expr-read.R`, `test-expr-write.R`, `test-file-formats.R`

**What to test**:
- Pure functions (`read_expr()`, `write_expr()`, helpers)
- Expression generation and evaluation
- Edge cases (empty data, missing files)
- All file formats and parameters

**Example**:
```r
test_that("read_expr generates correct CSV expression", {
  expr <- read_expr(paths = "data.csv", file_type = "csv", sep = ",")
  expect_true(grepl("readr::read_csv", deparse(expr)))
})
```

---

## Tier 2: testServer

**Files**: `test-read-block-server.R`, `test-write-block-server.R`

**What to test**:
- Block reactive logic
- State management
- UI interactions (via `session$setInputs()`)
- Button clicks and input changes

### Pattern: Testing with blockr.core Framework

```r
test_that("write_block generates expression", {
  blk <- new_write_block(
    directory = temp_dir,
    filename = "output",
    format = "csv",
    mode = "browse",
    auto_write = TRUE
  )

  testServer(
    blockr.core:::get_s3_method("block_server", blk),
    args = list(
      x = blk,
      data = list(
        ...args = reactiveValues(data = iris)
      )
    ),
    {
      session$flushReact()
      result <- session$returned

      # Test expression
      expr_result <- result$expr()
      expect_true(grepl("readr::write_csv", deparse(expr_result)))

      # Test actual result (framework evaluates expression)
      data_result <- result$result()
      expect_true(is.data.frame(data_result))
    }
  )
})
```

### Pattern: Testing Button Clicks

```r
test_that("write_block handles submit button click", {
  blk <- new_write_block(auto_write = FALSE)

  testServer(
    blockr.core:::get_s3_method("block_server", blk),
    args = list(x = blk, data = list(...args = reactiveValues(data = iris))),
    {
      session$flushReact()

      # Before click: no expression
      expect_null(session$returned$expr())

      # Simulate button click (use namespaced ID)
      session$setInputs(`expr-submit_write` = 1)
      session$flushReact()

      # After click: expression generated
      expect_false(is.null(session$returned$expr()))
    }
  )
})
```

### Pattern: Testing Input Changes

```r
test_that("block handles UI input changes", {
  testServer(..., {
    session$flushReact()

    # Change input value
    session$setInputs(csv_sep = ";")
    session$flushReact()

    # Verify expression updated
    expr <- session$returned$expr()
    expect_true(grepl('delim = ";"', deparse(expr)))
  })
})
```

---

## Key Points

1. **Use `session$returned$result()`** to get framework-evaluated results
2. **Use `session$setInputs()`** to simulate UI interactions
3. **Use namespaced IDs** for nested module inputs (e.g., `expr-submit_write`)
4. **Call `session$flushReact()`** after setting inputs to process reactives
