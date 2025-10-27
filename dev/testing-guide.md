# Testing Guide for blockr.io

## Overview

This document outlines the three-layer testing strategy for blockr.io. Each layer serves a distinct purpose with clear trade-offs. Understanding WHEN to use each tool is critical.

## Testing Philosophy

1. **Test at the appropriate level** - Don't use a slow tool when a fast one suffices
2. **Each layer has a purpose** - Unit tests ≠ testServer ≠ shinytest2
3. **Speed matters** - Fast tests = fast feedback = better development
4. **Coverage over duplication** - Don't test the same thing at multiple layers

---

## The Three Layers

### Layer 1: Unit Tests (Pure Functions, No Shiny)

**Files**: `test-expr-read.R` (35 tests), `test-file-formats.R` (25 tests)
**Speed**: ⚡⚡⚡ Very fast (~0.1-0.3s per test)
**Total**: 60 tests

**What it CAN test**:
- ✅ Pure functions (`read_expr()`, helper functions)
- ✅ Expression generation correctness
- ✅ Expression evaluation (execute the generated code)
- ✅ Edge cases (empty data, missing files, weird inputs)
- ✅ All file formats and parameters
- ✅ Logic that doesn't need Shiny reactivity

**What it CANNOT test**:
- ❌ Reactive values and reactive contexts
- ❌ Shiny modules and moduleServer
- ❌ UI rendering or user interactions
- ❌ State management across reactive updates

**Example**:
```r
test_that("read_expr generates correct CSV expression", {
  expr <- read_expr(paths = "data.csv", file_type = "csv", sep = ",")
  expect_equal(as.character(expr[[1]]), c("::", "readr", "read_csv"))

  # Can even evaluate it
  temp_file <- tempfile(fileext = ".csv")
  write.csv(data.frame(x = 1:3), temp_file, row.names = FALSE)
  result <- eval(expr)
  expect_equal(nrow(result), 3)
})
```

---

### Layer 2: testServer (Reactive Logic, No Browser)

**Files**: `test-read-block-server.R` (21 tests)
**Speed**: ⚡⚡ Fast (~0.2-0.5s per test)
**Total**: 21 tests

**What it CAN test**:
- ✅ Block's `expr_server` reactive logic
- ✅ Reactive values and contexts
- ✅ State management
- ✅ Expression generation in reactive context
- ✅ Module server functions

**What it CANNOT test**:
- ❌ Actual UI rendering
- ❌ User interactions (clicks, uploads)
- ❌ JavaScript behavior
- ❌ Full app integration with `serve()`
- ❌ Browser-specific behavior

**Critical pattern for blockr**:
```r
test_that("expr_server generates correct expression", {
  blk <- new_read_block(path = "data.csv")

  testServer(
    blk$expr_server,  # Test expr_server directly
    args = list(),    # Data blocks: no args; Transform blocks: data = reactive(df)
    {
      session$flushReact()

      result <- session$returned
      expect_true(is.reactive(result$expr))

      # Test expression
      expr_result <- result$expr()
      expect_true(grepl("readr::read_csv", deparse(expr_result)))

      # Test state
      expect_true(is.reactive(result$state$path))
      expect_equal(result$state$csv_sep(), ",")
    }
  )
})
```

---

### Layer 3: shinytest2 (Full Integration, Browser Required)

**Files**: `test-shinytest2-read-block.R` (13 tests)
**Speed**: 🐌 Slow (~8-15s per test)
**Total**: 13 tests

**What it CAN test**:
- ✅ Complete app launches with `serve(block)`
- ✅ Actual data output via `exportTestValues()`
- ✅ End-to-end workflows
- ✅ Block integration
- ✅ User interactions (if tests added)

**What it CANNOT do better than testServer**:
- ❌ **Nothing** - it can do everything testServer can, BUT 20-50x slower
- ❌ Not suitable for rapid iteration
- ❌ Requires browser setup (chromote)

**When to use**:
- **Critical user journeys only**
- Final validation that `serve()` works
- Testing actual data output users see
- **Use sparingly** - if testServer can test it, use testServer

**Anti-pattern (DON'T DO THIS)**:
```r
test_that("expression has correct structure", {
  app <- AppDriver$new(...)  # Takes 10 seconds
  values <- app$get_values()
  expect_true(grepl("readr", values$...))  # Could test in 0.1s with testServer!
})
```

**Good use**:
```r
test_that("block outputs correct data end-to-end", {
  temp_csv <- tempfile(fileext = ".csv")
  write.csv(mtcars[1:5, ], temp_csv, row.names = FALSE)

  app_dir <- create_test_app(
    block_code = sprintf('serve(new_read_block(path = "%s"))', temp_csv)
  )

  app <- AppDriver$new(app_dir, timeout = 30000)
  result_data <- get_block_result(app)

  # Verify END RESULT
  expect_equal(nrow(result_data), 5)
  expect_equal(result_data$mpg, mtcars$mpg[1:5])

  cleanup_test_app(app_dir, app)
})
```

---

## Migration Success Story: From Layer 3 to Layer 2

### Case Study: Read Block Tests

**Problem:** Three shinytest2 tests were taking ~30 seconds to verify basic data output functionality that didn't require browser interaction.

**Tests migrated:**
1. "loads CSV file with default settings" - Basic data loading
2. "handles multiple CSV files with rbind" - Multiple file combination
3. "handles CSV with custom delimiter" - Parameter configuration

**What changed:**
- Enhanced existing testServer tests to also **evaluate expressions** and verify data output
- Deleted redundant shinytest2 test file entirely

**Before:**
```r
# shinytest2 - Takes ~10 seconds
app <- AppDriver$new(serve(new_read_block(path = csv_file)))
result_data <- get_block_result(app)
expect_equal(nrow(result_data), 5)
```

**After:**
```r
# testServer - Takes ~0.5 seconds
blk <- new_read_block(path = csv_file)
testServer(blk$expr_server, args = list(), {
  session$flushReact()
  expr_result <- session$returned$expr()

  # Evaluate expression and verify data
  data <- eval(expr_result)
  expect_equal(nrow(data), 5)
})
```

**Results:**
- **Performance:** 30s → 1.5s (20x faster)
- **Coverage:** Identical - both verify data output
- **Maintenance:** Simpler tests without browser setup overhead

**Key insight:** These tests were NOT testing integration - they were testing that `serve()` could launch and extract data, which is just expression evaluation with extra steps. testServer + `eval()` provides identical coverage at a fraction of the cost.

**When Layer 3 IS appropriate:**
- User uploads file via fileInput widget
- User clicks "Advanced Options" accordion and changes delimiter
- Multi-block stack integration testing
- Testing actual UI rendering and JavaScript behavior

These scenarios test **user workflows and integration**, not just data output.

**New integration test added:**
See `test-shinytest2-read-block-integration.R` for focused integration testing:

**"user changes CSV delimiter in UI and data updates" (1 test, ~5s, 8 assertions):**
- Tests TRUE user workflow that testServer CANNOT simulate:
  1. User loads file with wrong delimiter → sees malformed data
  2. User changes `csv_sep` input in UI via `app$set_inputs()`
  3. App reactively updates → data becomes correct
- This tests the complete pipeline: UI input → reactive update → data output → exportTestValues
- Input ID pattern discovered: `"{serve_id}-expr-{input_name}"` (e.g., `"block-expr-csv_sep"`)
- Note: No separate smoke test needed - if integration test passes, serve() infrastructure works

This test provides REAL integration value. It verifies something testServer fundamentally cannot test: user interaction workflows.

**Additional integration test ideas for future:**
- File upload widget interaction with persistence
- shinyFiles browser widget interaction
- Multi-block stack integration with data flow between blocks
