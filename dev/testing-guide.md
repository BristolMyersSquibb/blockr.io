# Testing Guide for blockr.io

## Overview

This document outlines the **TWO-TIER testing strategy** for blockr.io. Understanding WHEN to use each tool is critical for fast, maintainable tests.

## Testing Philosophy

1. **DEFAULT: Use testServer** - For all Shiny reactivity, UI interactions, and module logic
2. **Use unit tests for pure functions** - Expression builders, helpers, parsers
3. **Speed matters** - Fast tests = fast feedback = better development
4. **shinytest2 is almost never needed** - Only for pure visual UI checks with no server impact

---

## ⚠️ IMPORTANT: Two-Tier Strategy

**99% of your tests should be:**
- **Tier 1: Unit Tests** - Pure R functions (no Shiny)
- **Tier 2: testServer** - ALL Shiny reactivity and UI interactions

**shinytest2 is rarely needed** - See bottom of this guide for exceptional cases.

---

## The Two Main Testing Layers

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

### Layer 2: testServer (ALL Shiny Interactions - USE THIS)

**Files**: `test-read-block-server.R`, `test-write-block-server.R`
**Speed**: ⚡⚡ Fast (~0.2-0.5s per test)

**What it CAN test**:
- ✅ Block reactive logic (data and transform blocks)
- ✅ Reactive values and contexts
- ✅ State management
- ✅ Expression generation in reactive context
- ✅ Module server functions
- ✅ Data transformations (by evaluating expressions)
- ✅ **UI interactions** (button clicks, input changes via `session$setInputs()`)
- ✅ **Reactive auto-updates**
- ✅ **ALL edge cases and error handling**

**What it CANNOT test**:
- ❌ Pure visual UI layout (CSS, rendering)
- ❌ JavaScript-only behavior
- ❌ File upload widgets (fileInput)
- ❌ True browser integration testing

**When to use**:
- Testing Shiny modules (moduleServer)
- Testing reactive behavior
- Testing UI input changes and their effects
- Testing button clicks and interactions
- Testing state management
- **This should be your default for testing ANY Shiny logic**

**Critical patterns for blockr.io**:

**Pattern 1: Testing Data Blocks (Read)**
```r
test_that("read_block expr_server generates correct expression", {
  blk <- new_read_block(path = "data.csv")

  testServer(
    blk$expr_server,  # Test expr_server directly
    args = list(),    # Data blocks don't need data arguments
    {
      session$flushReact()

      result <- session$returned
      expect_true(is.reactive(result$expr))

      # Test expression
      expr_result <- result$expr()
      expect_true(grepl("readr::read_csv", deparse(expr_result)))

      # Can also evaluate to verify data
      data <- eval(expr_result)
      expect_true(is.data.frame(data))
    }
  )
})
```

**Pattern 2: Testing Transform Blocks (Write)**
```r
test_that("write_block block_server generates expression", {
  blk <- new_write_block(
    directory = temp_dir,
    filename = "output",
    format = "csv",
    mode = "browse"
  )

  testServer(
    blockr.core:::get_s3_method("block_server", blk),  # Use block_server for transform blocks
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
      expr_result <- result$expr()

      # Test expression structure
      expect_true(grepl("readr::write_csv", deparse(expr_result)))
    }
  )
})
```

**Pattern 3: Testing UI Button Clicks**
```r
test_that("write_block handles submit button click", {
  blk <- new_write_block(auto_write = FALSE)  # Requires manual submit

  testServer(
    blockr.core:::get_s3_method("block_server", blk),
    args = list(x = blk, data = list(...args = reactiveValues(data = iris))),
    {
      session$flushReact()

      # Expression is NULL before button click
      expect_null(session$returned$expr())

      # SIMULATE BUTTON CLICK - Key: use namespaced ID
      session$setInputs(`expr-submit_write` = 1)
      session$flushReact()

      # Expression is generated after click
      expect_false(is.null(session$returned$expr()))
    }
  )
})
```

**Pattern 4: Testing Input Changes**
```r
test_that("read_block handles UI input changes for delimiter", {
  blk <- new_read_block(path = semicolon_file)  # File has ";" delimiter

  testServer(
    blk$expr_server,
    args = list(),
    {
      session$flushReact()

      # Initial expression uses default delimiter
      expr_initial <- session$returned$expr()

      # USER CHANGES DELIMITER IN UI
      session$setInputs(csv_sep = ";")
      session$flushReact()

      # Expression updated with new delimiter
      expr_updated <- session$returned$expr()
      expect_true(grepl('delim = ";"', deparse(expr_updated)))

      # Data is now correct
      data_correct <- eval(expr_updated)
      expect_equal(ncol(data_correct), 2)  # Should have 2 columns now
    }
  )
})
```

---

## Note on shinytest2

**We don't use shinytest2.** All Shiny testing is done with testServer, which can simulate UI interactions via `session$setInputs()`.

### Why We Migrated Away from shinytest2

**Previous misunderstanding:**
- "Button clicks need shinytest2" ❌
- "UI interactions need a browser" ❌
- "testServer can't simulate user workflows" ❌

**Reality:**
- Button clicks: `session$setInputs(button_id = 1)` ✅
- Input changes: `session$setInputs(input_id = new_value)` ✅
- Reactive updates: All work in testServer ✅

**Migration results:**
- Deleted ALL shinytest2 tests (read and write blocks)
- Migrated to testServer with `session$setInputs()`
- **20-50x faster tests**
- **Zero loss of coverage**

### Examples of "Impossible" Tests That Actually Work

**"You can't test button clicks without a browser"** - FALSE:
```r
# This WORKS with testServer:
session$setInputs(`expr-submit_write` = 1)  # Simulates clicking submit button
session$flushReact()
expect_false(is.null(result$expr()))  # Expression generated!
```

**"You can't test UI input changes without a browser"** - FALSE:
```r
# This WORKS with testServer:
session$setInputs(csv_sep = ";")  # User changes delimiter in UI
session$flushReact()
expect_true(grepl('delim = ";"', deparse(result$expr())))  # Expression updated!
```

### If You Think You Need shinytest2

**Stop and ask yourself:**
- Can I test this with `session$setInputs()` in testServer? → Almost always YES
- Am I testing logic or just visual UI layout? → Logic = testServer
- Do I need a browser? → Almost always NO

**The only true exceptions:**
- Testing pure CSS/visual layout (no server impact)
- Testing JavaScript-only behavior
- Testing file upload widgets (fileInput requires browser)
- True end-to-end integration across multiple systems

For blockr.io, **NONE of these apply**. All our tests are now testServer.
