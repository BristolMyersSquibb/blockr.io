# Block Development Guide: Core Concepts

**Universal guide for developing blocks in the blockr ecosystem**

This document contains core concepts applicable to all blockr packages (blockr.ggplot, blockr.dplyr, blockr.io, etc.). For package-specific patterns, see your package's additional guides.

## Table of Contents

1. [What is a Block?](#what-is-a-block)
2. [Block Anatomy](#block-anatomy)
3. [Step-by-Step: Creating a Block](#step-by-step-creating-a-block)
4. [Testing & Validation](#testing--validation)
5. [Additional Resources](#additional-resources)

---

## What is a Block?

A **block** is a Shiny module that performs a single task in a data analysis workflow.

Blocks can be connected in a **DAG** (directed acyclic graph) to create powerful workflows. A block receives data from upstream blocks, performs its operation, and passes results to downstream blocks.

**Key characteristics:**
- Built on Shiny modules
- Returns two special values: `expr` (expression) and `state` (reactive values)
- Self-contained: UI + Server + Constructor
- Composable: Can be linked with other blocks

**Block types in blockr.core:**
- **Data blocks**: Load or generate data (e.g., `new_dataset_block()`)
- **Transform blocks**: Modify data (e.g., filter, select, mutate)
- **Plot blocks**: Visualize data
- **Join blocks**: Combine multiple data sources
- **Variadic blocks**: Accept variable number of inputs

---

## Block Anatomy

Every block consists of **three components**:

### 1. UI Function

Defines the user interface using standard Shiny UI functions.

```r
ui <- function(id) {
  tagList(
    selectInput(
      NS(id, "column"),
      label = "Select Column",
      choices = column,
      selected = column
    ),
    numericInput(
      NS(id, "value"),
      label = "Value",
      value = value
    )
  )
}
```

**Requirements:**
- Must take single `id` argument
- Use `NS(id, "input_name")` for all input IDs to create namespaced IDs
- Return `shiny.tag` or `shiny.tag.list` objects
- Use `tagList()` to combine multiple UI elements

### 2. Server Function

Handles reactive logic and returns `expr` and `state`.

```r
server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    # Reactive values to track state
    r_column <- reactiveVal(column)
    r_value <- reactiveVal(value)

    # Update reactive values from inputs
    observeEvent(input$column, r_column(input$column))
    observeEvent(input$value, r_value(input$value))

    # Update UI choices when data changes
    observeEvent(colnames(data()), {
      updateSelectInput(
        session,
        inputId = "column",
        choices = colnames(data()),
        selected = r_column()
      )
    })

    # Return expr and state
    list(
      expr = reactive({
        # Build expression that can be evaluated outside of reactive context
        # Implementation varies by package
        quote(my_function(data, column = r_column(), value = r_value()))
      }),
      state = list(
        column = r_column,
        value = r_value
      )
    )
  })
}
```

**Key points:**
- Takes `id` and data inputs (signature varies by block type)
  - **Data blocks**: `function(id)` (no data input)
  - **Transform blocks**: `function(id, data)` (one data input)
  - **Join blocks**: `function(id, x, y)` (two data inputs)
  - **Variadic blocks**: `function(id, ...args)` (variable inputs)
- Returns `moduleServer()` call
- Must return `list(expr = reactive(...), state = list(...))`
- State names must match constructor parameters exactly

### 3. Constructor Function

Wraps UI and server, initializes the block.

```r
new_my_block <- function(column = character(), value = 0, ...) {
  new_transform_block(
    server = server,
    ui = ui,
    class = "my_block",
    ...
  )
}
```

**Requirements:**
- Expose all UI-controllable parameters as arguments
- DO NOT expose data inputs (these are handled automatically by server signature)
- Call appropriate `new_*_block()` constructor:
  - `new_block()` - Base constructor
  - `new_data_block()` - For data sources
  - `new_transform_block()` - For data transformations
  - `new_plot_block()` - For visualizations
- Forward `...` to parent constructor
- Set appropriate `class` attribute for S3 methods

---

## Step-by-Step: Creating a Block

### Step 1: Define Parameters

Identify what the user should control:

```r
new_my_block <- function(
  column = character(),    # Column to operate on
  value = 0,              # Numeric parameter
  option = "default",     # String parameter
  ...
)
```

**Guidelines:**
- Use sensible defaults
- `character()` for column names (allows empty initialization)
- Document all parameters with `@param` roxygen tags

### Step 2: Create UI Function

```r
ui <- function(id) {
  tagList(
    selectInput(
      NS(id, "column"),
      "Column",
      choices = column,
      selected = column
    ),
    numericInput(
      NS(id, "value"),
      "Value",
      value = value
    ),
    selectInput(
      NS(id, "option"),
      "Option",
      choices = c("default", "alternative"),
      selected = option
    )
  )
}
```

**Best practices:**
- Use clear, descriptive labels
- Provide sensible initial values
- Namespace all IDs with `NS(id, ...)`

### Step 3: Create Server Function

```r
server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    # 1. Initialize reactive values
    r_column <- reactiveVal(column)
    r_value <- reactiveVal(value)
    r_option <- reactiveVal(option)

    # 2. Update reactive values from inputs
    observeEvent(input$column, r_column(input$column))
    observeEvent(input$value, r_value(input$value))
    observeEvent(input$option, r_option(input$option))

    # 3. Update choices when data changes (for column selectors)
    observeEvent(colnames(data()), {
      updateSelectInput(
        session,
        inputId = "column",
        choices = colnames(data()),
        selected = r_column()
      )
    })

    # 4. Build expression and return state
    list(
      expr = reactive({
        # Build expression - varies by package
        # See package-specific guide for patterns
      }),
      state = list(
        column = r_column,
        value = r_value,
        option = r_option
      )
    )
  })
}
```

**Critical requirements:**
- ALL constructor parameters must appear in state list
- State names must match constructor parameter names exactly
- Reactive values should use `r_*` prefix (convention)

### Step 4: Create Constructor

```r
new_my_block <- function(column = character(), value = 0, option = "default", ...) {
  new_transform_block(
    server = server,
    ui = ui,
    class = "my_block",
    ...
  )
}
```

### Step 5: Add Documentation

```r
#' Create a my_block
#'
#' @param column Column to operate on
#' @param value Numeric value for operation
#' @param option Operation mode: "default" or "alternative"
#' @param ... Forwarded to \code{\link[blockr.core]{new_transform_block}}
#' @export
```

---

## Testing & Validation

### Manual Testing

Test your block in isolation:

```r
library(blockr.core)
library(your.package)

serve(
  new_my_block(column = "col1", value = 10),
  data = list(data = your_data)
)
```

**Test different scenarios:**
- Default parameters
- All parameters specified
- Empty inputs (character())
- Edge cases (boundary values)

### Screenshot Validation

**Use the `blockr-validate-blocks` agent for visual validation:**

The validation agent:
- Generates screenshots of your block with realistic test data
- Shows both UI and rendered output
- Identifies common issues automatically
- Creates comprehensive validation reports

**Interpreting screenshots:**
- **Working block:** Screenshot shows UI + output
- **Broken block:** Screenshot shows only UI, no output

### Unit Tests

Every block needs comprehensive tests:

```r
test_that("my_block constructor", {
  # Test basic constructor
  blk <- new_my_block()
  expect_s3_class(blk, c("my_block", "transform_block", "block"))

  # Test with parameters
  blk <- new_my_block(column = "col1", value = 10)
  expect_s3_class(blk, "my_block")

  # Test with all parameters
  blk <- new_my_block(column = "col1", value = 10, option = "alternative")
  expect_s3_class(blk, "my_block")
})
```

**Run tests:**
```r
devtools::test()                    # Run all tests
devtools::test(filter = "my_block") # Run specific test file
```

---

## Additional Resources

### blockr.core Documentation

- **Create block vignette:** `vignette("create-block", package = "blockr.core")`
- **Extend blockr vignette:** `vignette("extend-blockr", package = "blockr.core")`
- **Get started vignette:** `vignette("get-started", package = "blockr.core")`

### Package-Specific Guides

After understanding these core concepts, consult your package's specific guide:
- **blockr.ggplot:** See `ggplot-blocks-guide.md` for ggplot2 patterns
- **blockr.dplyr:** See package-specific guide for dplyr patterns
- **blockr.io:** See package-specific guide for I/O patterns

### Quick Reference

**Block Constructor Pattern:**
```r
new_my_block <- function(param1 = default1, param2 = default2, ...) {
  ui <- function(id) {
    tagList(
      selectInput(NS(id, "param1"), "Label", choices = param1, selected = param1)
    )
  }

  server <- function(id, data) {
    moduleServer(id, function(input, output, session) {
      r_param1 <- reactiveVal(param1)
      observeEvent(input$param1, r_param1(input$param1))

      list(
        expr = reactive({
          # Build expression
        }),
        state = list(param1 = r_param1)
      )
    })
  }

  new_transform_block(server, ui, class = "my_block", ...)
}
```

**Key Rules:**
1. All constructor params → state list
2. Use `NS()` for input IDs
3. Return `expr` + `state` from server
4. Forward `...` to parent constructor
5. Write tests for all blocks

---

## Next Steps

1. ✅ Understand core concepts (this document)
2. 📚 Read package-specific guide for implementation patterns
3. 🎨 Review UI guidelines for consistent design
4. 🧪 Write comprehensive tests
5. ✨ Validate with screenshot testing

**Happy block building!** 🎉
