# UI Development Guidelines

This guide documents UI patterns and design principles for blockr.dplyr blocks. Follow these guidelines to create consistent, professional, and responsive block interfaces for data manipulation.

## Table of Contents

1. [UI Philosophy](#ui-philosophy)
2. [Responsive Layout System](#responsive-layout-system)
3. [Color & Styling](#color--styling)
4. [Common Patterns](#common-patterns)
5. [Quick Reference](#quick-reference)

---

## UI Philosophy

### Core Principles

1. **Minimalist & Professional**: Clean, uncluttered interfaces with subtle styling
2. **Responsive**: Automatically adapts from narrow (1 column) to wide (4 columns)
3. **Consistent**: All inputs have uniform width and spacing
4. **Clear Labels**: Descriptive labels for all inputs

### Design Goals

- **Uniform Input Width**: All controls (select, slider, checkbox) have same width
- **Light Color Palette**: Gray/white tones, no flashy colors
- **Clean Typography**: Clear hierarchy with section headers
- **Consistent Spacing**: Predictable margins and padding

---

## Responsive Layout System

### The Grid Pattern

Use the `block_responsive_css()` function for automatic responsive layout:

```r
ui <- function(id) {
  tagList(
    # Add responsive CSS
    block_responsive_css(),

    div(
      class = "block-container",
      div(
        class = "block-form-grid",

        # Your sections and inputs here
        div(
          class = "block-section",
          tags$h4("Filter Options"),
          div(
            class = "block-section-grid",

            div(
              class = "block-input-wrapper",
              selectInput(NS(id, "column"), "Column", choices = column)
            ),

            div(
              class = "block-input-wrapper",
              selectInput(NS(id, "operator"), "Operator",
                         choices = c("==", "!=", ">", "<"))
            )
          )
        )
      )
    )
  )
}
```

### How It Works

**CSS Grid with Auto-Fit:**
```css
.block-form-grid {
  display: grid;
  gap: 15px;
  grid-template-columns: repeat(auto-fit, minmax(250px, 1fr));
}
```

**Responsive Behavior:**
- **Narrow (< 250px)**: 1 column
- **Medium (250-500px)**: 1 column
- **Wide (500-750px)**: 2 columns
- **Very Wide (750-1000px)**: 3 columns
- **Extra Wide (> 1000px)**: 4 columns

**Key Features:**
- All inputs automatically share same grid tracks
- Uniform width across different input types
- No manual media queries needed
- Sections use `display: contents` to flatten structure

### Section Structure

```r
# Section with header
div(
  class = "block-section",
  tags$h4("Section Title"),
  div(
    class = "block-section-grid",

    # Inputs go here - each wrapped in block-input-wrapper
    div(
      class = "block-input-wrapper",
      selectInput(NS(id, "param"), "Label", choices = ...)
    ),

    div(
      class = "block-input-wrapper",
      sliderInput(NS(id, "value"), "Value", ...)
    )
  )
)
```

**CSS Behavior:**
- `.block-section` and `.block-section-grid` use `display: contents`
- This "flattens" them so inputs participate directly in parent grid
- Headers (`h4`) span full width: `grid-column: 1 / -1`
- Result: uniform input widths regardless of input type

### Help Text

```r
div(
  class = "block-help-text",
  "Explanatory text goes here"
)
```

Help text automatically:
- Spans full width
- Uses subtle gray color (#666)
- Has proper spacing from inputs above

---

## Color & Styling

### Button Styles

**Use light/gray buttons for all controls:**

```r
# shinyWidgets buttons
shinyWidgets::radioGroupButtons(
  inputId = NS(id, "type"),
  label = "Join Type",
  choices = c("left", "right", "inner", "full"),
  status = "light",  # ← Always use "light"
  size = "sm"
)
```

**Status colors available:**
- `status = "light"` → Gray/white (preferred for blockr)
- ❌ `status = "primary"` → Blue (too flashy)
- ❌ `status = "success"` → Green (too flashy)

### Color Palette

**UI Elements (Minimalist Grays):**
```css
/* Primary text */
color: #333;

/* Secondary/muted text */
color: #6c757d;

/* Help text */
color: #666;

/* Borders */
border-color: #ddd;

/* Backgrounds */
background: #f8f9fa;
background: white;
```

### Typography

```css
/* Section headers */
h4 {
  font-size: 1.1rem;
  font-weight: 600;
  color: #333;
  margin-top: 5px;
  margin-bottom: 0px;
}

/* Help text */
.block-help-text {
  font-size: 0.875rem;
  color: #666;
}
```

### Spacing & Layout

```css
/* Grid gap between inputs */
gap: 15px;

/* Section spacing */
.block-section:not(:first-child) {
  margin-top: 20px;
}

/* Input margins */
.block-input-wrapper .form-group {
  margin-bottom: 10px;
}

/* Container padding */
.block-container {
  padding-bottom: 15px;
}
```

---

## Common Patterns

### Basic Block Layout

```r
new_my_dplyr_block <- function(column = character(), value = "", ...) {
  ui <- function(id) {
    tagList(
      block_responsive_css(),

      div(
        class = "block-container",
        div(
          class = "block-form-grid",

          # Main Section
          div(
            class = "block-section",
            tags$h4("Filter Settings"),
            div(
              class = "block-section-grid",
              div(
                class = "block-input-wrapper",
                selectInput(NS(id, "column"), "Column",
                           choices = column, selected = column)
              ),
              div(
                class = "block-input-wrapper",
                textInput(NS(id, "value"), "Value", value = value)
              )
            )
          )
        )
      )
    )
  }

  server <- function(id, data) {
    moduleServer(id, function(input, output, session) {
      # Server logic here
    })
  }

  new_transform_block(
    server = server,
    ui = ui,
    class = "my_dplyr_block",
    ...
  )
}
```

### Multi-Column Selector

```r
# For blocks like select, arrange
div(
  class = "block-input-wrapper",
  selectInput(
    NS(id, "columns"),
    "Select Columns",
    choices = colnames(data()),
    selected = columns,
    multiple = TRUE
  )
)
```

### Operator Selection

```r
# For filter blocks
div(
  class = "block-input-wrapper",
  selectInput(
    NS(id, "operator"),
    "Operator",
    choices = c(
      "Equal to" = "==",
      "Not equal to" = "!=",
      "Greater than" = ">",
      "Less than" = "<",
      "Greater or equal" = ">=",
      "Less or equal" = "<="
    ),
    selected = operator
  )
)
```

### Join Type Selection

```r
# For join blocks
div(
  class = "block-input-wrapper",
  shinyWidgets::radioGroupButtons(
    inputId = NS(id, "type"),
    label = "Join Type",
    choices = c("left", "right", "inner", "full"),
    selected = type,
    status = "light",
    size = "sm"
  )
)
```

---

## Quick Reference

### CSS Classes

| Class | Purpose | Behavior |
|-------|---------|----------|
| `.block-container` | Outer wrapper | Padding, margins |
| `.block-form-grid` | Main grid | Responsive columns |
| `.block-section` | Section wrapper | `display: contents` |
| `.block-section-grid` | Section grid | `display: contents` |
| `.block-input-wrapper` | Input wrapper | Full width |
| `.block-help-text` | Help text | Full width, gray |

### Required Functions

```r
# Add responsive CSS
block_responsive_css()

# Namespace inputs
NS(id, "input_name")
```

### Grid Behavior

- **1 column**: Width < 250px
- **2 columns**: Width 250-750px
- **3 columns**: Width 750-1000px
- **4 columns**: Width > 1000px

### Color Reference

**Buttons:** Always `status = "light"`

**UI Grays:**
- Primary: `#333`
- Secondary: `#6c757d`
- Help: `#666`
- Border: `#ddd`
- Background: `#f8f9fa`

### Typography Scale

- Headers: `1.1rem`, `font-weight: 600`
- Help: `0.875rem`

---

## Best Practices Summary

✅ **Do:**
- Use `block_responsive_css()` for all blocks
- Keep buttons `status = "light"`
- Maintain uniform input widths
- Use clear, descriptive labels
- Add help text for complex options

❌ **Don't:**
- Use flashy button colors
- Mix different width systems
- Skip help text when needed
- Use bright, saturated colors

---

## Related Documentation

- **Block Development:** [blocks-core-guide.md](blocks-core-guide.md)
- **Example Blocks:**
  - [filter.R](../R/filter.R) - Basic filter block
  - [mutate.R](../R/mutate.R) - Column transformation
  - [select.R](../R/select.R) - Column selection
  - [join.R](../R/join.R) - Join operations

---

**Remember:** The goal is a clean, professional, minimalist interface that adapts gracefully to any width. Let the responsive grid system do the work!
