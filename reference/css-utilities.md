# CSS Utilities for blockr.io Blocks

This file provides centralized CSS functions for consistent block
styling. All custom classes use the `.block-` prefix to distinguish them
from framework classes (Bootstrap, Shiny, etc.) and prevent naming
conflicts.

## Details

**Required for all blocks:**

- `css_responsive_grid()` - Base grid layout and block styling
  (REQUIRED)

**Common optional utilities:**

- `css_single_column()` - Force single-column layout (common)

- `css_advanced_toggle()` - Collapsible sections with toggle (optional)

- `css_inline_checkbox()` - Checkbox/label styling for inline layouts
  (optional)

**Usage in block UI:**

    ui = function(id) {
      tagList(
        css_responsive_grid(),        # Always include first
        css_single_column("myblock"), # If single column layout needed
        css_inline_checkbox(),        # If using inline checkboxes
        # ... block-specific CSS with tags$style(HTML(...)) ...
        # ... block HTML structure ...
      )
    }

Block-specific styling (unique to one block) should use
`tags$style(HTML(...))` directly in the block file, not added here. Only
add CSS here if it's reused by 2+ blocks.
