#' CSS Utilities for blockr.dplyr Blocks
#'
#' This file provides centralized CSS functions for consistent block styling.
#' All custom classes use the `.block-` prefix to distinguish them from
#' framework classes (Bootstrap, Shiny, etc.) and prevent naming conflicts.
#'
#' **Required for all blocks:**
#' - `css_responsive_grid()` - Base grid layout and block styling (REQUIRED)
#'
#' **Common optional utilities:**
#' - `css_single_column()` - Force single-column layout (common)
#' - `css_advanced_toggle()` - Collapsible sections with toggle (optional)
#' - `css_inline_checkbox()` - Checkbox/label styling for inline layouts (optional)
#'
#' **Usage in block UI:**
#' ```r
#' ui = function(id) {
#'   tagList(
#'     css_responsive_grid(),        # Always include first
#'     css_single_column("myblock"), # If single column layout needed
#'     css_inline_checkbox(),        # If using inline checkboxes
#'     # ... block-specific CSS with tags$style(HTML(...)) ...
#'     # ... block HTML structure ...
#'   )
#' }
#' ```
#'
#' Block-specific styling (unique to one block) should use `tags$style(HTML(...))`
#' directly in the block file, not added here. Only add CSS here if it's
#' reused by 2+ blocks.
#'
#' @name css-utilities
#' @keywords internal
NULL

#' Responsive grid layout CSS for blocks
#'
#' Creates CSS for responsive grid layout with consistent styling.
#' This is the foundation CSS that **must** be loaded by all blocks.
#'
#' Defines the following classes:
#' - `.block-container` - Main container with padding
#' - `.block-form-grid` - Responsive grid layout
#' - `.block-section`, `.block-section-grid` - Flattened wrappers for grid
#' - `.block-help-text` - Gray help text styling
#' - `.block-input-wrapper` - Input field wrapper
#'
#' @return HTML style tag with responsive grid CSS
#' @noRd
#' @examples
#' \dontrun{
#' # In a block's UI function:
#' tagList(
#'   css_responsive_grid(),
#'   # ... rest of UI ...
#' )
#' }
css_responsive_grid <- function() {
  tags$style(HTML(
    "
    .block-container {
      width: 100%;
      margin: 0px;
      padding: 0px;
      padding-bottom: 15px;
    }

    /* One shared grid across the whole form */
    .block-form-grid {
      display: grid;
      gap: 15px;
      grid-template-columns: repeat(auto-fit, minmax(250px, 1fr));
    }

    /* Flatten wrappers so all controls share the same tracks */
    .block-section,
    .block-section-grid {
      display: contents;
    }

    /* Headings/help span full width */
    .block-section h4,
    .block-help-text {
      grid-column: 1 / -1;
    }

    .block-section h4 {
      margin-top: 5px;
      margin-bottom: 0px;
      font-size: 1.1rem;
      font-weight: 600;
      color: #333;
    }

    .block-section:not(:first-child) {
      margin-top: 20px;
    }

    .block-input-wrapper {
      width: 100%;
    }

    .block-input-wrapper .form-group {
      margin-bottom: 10px;
    }

    .block-help-text {
      margin-top: 0px;
      padding-top: 0px;
      font-size: 0.875rem;
      color: #666;
    }
    "
  ))
}

#' Force single-column layout for a block
#'
#' Many blocks need a single column layout regardless of screen width.
#' This helper provides the standard CSS override to force 1-column grid.
#'
#' Used by: filter, arrange, mutate, rename, select, summarize, join, value_filter
#'
#' @param block_name Character string, name of the block (e.g., "filter", "select")
#' @return HTML style tag with single-column grid CSS
#' @noRd
#' @examples
#' \dontrun{
#' # In a block's UI function:
#' tagList(
#'   css_responsive_grid(),
#'   css_single_column("filter"),
#'   # ... rest of UI ...
#' )
#' }
css_single_column <- function(block_name) {
  tags$style(HTML(sprintf(
    "
    .%s-block-container .block-form-grid {
      grid-template-columns: 1fr !important;
    }
    ",
    block_name
  )))
}

#' CSS for collapsible advanced options section
#'
#' Provides standardized CSS for expandable/collapsible sections with
#' animated chevron indicator. Used by blocks with optional advanced settings
#' that can be shown/hidden by the user.
#'
#' Defines the following classes:
#' - `.block-advanced-toggle` - Clickable toggle button with chevron
#' - `.block-chevron` - Chevron icon (► symbol)
#' - `.block-chevron.rotated` - Rotated chevron when expanded (▼ symbol)
#' - `#{id}` and `#{id}.expanded` - Max-height transitions for content
#'
#' The toggle works via JavaScript that:
#' 1. Toggles `.expanded` class on the content div
#' 2. Toggles `.rotated` class on the chevron
#'
#' Used by: summarize, bind_rows
#'
#' @param id Character string, the namespaced ID for the advanced options div.
#'   Should be created with `NS(id, "advanced-options")` or similar.
#' @param use_subgrid Logical, whether to use CSS subgrid for better grid integration.
#'   Set to TRUE if the advanced options should participate in the parent grid layout.
#'   Default FALSE uses simple block layout.
#' @return HTML style tag with advanced toggle CSS
#' @noRd
#' @examples
#' \dontrun{
#' # In a block's UI function:
#' tagList(
#'   css_responsive_grid(),
#'   css_advanced_toggle(NS(id, "advanced-options"), use_subgrid = TRUE),
#'   # ... toggle button and collapsible content ...
#' )
#' }
css_advanced_toggle <- function(id, use_subgrid = FALSE) {
  subgrid_css <- if (use_subgrid) {
    "
    grid-column: 1 / -1;
    display: grid;
    grid-template-columns: subgrid;
    gap: 15px;
    "
  } else {
    ""
  }

  tags$style(HTML(sprintf(
    "
    #%s {
      max-height: 0;
      overflow: hidden;
      transition: max-height 0.3s ease-out;
      %s
    }
    #%s.expanded {
      max-height: 500px;
      overflow: visible;
      transition: max-height 0.5s ease-in;
    }
    .block-advanced-toggle {
      cursor: pointer;
      user-select: none;
      padding: 8px 0;
      margin-bottom: 8px;
      display: flex;
      align-items: center;
      gap: 6px;
      color: #6c757d;
      font-size: 0.875rem;
    }
    .block-chevron {
      transition: transform 0.2s;
      display: inline-block;
      font-size: 14px;
      font-weight: bold;
    }
    .block-chevron.rotated {
      transform: rotate(90deg);
    }
    ",
    id,
    subgrid_css,
    id
  )))
}

#' Common checkbox and label styling for inline layouts
#'
#' Provides standardized styling for inline checkbox patterns and small labels.
#' This is used when you have an input field with a checkbox next to it
#' (e.g., "Number of rows [ ] Use proportion").
#'
#' Defines the following classes:
#' - `.block-inline-checkbox-wrapper` - Flex container for input + checkbox
#' - `.block-inline-checkbox` - Checkbox container with styling
#' - `.block-label-small` - Small gray labels for secondary UI elements
#'
#' Used by: select, slice, join (for labels)
#'
#' @return HTML style tag with inline checkbox CSS
#' @noRd
#' @examples
#' \dontrun{
#' # In a block's UI function:
#' tagList(
#'   css_responsive_grid(),
#'   css_inline_checkbox(),
#'   # ... HTML using the classes ...
#'   div(
#'     class = "block-inline-checkbox-wrapper",
#'     numericInput(...),
#'     div(class = "block-inline-checkbox", checkboxInput(...))
#'   )
#' )
#' }
css_inline_checkbox <- function() {
  tags$style(HTML(
    "
    /* Inline layout for input + checkbox side-by-side */
    .block-inline-checkbox-wrapper {
      display: flex;
      align-items: flex-end;
      gap: 4px;
      flex-wrap: nowrap;
    }

    .block-inline-checkbox-wrapper > div:first-child {
      flex: 1;
      min-width: 0;
    }

    /* Inline checkbox container styling */
    .block-inline-checkbox {
      display: flex;
      align-items: center;
      margin-left: 0;
      margin-bottom: 5px;
    }

    .block-inline-checkbox .shiny-input-container {
      width: auto !important;
      max-width: none !important;
      margin-bottom: 0 !important;
    }

    .block-inline-checkbox .checkbox {
      margin-bottom: 0;
      margin-top: 0;
    }

    .block-inline-checkbox label {
      font-size: 0.75rem;
      color: #6c757d;
      font-weight: normal;
      margin-bottom: 0;
      padding-left: 4px;
    }

    .block-inline-checkbox input[type='checkbox'] {
      margin-top: 0;
      margin-right: 4px;
    }

    /* Small gray labels for secondary elements */
    .block-label-small {
      font-size: 0.75rem;
      color: #6c757d;
      margin-bottom: 2px;
    }
    "
  ))
}

#' CSS for documentation helper links
#'
#' Provides standardized styling for inline documentation links,
#' typically used to link to expression helpers or block documentation.
#'
#' Defines the following class:
#' - `.expression-help-link` - Styled documentation link with margins
#'
#' Used by: mutate, filter_expr, summarize
#'
#' @return HTML style tag with documentation link CSS
#' @noRd
#' @examples
#' \dontrun{
#' # In a block's UI function:
#' tagList(
#'   css_responsive_grid(),
#'   css_doc_links(),
#'   # ... HTML with documentation links ...
#'   div(
#'     class = "expression-help-link",
#'     tags$a(href = "...", target = "_blank", "Documentation \u2197")
#'   )
#' )
#' }
css_doc_links <- function() {
  tags$style(HTML(
    "
    .expression-help-link {
      margin-top: 0.25rem;
      margin-bottom: 0.5rem;
      display: block;
    }
    "
  ))
}
