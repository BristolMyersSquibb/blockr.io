#' CSS Utilities for blockr.io Blocks
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
#' - `css_gear_popover()` - Gear-button + popover (cogwheel) for advanced settings
#' - `css_advanced_toggle()` - Legacy chevron-collapse toggle (kept for backwards compat)
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
#' Used by: legacy callers only. New blocks should prefer `css_gear_popover()`.
#'
#' @param id Character string, the namespaced ID for the advanced options div.
#'   Should be created with `NS(id, "advanced-options")` or similar.
#' @param use_subgrid Logical, whether to use CSS subgrid for better grid integration.
#'   Set to TRUE if the advanced options should participate in the parent grid layout.
#'   Default FALSE uses simple block layout.
#' @return HTML style tag with advanced toggle CSS
#' @noRd
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

#' Inline gear icon SVG (Bootstrap Icons)
#'
#' Kept inline so blockr.io has no cross-package dep on blockr.dplyr for the
#' icon. Used by every block that exposes a `.blockr-gear-btn`.
#'
#' @return Character string containing the SVG markup.
#' @noRd
gear_icon_svg <- function() {
  paste0(
    '<svg xmlns="http://www.w3.org/2000/svg" width="14" height="14" fill="currentColor" ',
    'viewBox="0 0 16 16"><path d="M9.405 1.05c-.413-1.4-2.397-1.4-2.81 0l-.1.34a1.464 ',
    '1.464 0 0 1-2.105.872l-.31-.17c-1.283-.698-2.686.705-1.987 1.987l.169.311c.446.82',
    '.023 1.841-.872 2.105l-.34.1c-1.4.413-1.4 2.397 0 2.81l.34.1a1.464 1.464 0 0 1 ',
    '.872 2.105l-.17.31c-.698 1.283.705 2.686 1.987 1.987l.311-.169a1.464 1.464 0 0 1 ',
    '2.105.872l.1.34c.413 1.4 2.397 1.4 2.81 0l.1-.34a1.464 1.464 0 0 1 2.105-.872l.31',
    '.17c1.283.698 2.686-.705 1.987-1.987l-.169-.311a1.464 1.464 0 0 1 .872-2.105l.34-',
    '.1c1.4-.413 1.4-2.397 0-2.81l-.34-.1a1.464 1.464 0 0 1-.872-2.105l.17-.31c.698-',
    '1.283-.705-2.686-1.987-1.987l-.311.169a1.464 1.464 0 0 1-2.105-.872zM8 10.93a2.929 ',
    '2.929 0 1 1 0-5.86 2.929 2.929 0 0 1 0 5.858z"/></svg>'
  )
}

#' CSS + JS for the gear-button / popover (cogwheel) pattern
#'
#' Shared by `new_read_block()`, `new_write_block()`, and `new_download_block()`.
#' Class names mirror `blockr.dplyr` so visual style matches when both packages
#' load in the same app. The host element (the block container, or any
#' relatively-positioned wrapper that owns the gear + popover) must carry the
#' `.blockr-gear-host` class so the outside-click handler can dismiss the right
#' popover.
#'
#' Defines:
#' - `.blockr-gear-btn` - 32px square icon button (idle / hover / active states)
#' - `.blockr-popover` - absolute-positioned panel, anchored to its gear host
#' - `.blockr-popover-row` / `.blockr-popover-label` - internal row layout
#'
#' Also installs the idempotent `window.blockrIoGearToggle(gearId, popId)`
#' helper and a single document-level click listener that closes any open
#' popover when the user clicks outside its `.blockr-gear-host`.
#'
#' @return HTML style + script tags wrapped in a `tagList`.
#' @noRd
css_gear_popover <- function() {
  tagList(
    tags$style(HTML("
      .blockr-gear-btn {
        display: inline-flex;
        align-items: center;
        justify-content: center;
        width: 32px;
        height: 32px;
        color: #9ca3af;
        background: transparent;
        border: 1px solid #e5e7eb;
        border-radius: 4px;
        cursor: pointer;
        transition: color 0.15s ease, border-color 0.15s ease, background 0.15s ease;
      }
      .blockr-gear-btn:hover {
        color: #2563eb;
        border-color: rgba(37, 99, 235, 0.3);
        background: rgba(37, 99, 235, 0.04);
      }
      .blockr-gear-btn.blockr-gear-active {
        color: #2563eb;
        border-color: rgba(37, 99, 235, 0.3);
        background: rgba(37, 99, 235, 0.08);
      }
      .blockr-gear-host {
        position: relative;
      }
      .blockr-gear-row {
        display: flex;
        justify-content: flex-end;
      }
      .blockr-popover {
        position: absolute;
        right: 0;
        top: calc(100% + 4px);
        z-index: 1000;
        background: #ffffff;
        border: 1px solid #e5e7eb;
        border-radius: 8px;
        box-shadow: 0 4px 12px rgba(0, 0, 0, 0.1);
        padding: 12px 14px;
        min-width: 320px;
        max-width: 380px;
        max-height: 70vh;
        overflow-y: auto;
      }
      .blockr-popover-row {
        margin-bottom: 10px;
      }
      .blockr-popover-row:last-child {
        margin-bottom: 0;
      }
      .blockr-popover-label {
        display: block;
        font-size: 0.75rem;
        font-weight: 500;
        color: #6b7280;
        margin-bottom: 0.25rem;
      }
      .blockr-popover h4 {
        font-size: 0.8125rem;
        font-weight: 600;
        color: #374151;
        margin: 12px 0 6px;
      }
      .blockr-popover h4:first-child {
        margin-top: 0;
      }
      .blockr-popover .form-group,
      .blockr-popover .shiny-input-container {
        margin-bottom: 0;
        width: 100% !important;
      }
      .blockr-popover .selectize-control {
        width: 100% !important;
        margin-bottom: 0;
      }
    ")),
    tags$script(HTML("
      (function() {
        if (window.blockrIoGearToggle) return;
        window.blockrIoGearToggle = function(gearId, popId) {
          var gear = document.getElementById(gearId);
          var pop  = document.getElementById(popId);
          if (!gear || !pop) return;
          var open = pop.style.display !== 'none';
          if (open) {
            pop.style.display = 'none';
            gear.classList.remove('blockr-gear-active');
            gear.setAttribute('aria-expanded', 'false');
          } else {
            pop.style.display = 'block';
            gear.classList.add('blockr-gear-active');
            gear.setAttribute('aria-expanded', 'true');
          }
        };
        document.addEventListener('click', function(e) {
          document.querySelectorAll('.blockr-gear-host .blockr-popover').forEach(function(pop) {
            if (pop.style.display === 'none') return;
            var host = pop.closest('.blockr-gear-host');
            if (!host || host.contains(e.target)) return;
            pop.style.display = 'none';
            var gear = host.querySelector('.blockr-gear-btn');
            if (gear) {
              gear.classList.remove('blockr-gear-active');
              gear.setAttribute('aria-expanded', 'false');
            }
          });
        });
      })();
    "))
  )
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
