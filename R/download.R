#' Gear icon SVG (Bootstrap Icons)
#'
#' Kept inline to avoid a cross-package dep on blockr.dplyr.
#' @noRd
download_block_gear_svg <- function() {
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

#' CSS + JS for the gear popover used by new_download_block()
#'
#' Class names mirror blockr.dplyr's `.blockr-gear-btn` / `.blockr-popover`
#' for visual consistency when both packages load in the same app.
#' @noRd
download_block_css <- function() {
  tagList(
    tags$style(HTML("
      .download-block-container {
        padding: 12px 14px;
      }
      .download-block-main {
        display: flex;
        align-items: center;
        justify-content: space-between;
        gap: 8px;
      }
      .download-block-main-left {
        display: flex;
        align-items: center;
        gap: 8px;
      }
      .download-block-main .btn {
        flex: 0 0 auto;
      }
      /* Download button — aligned to the format select on its right.
         Target values measured from the selectize-input computed style:
         height 42px, padding 5px 12px, radius 8px, bg #f9fafb. */
      .download-block-container .download-block-main-left .btn {
        height: 42px;
        padding: 5px 14px;
        display: inline-flex;
        align-items: center;
        gap: 6px;
        font-size: 14px;
        line-height: 14px;
        border-radius: 8px;
        background: #f9fafb;
        color: rgba(0, 0, 0, 0.83);
        border: 1px solid #e5e7eb;
        box-shadow: none;
      }
      .download-block-container .download-block-main-left .btn:hover {
        background: #f3f4f6;
        border-color: #d1d5db;
        color: rgba(0, 0, 0, 0.9);
      }
      .download-block-container .download-block-main-left .btn:focus,
      .download-block-container .download-block-main-left .btn:active {
        background: #f9fafb;
        border-color: #2563eb;
        box-shadow: 0 0 0 3px rgba(59, 130, 246, 0.1);
        color: rgba(0, 0, 0, 0.83);
      }
      .download-block-container .download-block-main-left .btn i,
      .download-block-container .download-block-main-left .btn .fa {
        color: #6b7280;
      }
      /* Tighten the inline format selector */
      .download-block-format .form-group,
      .download-block-format .shiny-input-container {
        margin-bottom: 0;
      }
      .download-block-format .selectize-control {
        margin-bottom: 0;
      }
      /* Gear button — matches blockr.dplyr */
      .download-block-container .blockr-gear-btn {
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
      .download-block-container .blockr-gear-btn:hover {
        color: #2563eb;
        border-color: rgba(37, 99, 235, 0.3);
        background: rgba(37, 99, 235, 0.04);
      }
      .download-block-container .blockr-gear-btn.blockr-gear-active {
        color: #2563eb;
        border-color: rgba(37, 99, 235, 0.3);
        background: rgba(37, 99, 235, 0.08);
      }
      /* Popover — matches blockr.dplyr */
      .download-block-container .blockr-popover {
        position: absolute;
        right: 14px;
        top: 56px;
        z-index: 1000;
        background: #ffffff;
        border: 1px solid #e5e7eb;
        border-radius: 8px;
        box-shadow: 0 4px 12px rgba(0, 0, 0, 0.1);
        padding: 12px 14px;
        min-width: 280px;
      }
      .download-block-container .blockr-popover-row {
        margin-bottom: 10px;
      }
      .download-block-container .blockr-popover-row:last-child {
        margin-bottom: 0;
      }
      .download-block-container .blockr-popover-label {
        display: block;
        font-size: 0.75rem;
        font-weight: 500;
        color: #6b7280;
        margin-bottom: 0.25rem;
      }
      /* Make Shiny inputs inside the popover compact and full width */
      .download-block-container .blockr-popover .form-group,
      .download-block-container .blockr-popover .shiny-input-container {
        margin-bottom: 0;
        width: 100% !important;
      }
      .download-block-container .blockr-popover .selectize-control {
        width: 100% !important;
        margin-bottom: 0;
      }
    ")),
    # Idempotent toggle helper — safe to include in multiple block instances.
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
          document.querySelectorAll('.download-block-container .blockr-popover').forEach(function(pop) {
            if (pop.style.display === 'none') return;
            var container = pop.closest('.download-block-container');
            if (!container || container.contains(e.target)) return;
            pop.style.display = 'none';
            var gear = container.querySelector('.blockr-gear-btn');
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


#' Download-only file export block
#'
#' A variadic block that lets the user download one or more data frames as a
#' file in the browser. Intended as a lightweight counterpart to
#' [`new_write_block()`] for the common case where no server-side save is
#' needed.
#'
#' @param filename Character. Optional fixed filename (without extension).
#'   If empty (default), a timestamped filename is generated.
#' @param format Character. One of the values in [`write_formats()`]:
#'   `"csv"`, `"excel"`, `"parquet"`, or `"feather"`. Default: `"csv"`.
#' @param args Named list of format-specific writing parameters (same as
#'   [`new_write_block()`]). Only relevant values for the selected format are
#'   used.
#' @param ... Forwarded to [`blockr.core::new_transform_block()`].
#'
#' @details
#' Multi-input behavior matches [`new_write_block()`]: multiple inputs produce
#' a multi-sheet Excel file for `format = "excel"`, or a ZIP archive for CSV,
#' Parquet, and Feather.
#'
#' Adding a new format (e.g. SAS) only requires extending
#' [`write_formats()`], `format_extension()`, and the dispatch in
#' `write_expr()` — both [`new_write_block()`] and [`new_download_block()`]
#' pick it up automatically.
#'
#' @return A blockr transform block exposing a download button.
#'
#' @examples
#' if (interactive()) {
#'   library(blockr.core)
#'   serve(new_download_block())
#' }
#'
#' @rdname download
#' @export
new_download_block <- function(
  filename = "",
  format = "csv",
  args = list(),
  ...
) {
  format <- match.arg(format, unname(write_formats()))

  new_transform_block(
    server = function(id, ...args) {
      moduleServer(
        id,
        function(input, output, session) {
          arg_names <- reactive({
            names_vec <- names(...args)
            set_names(names_vec, dot_args_names(...args))
          })

          r_filename <- reactiveVal(filename)
          r_format <- reactiveVal(format)
          r_args <- reactiveVal(args)

          observeEvent(input$filename, r_filename(input$filename))
          observeEvent(input$format, r_format(input$format))

          observeEvent(input$csv_sep, {
            current <- r_args()
            current$sep <- input$csv_sep
            r_args(current)
          })
          observeEvent(input$csv_quote, {
            current <- r_args()
            current$quote <- input$csv_quote
            r_args(current)
          })
          observeEvent(input$csv_na, {
            current <- r_args()
            current$na <- input$csv_na
            r_args(current)
          })

          output$download_data <- downloadHandler(
            filename = function() {
              base <- generate_filename(r_filename())
              needs_zip <- length(arg_names()) > 1 && r_format() != "excel"
              paste0(base, format_extension(r_format(), needs_zip = needs_zip))
            },
            content = function(file) {
              fixed_timestamp <- Sys.time()
              base_filename <- generate_filename(r_filename(), fixed_timestamp)

              temp_dir <- dirname(file)
              expr <- write_expr(
                data_names = arg_names(),
                directory = temp_dir,
                filename = base_filename,
                format = r_format(),
                args = r_args()
              )

              eval_env <- new.env(parent = parent.frame())
              names_vec <- arg_names()
              ll <- reactiveValuesToList(...args)

              for (i in seq_along(names_vec)) {
                arg_i <- ll[[i]]
                data_val <- if (is.reactive(arg_i)) arg_i() else arg_i
                assign(names_vec[i], data_val, envir = eval_env)
              }

              eval(expr, envir = eval_env)

              generated_file <- list.files(
                temp_dir,
                pattern = paste0(
                  "^",
                  gsub("\\.", "\\\\.", base_filename),
                  "\\."
                ),
                full.names = TRUE
              )[1]

              if (!is.na(generated_file) && file.exists(generated_file)) {
                if (normalizePath(generated_file) !=
                      normalizePath(file, mustWork = FALSE)) {
                  file.copy(generated_file, file, overwrite = TRUE)
                }
              }
            }
          )

          output$show_csv_options <- reactive({
            identical(r_format(), "csv")
          })
          outputOptions(output, "show_csv_options", suspendWhenHidden = FALSE)

          list(
            expr = reactive(NULL),
            state = list(
              filename = r_filename,
              format = r_format,
              args = r_args
            )
          )
        }
      )
    },
    ui = function(id) {
      gear_id <- NS(id, "gear_btn")
      popover_id <- NS(id, "gear_popover")
      tagList(
        download_block_css(),
        div(
          class = "block-container download-block-container",
          style = "position: relative;",

          # Main row: download button + format selector on the left, gear on the right
          div(
            class = "download-block-main",
            div(
              class = "download-block-main-left",
              downloadButton(
                NS(id, "download_data"),
                "Download",
                class = "btn-primary"
              ),
              div(
                class = "download-block-format",
                selectInput(
                  inputId = NS(id, "format"),
                  label = NULL,
                  choices = write_formats(),
                  selected = format,
                  width = "140px"
                )
              )
            ),
            tags$button(
              type = "button",
              id = gear_id,
              class = "blockr-gear-btn",
              title = "Advanced settings",
              `aria-label` = "Advanced settings",
              `aria-controls` = popover_id,
              `aria-expanded` = "false",
              onclick = sprintf(
                "window.blockrIoGearToggle && window.blockrIoGearToggle('%s','%s');",
                gear_id, popover_id
              ),
              HTML(download_block_gear_svg())
            )
          ),

          # Popover: filename + CSV args
          div(
            id = popover_id,
            class = "blockr-popover",
            style = "display: none;",
            role = "dialog",
            `aria-label` = "Download settings",

            div(
              class = "blockr-popover-row",
              tags$label(
                class = "blockr-popover-label",
                `for` = NS(id, "filename"),
                "Filename (optional)"
              ),
              textInput(
                inputId = NS(id, "filename"),
                label = NULL,
                value = filename,
                placeholder = "Leave empty for auto-timestamp",
                width = "100%"
              )
            ),

            conditionalPanel(
              condition = "output['show_csv_options']",
              ns = NS(id),

              div(
                class = "blockr-popover-row",
                tags$label(
                  class = "blockr-popover-label",
                  `for` = NS(id, "csv_sep"),
                  "Delimiter"
                ),
                selectizeInput(
                  inputId = NS(id, "csv_sep"),
                  label = NULL,
                  choices = c(
                    "Comma (,)" = ",",
                    "Semicolon (;)" = ";",
                    "Tab (\\t)" = "\t",
                    "Pipe (|)" = "|"
                  ),
                  selected = if (!is.null(args$sep)) args$sep else ",",
                  options = list(create = TRUE),
                  width = "100%"
                )
              ),
              div(
                class = "blockr-popover-row",
                checkboxInput(
                  inputId = NS(id, "csv_quote"),
                  label = "Quote strings",
                  value = if (!is.null(args$quote)) args$quote else TRUE
                )
              ),
              div(
                class = "blockr-popover-row",
                tags$label(
                  class = "blockr-popover-label",
                  `for` = NS(id, "csv_na"),
                  "NA representation"
                ),
                textInput(
                  inputId = NS(id, "csv_na"),
                  label = NULL,
                  value = if (!is.null(args$na)) args$na else "",
                  placeholder = "default: empty string",
                  width = "100%"
                )
              )
            )
          )
        )
      )
    },
    dat_valid = function(...args) {
      stopifnot(length(...args) >= 1L)
    },
    allow_empty_state = TRUE,
    class = c("download_block", "rbind_block"),
    ...
  )
}
