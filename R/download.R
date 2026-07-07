
#' Download-block layout CSS
#'
#' Layout rules specific to `new_download_block()` (button row, inline format
#' selector). Shared gear/popover styling lives in [`css_gear_popover()`].
#' @noRd
download_block_css <- function() {
  tagList(
    css_gear_popover(),
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
      /* Download button - aligned to the format select on its right.
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
#' `write_expr()` - both [`new_write_block()`] and [`new_download_block()`]
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
          # Eval-env reference names for the connected inputs: the link name
          # for named slots, ".arg1", ".arg2", ... for unnamed ones (added via
          # the DAG UI). Reactive on the link set.
          arg_names <- reactive({
            dot_arg_refs(...args)
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

              # Bind each input under its reference symbol (.arg1 for unnamed
              # DAG-UI slots, else the link name) — the same names write_expr()
              # emits. dot_arg_values() handles both the live-board reactives
              # and the reactiveValues used in tests.
              eval_env <- new.env(parent = parent.frame())
              arg_vals <- dot_arg_values(...args)

              for (nm in names(arg_vals)) {
                data_val <- arg_vals[[nm]]
                assign(
                  nm,
                  if (is.reactive(data_val)) data_val() else data_val,
                  envir = eval_env
                )
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

          # Main row: download button + format selector on the left, gear on the right
          div(
            class = "download-block-main blockr-gear-host",
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
              HTML(gear_icon_svg())
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
