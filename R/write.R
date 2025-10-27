#' Unified file writing block
#'
#' A variadic block for writing dataframes to files in various formats.
#' Accepts multiple input dataframes and handles single files, multi-sheet
#' Excel, or ZIP archives depending on format and number of inputs.
#'
#' @param directory Character. Default directory for file output (browse mode only).
#'   Can be configured via `options(blockr.write_dir = "/path")` or environment
#'   variable `BLOCKR_WRITE_DIR`. Default: current working directory.
#' @param filename Character. Optional fixed filename (without extension).
#'   - **If provided**: Writes to same file path on every upstream change (auto-overwrite)
#'   - **If empty** (default): Generates timestamped filename (e.g., `data_20250127_143022.csv`)
#' @param format Character. Output format: "csv", "excel", "parquet", or "feather".
#'   Default: "csv"
#' @param mode Character. Either "browse" to write files to server filesystem,
#'   or "download" to trigger browser download. Default: "browse"
#' @param args Named list of format-specific writing parameters. Only specify values
#'   that differ from defaults. Available parameters:
#'   - **For CSV files:** `sep` (default: ","), `quote` (default: TRUE),
#'     `na` (default: "")
#'   - **For Excel/Arrow:** Minimal options needed (handled by underlying packages)
#' @param ... Forwarded to [blockr.core::new_transform_block()]
#'
#' @details
#' ## Variadic Inputs
#'
#' This block accepts multiple dataframe inputs (1 or more) similar to `bind_rows_block`.
#' Inputs can be numbered ("1", "2", "3") or named ("sales_data", "inventory").
#' Input names are used for sheet names (Excel) or filenames (multi-file ZIP).
#'
#' ## File Output Behavior
#'
#' **Single input:**
#' - Writes single file in specified format
#' - Filename: `{filename}.{ext}` or `data_{timestamp}.{ext}`
#'
#' **Multiple inputs + Excel:**
#' - Single Excel file with multiple sheets
#' - Sheet names derived from input names
#'
#' **Multiple inputs + CSV/Arrow:**
#' - Single ZIP file containing individual files
#' - Each file named from input names
#'
#' ## Filename Behavior
#'
#' **Fixed filename** (`filename = "output"`):
#' - Reproducible path: always writes to `{directory}/output.{ext}`
#' - Overwrites file on every upstream data change
#' - Ideal for automated pipelines
#'
#' **Auto-timestamped** (`filename = ""`):
#' - Unique files: `{directory}/data_YYYYMMDD_HHMMSS.{ext}`
#' - Preserves history, prevents accidental overwrites
#' - Safe default behavior
#'
#' ## Mode: Browse vs Download
#'
#' **Browse mode:**
#' - Writes files to server filesystem
#' - User selects directory with file browser
#' - Files persist on server
#'
#' **Download mode:**
#' - Generates file and triggers browser download
#' - File saved to user's download folder
#' - Useful for exporting results
#'
#' @return A blockr transform block that writes dataframes to files
#'
#' @examples
#' \dontrun{
#' # Single dataframe to CSV
#' serve(new_write_block(
#'   directory = "/tmp",
#'   filename = "output",
#'   format = "csv"
#' ))
#'
#' # Multiple dataframes to Excel with auto-timestamp
#' serve(new_write_block(
#'   directory = "/tmp",
#'   filename = "", # auto-generated
#'   format = "excel"
#' ))
#'
#' # CSV with custom delimiter
#' serve(new_write_block(
#'   directory = "/tmp",
#'   filename = "data",
#'   format = "csv",
#'   args = list(sep = ";")
#' ))
#' }
#'
#' @importFrom shinyFiles shinyDirButton shinyDirChoose parseDirPath
#' @rdname write
#' @export
new_write_block <- function(
  directory = "",
  filename = "",
  format = "csv",
  mode = "browse",
  args = list(),
  ...
) {
  # Validate parameters
  format <- match.arg(format, c("csv", "excel", "parquet", "feather"))
  mode <- match.arg(mode, c("browse", "download"))

  # Get default directory from options if not provided
  if (directory == "") {
    directory <- blockr_option(
      "write_dir",
      Sys.getenv("BLOCKR_WRITE_DIR", getwd())
    )
  }

  # Get volumes for directory browser
  volumes <- blockr_option("volumes", c(home = path.expand("~")))

  # Handle volumes parameter
  if (is.character(volumes)) {
    volumes <- path.expand(volumes)
  }

  if (is_string(volumes) && grepl(":", volumes)) {
    volumes <- strsplit(volumes, ":", fixed = TRUE)[[1L]]
  }

  if (is.null(names(volumes))) {
    if (length(volumes) == 1L) {
      names(volumes) <- "volume"
    } else if (length(volumes) > 1L) {
      names(volumes) <- paste0("volume", seq_along(volumes))
    }
  }

  # Expand directory path
  directory <- path.expand(directory)

  new_transform_block(
    server = function(id, ...args) {
      moduleServer(
        id,
        function(input, output, session) {
          # volumes and directory available here via closure

          # Extract arg names for variadic inputs
          arg_names <- reactive({
            names_vec <- names(...args)
            set_names(names_vec, dot_args_names(...args))
          })

          # Reactive values for state
          r_directory <- reactiveVal(directory)
          r_filename <- reactiveVal(filename)
          r_format <- reactiveVal(format)
          r_mode <- reactiveVal(mode)
          r_args <- reactiveVal(args)
          r_last_write <- reactiveVal(NULL) # Track last write time
          r_write_status <- reactiveVal("") # Status message

          # Initialize shinyFiles directory browser
          shinyFiles::shinyDirChoose(
            input,
            "dir_browser",
            roots = volumes,
            session = session
          )

          # Handle directory browser selection
          selected_dir <- reactive({
            if (!is.null(input$dir_browser) && !identical(input$dir_browser, "")) {
              path <- shinyFiles::parseDirPath(volumes, input$dir_browser)
              if (length(path) > 0) path else NULL
            } else {
              NULL
            }
          })

          observeEvent(selected_dir(), {
            if (!is.null(selected_dir())) {
              r_directory(selected_dir())
            }
          })

          # Update state from inputs
          observeEvent(input$mode_pills, {
            if (!is.null(input$mode_pills)) {
              r_mode(input$mode_pills)
            }
          })

          observeEvent(input$filename, r_filename(input$filename))
          observeEvent(input$format, r_format(input$format))

          # CSV parameter updates
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

          # Reactive to store validated write expression
          r_write_expression_validated <- reactiveVal(NULL)

          # Submit button for browse mode - writes file when clicked
          observeEvent(input$submit_write, {
            req(length(arg_names()) > 0)
            req(r_directory())
            req(r_mode() == "browse")

            # Generate write expression
            expr <- write_expr(
              data_names = arg_names(),
              directory = r_directory(),
              filename = r_filename(),
              format = r_format(),
              args = r_args()
            )

            # Create environment with data
            eval_env <- new.env(parent = parent.frame())
            names_vec <- arg_names()
            ll <- reactiveValuesToList(...args)

            for (i in seq_along(names_vec)) {
              arg_i <- ll[[i]]
              data_val <- if (is.reactive(arg_i)) {
                arg_i()
              } else {
                arg_i
              }
              assign(names_vec[i], data_val, envir = eval_env)
            }

            # Ensure directory exists
            dir.create(r_directory(), recursive = TRUE, showWarnings = FALSE)

            # Execute write
            tryCatch(
              {
                eval(expr, envir = eval_env)
                r_write_status(sprintf(
                  "\u2713 Written at %s",
                  format(Sys.time(), "%H:%M:%S")
                ))

                # Store expression for pipeline
                first_data <- as.name(arg_names()[1])
                r_write_expression_validated(bquote({
                  .(expr)
                  .(first_data)
                }))
              },
              error = function(e) {
                r_write_status(sprintf("\u2717 Error: %s", conditionMessage(e)))
              }
            )
          })

          # Generate expression that adapts based on mode
          r_write_expression <- reactive({
            if (r_mode() == "browse") {
              # Browse mode: Return last validated expression or identity
              if (!is.null(r_write_expression_validated())) {
                r_write_expression_validated()
              } else {
                # Before first submit, just return identity
                req(length(arg_names()) > 0)
                bquote(identity(.(as.name(arg_names()[1]))))
              }
            } else {
              # Download mode: Return identity() - valid do-nothing expression
              # Download handler will do the actual writing when user clicks download
              req(length(arg_names()) > 0)
              bquote(identity(.(as.name(arg_names()[1]))))
            }
          })


          # # Execute write when in browse mode and data changes
          # observeEvent(
          #   {
          #     list(r_write_expression(), ...args)
          #   },
          #   {
          #     if (r_mode() == "browse") {
          #       req(r_write_expression())

          #       tryCatch(
          #         {
          #           # Ensure directory exists
          #           dir.create(r_directory(), recursive = TRUE, showWarnings = FALSE)

          #           # Create environment with actual data for evaluation
          #           eval_env <- new.env(parent = parent.frame())
          #           names_vec <- arg_names()
          #           for (i in seq_along(...args)) {
          #             data_val <- if (is.reactive(...args[[i]])) {
          #               ...args[[i]]()
          #             } else {
          #               ...args[[i]]
          #             }
          #             assign(names_vec[i], data_val, envir = eval_env)
          #           }

          #           # Evaluate write expression in environment with data
          #           eval(r_write_expression(), envir = eval_env)

          #           # Update status
          #           r_last_write(Sys.time())
          #           r_write_status(sprintf(
          #             "\u2713 Written at %s",
          #             format(Sys.time(), "%H:%M:%S")
          #           ))
          #         },
          #         error = function(e) {
          #           r_write_status(sprintf("\u2717 Error: %s", conditionMessage(e)))
          #         }
          #       )
          #     }
          #   },
          #   ignoreInit = TRUE
          # )

          # Download handler for download mode
          output$download_data <- downloadHandler(
            filename = function() {
              base <- generate_filename(r_filename())
              needs_zip <- length(arg_names()) > 1 && r_format() != "excel"

              if (needs_zip) {
                paste0(base, ".zip")
              } else {
                ext <- switch(r_format(),
                  csv = ".csv",
                  excel = ".xlsx",
                  parquet = ".parquet",
                  feather = ".feather"
                )
                paste0(base, ext)
              }
            },
            content = function(file) {
              # Generate write expression for temp directory
              temp_dir <- dirname(file)
              expr <- write_expr(
                data_names = arg_names(),
                directory = temp_dir,
                filename = r_filename(),
                format = r_format(),
                args = r_args()
              )

              # Create environment with parent.frame() as parent
              eval_env <- new.env(parent = parent.frame())

              # Extract data from ...args and add to eval_env
              names_vec <- arg_names()
              ll <- reactiveValuesToList(...args)

              for (i in seq_along(names_vec)) {
                arg_i <- ll[[i]]
                data_val <- if (is.reactive(arg_i)) {
                  arg_i()
                } else {
                  arg_i
                }
                assign(names_vec[i], data_val, envir = eval_env)
              }

              # Evaluate write expression - this writes the file(s)
              eval(expr, envir = eval_env)

              # Find generated file and copy to download location
              generated_file <- list.files(
                temp_dir,
                pattern = paste0(
                  "^",
                  gsub("\\.", "\\\\.", generate_filename(r_filename())),
                  "\\."
                ),
                full.names = TRUE
              )[1]

              if (!is.na(generated_file) && file.exists(generated_file)) {
                file.copy(generated_file, file, overwrite = TRUE)
              }
            }
          )

          # Output: Current directory display
          output$current_directory <- renderText({
            dir <- r_directory()
            if (!is.null(dir) && nzchar(dir)) {
              paste("Current directory:", dir)
            } else {
              "No directory selected"
            }
          })

          # Output: Write status display
          output$write_status <- renderText({
            r_write_status()
          })

          # Output: Show/hide format-specific options
          output$show_csv_options <- reactive({
            identical(r_format(), "csv")
          })

          outputOptions(output, "show_csv_options", suspendWhenHidden = FALSE)

          list(
            expr = r_write_expression,
            state = list(
              directory = r_directory,
              filename = r_filename,
              format = r_format,
              mode = r_mode,
              args = r_args
            )
          )
        }
      )
    },
    ui = function(id) {
      tagList(
        shinyjs::useShinyjs(),

        # Add CSS
        css_responsive_grid(),
        css_advanced_toggle(NS(id, "advanced-options"), use_subgrid = TRUE),
        div(
          class = "block-container write-block-container",

          # Mode selector
          div(
            class = "block-section",
            tags$h4("Output Mode", class = "mb-3"),
            tags$style(HTML(
              "
              .nav-pills {
                display: inline-flex;
                overflow: hidden;
              }
              .nav-pills .nav-link {
                background-color: rgb(249, 249, 250);
                color: rgb(104, 107, 130);
                border: none;
                border-radius: 8px;
                margin: 8px;
                margin-left:0;
                padding: 6px 10px;
                font-size: 0.8rem;
              }
              .nav-pills .nav-link:hover {
                background-color: #f8f9fa;
                z-index: 1;
              }
              .nav-pills .nav-link.active {
                background-color: rgb(236, 236, 236);
                color: rgb(104, 107, 130);
                border-color: rgb(236, 236, 236);
                z-index: 2;
              }
            "
            )),
            bslib::navset_pill(
              id = NS(id, "mode_pills"),
              selected = mode,
              bslib::nav_panel(
                title = "Browse",
                value = "browse",
                div(
                  class = "mt-3",
                  div(
                    class = "block-help-text mb-3",
                    "Write files to server filesystem. Click Submit to write."
                  ),
                  shinyFiles::shinyDirButton(
                    NS(id, "dir_browser"),
                    label = "Select Directory...",
                    title = "Choose output directory",
                    multiple = FALSE
                  ),
                  div(
                    class = "block-help-text mt-2",
                    textOutput(NS(id, "current_directory"))
                  ),
                  div(
                    class = "mt-3",
                    actionButton(
                      NS(id, "submit_write"),
                      "Submit",
                      class = "btn-primary btn-sm"
                    )
                  )
                )
              ),
              bslib::nav_panel(
                title = "Download",
                value = "download",
                div(
                  class = "mt-3",
                  div(
                    class = "block-help-text mb-3",
                    "Download file(s) to your browser's download folder."
                  ),
                  downloadButton(
                    NS(id, "download_data"),
                    "Download File",
                    class = "btn-primary"
                  )
                )
              )
            )
          ),

          # File Configuration
          div(
            class = "block-form-grid",
            div(
              class = "block-section",
              tags$h4("File Configuration"),
              div(
                class = "block-section-grid",
                div(
                  class = "block-input-wrapper",
                  textInput(
                    inputId = NS(id, "filename"),
                    label = "Filename (optional)",
                    value = filename,
                    placeholder = "Leave empty for auto-timestamp"
                  ),
                  div(
                    class = "block-help-text",
                    "Fixed filename overwrites on each change. Empty generates unique timestamped files."
                  )
                ),
                div(
                  class = "block-input-wrapper",
                  selectInput(
                    inputId = NS(id, "format"),
                    label = "Format",
                    choices = c(
                      "CSV" = "csv",
                      "Excel" = "excel",
                      "Parquet" = "parquet",
                      "Feather" = "feather"
                    ),
                    selected = format
                  )
                )
              )
            ),

            # Write status
            div(
              class = "block-section",
              div(
                class = "block-help-text",
                textOutput(NS(id, "write_status"))
              )
            ),

            # Advanced Options Toggle
            div(
              class = "block-section",
              div(
                class = "block-advanced-toggle text-muted",
                id = NS(id, "advanced-toggle"),
                onclick = sprintf(
                  "
                  const section = document.getElementById('%s');
                  const chevron = document.querySelector('#%s .block-chevron');
                  section.classList.toggle('expanded');
                  chevron.classList.toggle('rotated');
                  ",
                  NS(id, "advanced-options"),
                  NS(id, "advanced-toggle")
                ),
                tags$span(class = "block-chevron", "\u203A"),
                "Advanced Options"
              )
            ),

            # Advanced Options Content
            div(
              id = NS(id, "advanced-options"),

              # Format-Specific Options Section
              div(
                class = "block-section",
                tags$h4("Format-Specific Options"),

                # CSV Options (conditional)
                div(
                  class = "block-section-grid",
                  conditionalPanel(
                    condition = "output['show_csv_options']",
                    ns = NS(id),
                    div(
                      class = "block-input-wrapper",
                      textInput(
                        inputId = NS(id, "csv_sep"),
                        label = "Delimiter",
                        value = if (!is.null(args$sep)) args$sep else ",",
                        placeholder = "default: ,"
                      )
                    ),
                    div(
                      class = "block-input-wrapper",
                      checkboxInput(
                        inputId = NS(id, "csv_quote"),
                        label = "Quote strings",
                        value = if (!is.null(args$quote)) args$quote else TRUE
                      )
                    ),
                    div(
                      class = "block-input-wrapper",
                      textInput(
                        inputId = NS(id, "csv_na"),
                        label = "NA representation",
                        value = if (!is.null(args$na)) args$na else "",
                        placeholder = "default: empty string"
                      )
                    )
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
    class = c("write_block", "rbind_block"),
    ...
  )
}
