#' Unified file writing block
#'
#' A variadic block for writing dataframes to files in various formats.
#' Accepts multiple input dataframes and handles single files, multi-sheet
#' Excel, or ZIP archives depending on format and number of inputs.
#'
#' @param directory Character. Default directory for file output. When non-empty,
#'   enables server-side writing. Can be configured via
#'   `options(blockr.write_dir = "/path")` or environment variable
#'   `BLOCKR_WRITE_DIR`. Default: `""` (empty — download-only until user sets a path).
#' @param filename Character. Optional fixed filename (without extension).
#'   - **If provided**: Writes to same file path on every upstream change (auto-overwrite)
#'   - **If empty** (default): Generates timestamped filename (e.g., `data_20250127_143022.csv`)
#' @param format Character. Output format: "csv", "excel", "parquet", or "feather".
#'   Default: "csv"
#' @param auto_write Logical. When TRUE, automatically writes files when data changes
#'   (requires a non-empty directory). When FALSE (default), user must click
#'   "Save to File" button.
#' @param args Named list of format-specific writing parameters. Only specify values
#'   that differ from defaults. Available parameters:
#'   - **For CSV files:** `sep` (default: ","), `quote` (default: TRUE),
#'     `na` (default: "")
#'   - **For Excel/Arrow:** Minimal options needed (handled by underlying packages)
#' @param mode `r lifecycle::badge("deprecated")` Previously selected between
#'   "browse" and "download" tabs. Now ignored — both download and server-save
#'   are always available. Kept for backwards compatibility; emits a deprecation
#'   warning when non-NULL.
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
#' ## Download vs Server Save
#'
#' Both options are always available in a flat layout (no tabs):
#'
#' **Download to Browser:**
#' - Always available via the download button
#' - Triggers a download to your browser's download folder
#'
#' **Save to Server:**
#' - Active when a server directory path is set (non-empty)
#' - User enters a directory path in the path input
#' - Files persist on server
#' - When running locally, this is your computer's file system
#'
#' @return A blockr transform block that writes dataframes to files
#'
#' @examples
#' # Create a write block for CSV output
#' block <- new_write_block(
#'   directory = tempdir(),
#'   filename = "output",
#'   format = "csv"
#' )
#' block
#'
#' # Write block for Excel with auto-timestamp
#' block <- new_write_block(
#'   directory = tempdir(),
#'   filename = "",
#'   format = "excel"
#' )
#'
#' if (interactive()) {
#'   # Launch interactive app
#'   serve(new_write_block())
#' }
#'
#' @rdname write
#' @export
new_write_block <- function(
  directory = "",
  filename = "",
  format = "csv",
  auto_write = FALSE,
  args = list(),
  mode = NULL,
  ...
) {
  if (!is.null(mode)) {
    .Deprecated(
      msg = paste(
        "The 'mode' parameter of new_write_block() is deprecated.",
        "Both download and server-save are now always available.",
        "Use 'directory' to control server-save behavior."
      )
    )
  }

  # Validate parameters
  format <- match.arg(format, unname(write_formats()))

  # Expand directory path if non-empty
  if (nzchar(directory)) {
    directory <- path.expand(directory)
  }

  new_transform_block(
    server = function(id, ...args) {
      moduleServer(
        id,
        function(input, output, session) {
          # directory, auto_write available here via closure

          # Extract arg names for variadic inputs
          arg_names <- reactive({
            names_vec <- names(...args)
            set_names(names_vec, dot_args_names(...args))
          })

          # Reactive values for state
          r_directory <- reactiveVal(directory)
          r_filename <- reactiveVal(filename)
          r_format <- reactiveVal(format)
          r_auto_write <- reactiveVal(auto_write)
          r_args <- reactiveVal(args)
          r_last_write <- reactiveVal(NULL) # Track last write time
          r_write_status <- reactiveVal("") # Status message

          # Data directory from board options
          data_dir_reactive <- reactive({
            coal(get_board_option_or_null("data_dir", session), "")
          })

          # Path input module for directory selection
          dir_path <- path_input_server(
            "dir_path",
            data_dir = data_dir_reactive,
            mode = "directory"
          )

          # Populate path text input on restore / init
          if (nzchar(directory)) {
            observe({
              # Strip data_dir prefix for display if applicable
              display_path <- directory
              dd <- data_dir_reactive()
              if (nzchar(dd)) {
                prefix <- paste0(dd, "/")
                if (startsWith(directory, prefix)) {
                  display_path <- substr(directory, nchar(prefix) + 1, nchar(directory))
                }
              }
              session$sendCustomMessage("blockr-path-set-value", list(
                id = session$ns("dir_path-path_text"),
                value = display_path,
                silent = TRUE
              ))
            }) |> bindEvent(TRUE, once = TRUE)
          }

          # Handle directory path changes
          observeEvent(dir_path(), {
            path_val <- dir_path()
            req(nzchar(path_val))

            # Resolve relative paths against data directory
            resolved <- path_val
            data_dir <- data_dir_reactive()
            if (
              nzchar(data_dir) &&
              !grepl("^(/|~|[A-Za-z]:)", path_val)
            ) {
              resolved <- file.path(data_dir, path_val)
            }

            r_directory(resolved)
          }, ignoreInit = TRUE)

          # Update state from inputs
          observeEvent(input$write_mode, {
            r_auto_write(identical(input$write_mode, "auto"))
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

          # Reactive to store the write expression (set when submit clicked)
          r_write_expression_set <- reactiveVal(NULL)

          # Track whether directory existed before we created it
          r_dir_existed <- reactiveVal(
            nzchar(directory) && dir.exists(path.expand(directory))
          )

          # Directory creation - create when directory path is set
          observeEvent(r_directory(), {
            req(r_directory())
            existed <- dir.exists(r_directory())
            r_dir_existed(existed)
            if (!existed) {
              tryCatch(
                {
                  dir.create(r_directory(), recursive = TRUE, showWarnings = FALSE)
                  if (!dir.exists(r_directory())) {
                    r_write_status(sprintf(
                      "\u2717 Cannot create directory: %s", r_directory()
                    ))
                  }
                },
                error = function(e) {
                  r_write_status(sprintf(
                    "\u2717 Directory error: %s", conditionMessage(e)
                  ))
                }
              )
            }
          })

          # Submit button for server save (only when auto_write is FALSE)
          observeEvent(input$submit_write, {
            req(length(arg_names()) > 0)
            req(nzchar(r_directory()))
            req(!r_auto_write()) # Only trigger when auto_write is disabled

            # Generate write expression
            expr <- write_expr(
              data_names = arg_names(),
              directory = r_directory(),
              filename = r_filename(),
              format = r_format(),
              args = r_args()
            )

            # Set the expression - blockr.core will evaluate it
            # Expression writes file and returns first data for pipeline continuity
            first_data <- as.name(arg_names()[1])
            r_write_expression_set(bquote({
              .(expr)
              .(first_data)
            }))

            # Generate confirmation message with file path and timestamp
            base_filename <- generate_filename(r_filename())
            ext <- switch(r_format(),
              "csv" = ".csv",
              "excel" = ".xlsx",
              "parquet" = ".parquet",
              ".csv"
            )
            full_path <- file.path(r_directory(), paste0(base_filename, ext))
            timestamp <- format(Sys.time(), "%H:%M:%S")
            r_write_status(sprintf("\u2713 Saved to %s at %s", full_path, timestamp))
          })

          # Generate expression based on directory state and auto_write setting
          r_write_expression <- reactive({
            if (nzchar(r_directory())) {
              if (r_auto_write()) {
                # Auto-write enabled: Generate expression automatically
                req(length(arg_names()) > 0)

                expr <- write_expr(
                  data_names = arg_names(),
                  directory = r_directory(),
                  filename = r_filename(),
                  format = r_format(),
                  args = r_args()
                )

                # Return expression with write code and data passthrough
                first_data <- as.name(arg_names()[1])
                bquote({
                  .(expr)
                  .(first_data)
                })
              } else {
                # Auto-write disabled: Wait for submit button
                r_write_expression_set()
              }
            } else {
              # No directory set: Return NULL - download handler handles writing
              NULL
            }
          })

          # Update status when auto-write generates a new expression
          observe({
            req(nzchar(r_directory()))
            req(r_auto_write())
            req(r_write_expression())

            # Depend on all data values to trigger status update when data changes
            for (nm in names(...args)) {
              ...args[[nm]]
            }

            # Generate confirmation message with file path and timestamp
            base_filename <- generate_filename(r_filename())
            ext <- switch(r_format(),
              "csv" = ".csv",
              "excel" = ".xlsx",
              "parquet" = ".parquet",
              ".csv"
            )
            full_path <- file.path(r_directory(), paste0(base_filename, ext))
            timestamp <- format(Sys.time(), "%H:%M:%S")
            r_write_status(sprintf("\u2713 Saved to %s at %s", full_path, timestamp))
          })


          # Download handler — always available
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
              # Use a fixed timestamp for consistent filename generation
              # This prevents mismatch between write_expr and file search
              fixed_timestamp <- Sys.time()
              base_filename <- generate_filename(r_filename(), fixed_timestamp)

              # Generate write expression for temp directory
              temp_dir <- dirname(file)
              expr <- write_expr(
                data_names = arg_names(),
                directory = temp_dir,
                filename = base_filename, # Use pre-computed filename
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
              # Use same base_filename as we used for write_expr
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
                # Only copy if source and dest are different (can be same in tests)
                if (normalizePath(generated_file) != normalizePath(file, mustWork = FALSE)) {
                  file.copy(generated_file, file, overwrite = TRUE)
                }
              }
            }
          )


          # Status badge for directory validation
          observe({
            dir <- r_directory()
            existed <- r_dir_existed()
            if (nzchar(dir) && existed) {
              session$sendCustomMessage("blockr-path-status", list(
                id = session$ns("dir_path-path_text"),
                text = "Directory",
                state = "success"
              ))
            } else if (nzchar(dir)) {
              session$sendCustomMessage("blockr-path-status", list(
                id = session$ns("dir_path-path_text"),
                text = "New directory",
                state = "info"
              ))
            } else {
              session$sendCustomMessage("blockr-path-status", list(
                id = session$ns("dir_path-path_text"),
                text = "",
                state = "none"
              ))
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
              auto_write = r_auto_write,
              args = r_args,
              mode = reactiveVal(NULL)
            )
          )
        }
      )
    },
    ui = function(id) {
      tagList(
        # Add CSS
        css_responsive_grid(),
        css_advanced_toggle(NS(id, "advanced-options"), use_subgrid = TRUE),
        div(
          class = "block-container write-block-container",

          tags$style(HTML("
            /* Make inputs full width */
            .write-block-container .shiny-input-container {
              width: 100% !important;
            }
            .write-block-container .selectize-control {
              width: 100% !important;
            }
            /* Execution mode toggle */
            .blockr-exec-toggle {
              display: inline-flex;
              align-items: center;
              gap: 2px;
              background-color: #f3f4f6;
              border-radius: 8px;
              padding: 2px;
            }
            .blockr-exec-toggle button {
              padding: 0.25rem 0.5rem;
              font-size: 0.875rem;
              line-height: 1.5;
              font-weight: 500;
              color: #6b7280;
              background: transparent;
              border: none;
              border-radius: 6px;
              cursor: pointer;
              transition: all 0.15s ease;
              white-space: nowrap;
            }
            .blockr-exec-toggle button:hover {
              color: #374151;
              background-color: #e5e7eb;
            }
            .blockr-exec-toggle button.active {
              color: #111827;
              background-color: #fff;
              box-shadow: 0 1px 2px rgb(0 0 0 / 0.06);
            }
            /* Auto-save info banner */
            .blockr-exec-auto-hint {
              font-size: 0.8rem;
              color: #0d6efd;
              background: #e7f1ff;
              border: 1px solid #b6d4fe;
              border-radius: 6px;
              padding: 8px 12px;
            }
            /* Status text */
            .blockr-exec-status {
              font-size: 0.8rem;
              color: #6b7280;
              min-height: 1.2em;
            }
            /* OR divider */
            .blockr-or-divider {
              display: flex;
              align-items: center;
              gap: 12px;
              margin: 16px 0;
            }
            .blockr-or-divider::before,
            .blockr-or-divider::after {
              content: '';
              flex: 1;
              border-top: 1px solid #e5e7eb;
            }
            .blockr-or-divider span {
              font-size: 0.75rem;
              font-weight: 500;
              color: #9ca3af;
              text-transform: uppercase;
              letter-spacing: 0.05em;
            }
          ")),

          # Hidden input to track mode
          div(
            style = "display:none;",
            textInput(
              NS(id, "write_mode"),
              label = NULL,
              value = if (auto_write) "auto" else "manual"
            )
          ),

          # --- File Configuration (shared) ---
          div(
            class = "block-form-grid",
            style = "padding-bottom: 0;",
            div(
              class = "block-section",
              tags$h4("File Configuration", class = "mb-3"),
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
                    style = "font-size: 0.75rem;",
                    "Fixed filename overwrites on each change.",
                    "Empty generates unique timestamped files."
                  )
                ),
                div(
                  class = "block-input-wrapper",
                  selectInput(
                    inputId = NS(id, "format"),
                    label = "Format",
                    choices = write_formats(),
                    selected = format
                  )
                )
              )
            )
          ),

          # --- Separator ---
          tags$hr(style = "border-top: 1px solid #e5e7eb; margin: 16px 0;"),

          # --- Download to Browser ---
          div(
            class = "block-section",
            tags$h4("Download to Browser", class = "mb-3"),
            tags$p(
              class = "blockr-path-hint",
              "Download directly without saving to server"
            ),
            downloadButton(
              NS(id, "download_data"),
              "Download",
              class = "btn-outline-secondary btn-sm"
            )
          ),

          # --- OR divider ---
          div(
            class = "blockr-or-divider",
            tags$span("or")
          ),

          # --- Save to Server ---
          div(
            class = "block-section blockr-file-location",
            tags$h4("Save to Server", class = "mb-3"),
            tags$p(
              class = "blockr-path-hint",
              "Choose a server path to save"
            ),
            path_input_ui(NS(id, "dir_path")),
            # Mode toggle + save button row
            div(
              class = "mt-2",
              style = "display: flex; align-items: center; gap: 8px;",
              div(
                class = "blockr-exec-toggle",
                tags$button(
                  "Manual",
                  class = if (!auto_write) "active" else "",
                  onclick = sprintf(
                    "
                    document.getElementById('%s').value = 'manual';
                    document.getElementById('%s').dispatchEvent(new Event('change'));
                    this.classList.add('active');
                    this.nextElementSibling.classList.remove('active');
                    ",
                    NS(id, "write_mode"), NS(id, "write_mode")
                  )
                ),
                tags$button(
                  "Auto",
                  class = if (auto_write) "active" else "",
                  onclick = sprintf(
                    "
                    document.getElementById('%s').value = 'auto';
                    document.getElementById('%s').dispatchEvent(new Event('change'));
                    this.classList.add('active');
                    this.previousElementSibling.classList.remove('active');
                    ",
                    NS(id, "write_mode"), NS(id, "write_mode")
                  )
                )
              ),
              conditionalPanel(
                condition = "input.write_mode === 'manual'",
                ns = NS(id),
                actionButton(
                  NS(id, "submit_write"),
                  "Save to Server",
                  class = "btn-primary btn-sm"
                )
              )
            ),
            # Auto-save info box
            conditionalPanel(
              condition = "input.write_mode === 'auto'",
              ns = NS(id),
              div(
                class = "blockr-exec-auto-hint mt-2",
                "Auto-save enabled. File updates automatically on data changes."
              )
            ),
            # Status message
            div(
              class = "blockr-exec-status mt-2",
              textOutput(NS(id, "write_status"))
            )
          ),

          # --- Advanced Options ---
          div(
            class = "block-section mt-3",
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
          div(
            id = NS(id, "advanced-options"),
            div(
              class = "block-section",
              tags$h4("Format-Specific Options"),
              div(
                class = "block-section-grid",
                conditionalPanel(
                  condition = "output['show_csv_options']",
                  ns = NS(id),
                  div(
                    class = "block-input-wrapper",
                    selectizeInput(
                      inputId = NS(id, "csv_sep"),
                      label = "Delimiter",
                      choices = c(
                        "Comma (,)" = ",",
                        "Semicolon (;)" = ";",
                        "Tab (\\t)" = "\t",
                        "Pipe (|)" = "|"
                      ),
                      selected = if (!is.null(args$sep)) args$sep else ",",
                      options = list(create = TRUE)
                    ),
                    div(
                      class = "block-help-text",
                      style = "font-size: 0.75rem;",
                      "Type to add custom delimiter"
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
    },
    dat_valid = function(...args) {
      stopifnot(length(...args) >= 1L)
    },
    allow_empty_state = TRUE,
    class = c("write_block", "rbind_block"),
    ...
  )
}
