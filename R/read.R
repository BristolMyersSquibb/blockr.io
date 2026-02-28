#' Unified file reading block
#'
#' A single block for reading files in various formats with smart UI that adapts
#' based on detected file type. Supports "From Browser" (upload) and "Location"
#' (path/URL input) modes with persistent storage for uploaded files.
#'
#' @param path Character vector of file paths to pre-load. Accepts local paths
#'   and URLs. When provided, automatically switches to "path" mode regardless
#'   of the source parameter.
#' @param source Either "upload" for file upload widget or "path" for path/URL
#'   input. Default: "upload". Automatically set based on path parameter.
#' @param combine Strategy for combining multiple files: "auto", "rbind", "cbind", "first"
#' @param args Named list of format-specific reading parameters. Only specify values
#'   that differ from defaults. Available parameters:
#'   - **For CSV files:** `sep` (default: ","), `quote` (default: '"'),
#'     `encoding` (default: "UTF-8"), `skip` (default: 0),
#'     `n_max` (default: Inf), `col_names` (default: TRUE)
#'   - **For Excel files:** `sheet` (default: NULL), `range` (default: NULL),
#'     `skip` (default: 0), `n_max` (default: Inf), `col_names` (default: TRUE)
#' @param ... Forwarded to [blockr.core::new_data_block()]
#'
#' @section Configuration:
#' The following settings are retrieved from options and not stored in block state:
#' - **upload_path**: Directory for persistent file storage. Set via
#'   `options(blockr.upload_path = "/path")` or environment variable `BLOCKR_UPLOAD_PATH`.
#'   Default: `tools::R_user_dir("blockr", "data")`
#'
#' @details
#' ## File Handling Modes
#'
#' The block supports two modes:
#'
#' **From Browser mode** (upload):
#' - User uploads files from their computer via the browser
#' - Files are copied to persistent storage directory (upload_path)
#' - State stores permanent file paths
#' - Works across R sessions with state restoration
#'
#' **Location mode** (path):
#' - User enters a file path or URL in a text input with autocomplete
#' - For server paths: reads directly from original location
#' - For URLs: downloads to a temporary file each time
#' - When a board-level data directory is set, paths are resolved relative to it
#'
#' ## Smart Adaptive UI
#'
#' After file selection, the UI detects file type and shows relevant options:
#' - **CSV/TSV:** Delimiter, quote character, encoding options
#' - **Excel:** Sheet selection, cell range
#' - **Other formats:** Minimal or no options (handled automatically)
#'
#' ## Multi-file Support
#'
#' When multiple files are selected:
#' - **"auto"**: Attempts rbind, falls back to first file if incompatible
#' - **"rbind"**: Row-binds files (requires same columns)
#' - **"cbind"**: Column-binds files (requires same row count)
#' - **"first"**: Uses only the first file
#'
#' @return A blockr data block that reads file(s) and returns a data.frame.
#'
#' @examples
#' # Create a read block for a CSV file
#' csv_file <- tempfile(fileext = ".csv")
#' write.csv(mtcars[1:5, ], csv_file, row.names = FALSE)
#' block <- new_read_block(path = csv_file)
#' block
#'
#' # With custom CSV parameters
#' block <- new_read_block(
#'   path = csv_file,
#'   args = list(n_max = 3)
#' )
#'
#' if (interactive()) {
#'   # Launch interactive app
#'   serve(new_read_block())
#' }
#'
#' @importFrom shinyjs useShinyjs
#' @rdname read
#' @export
new_read_block <- function(
  path = character(),
  source = "upload",
  combine = "auto",
  args = list(),
  ...
) {
  # Validate parameters
  source <- match.arg(source, c("upload", "path"))
  combine <- match.arg(combine, c("auto", "rbind", "cbind", "first"))

  # Get upload_path from options (not constructor parameter)
  # Runtime configuration, not persisted state
  upload_path <- blockr_option(
    "upload_path",
    tools::R_user_dir("blockr", "data")
  )

  # Expand and validate upload path
  upload_path <- path.expand(upload_path)

  new_data_block(
    server = function(id) {
      moduleServer(
        id,
        function(input, output, session) {
          # upload_path available here via closure

          # Reactive values for state (only constructor parameters)
          r_source <- reactiveVal(source)
          r_combine <- reactiveVal(combine)

          # File type-specific options stored as a single list
          r_args <- reactiveVal(args)

          # Path storage
          # r_path: State-persisted value (URL or file path string)
          # r_file_paths: Actual file paths for reading (temp file when URL)

          # Handle URL paths at init time
          if (length(path) > 0 && nzchar(path[[1]]) && is_valid_url(path[[1]])) {
            # URL mode: r_path stores the URL string for state persistence
            r_path <- reactiveVal(path[[1]])

            # Download URL and set r_file_paths to temp file for reading
            initial_file_paths <- tryCatch(
              {
                temp_file <- download_url_to_temp(path[[1]])
                url_display <- basename(strsplit(path[[1]], "?", fixed = TRUE)[[
                  1
                ]][1])
                set_names(temp_file, url_display)
              },
              error = function(e) {
                character()
              }
            )
            r_file_paths <- reactiveVal(initial_file_paths)
          } else {
            # Local path mode: r_path and r_file_paths are the same
            if (length(path) > 0) {
              # Validate that provided paths exist (skip relative paths —
              # they'll be resolved against data_dir at runtime)
              is_absolute <- grepl("^(/|~|[A-Za-z]:)", path)
              abs_paths <- path[is_absolute]
              missing_files <- abs_paths[!file.exists(abs_paths)]
              if (length(missing_files) > 0) {
                stop(
                  "File(s) not found: ",
                  paste(missing_files, collapse = ", "),
                  call. = FALSE
                )
              }
              initial_path <- set_names(path, basename(path))
            } else {
              initial_path <- character()
            }
            r_path <- reactiveVal(initial_path)
            r_file_paths <- reactiveVal(initial_path)
          }

          # Detected file type
          initial_type <- if (
            length(path) > 0 && nzchar(path[[1]]) && is_valid_url(path[[1]])
          ) {
            if (exists("initial_file_paths") && length(initial_file_paths) > 0) {
              detect_file_category(initial_file_paths[1])
            } else {
              "unknown"
            }
          } else if (length(path) > 0) {
            detect_file_category(path[1])
          } else {
            "unknown"
          }
          detected_type <- reactiveVal(initial_type)

          # Update state from inputs
          observeEvent(input$combine, r_combine(input$combine))

          # CSV parameter updates - collect into args list
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
          observeEvent(input$csv_encoding, {
            current <- r_args()
            current$encoding <- input$csv_encoding
            r_args(current)
          })
          observeEvent(input$csv_skip, {
            current <- r_args()
            current$skip <- if (input$csv_skip == "") 0 else as.numeric(input$csv_skip)
            r_args(current)
          })
          observeEvent(input$csv_n_max, {
            current <- r_args()
            current$n_max <- if (input$csv_n_max == "") Inf else as.numeric(input$csv_n_max)
            r_args(current)
          })
          observeEvent(input$csv_col_names, {
            current <- r_args()
            current$col_names <- input$csv_col_names
            r_args(current)
          })

          # Excel parameter updates - collect into args list
          observeEvent(input$excel_sheet, {
            current <- r_args()
            current$sheet <- if (input$excel_sheet == "") NULL else input$excel_sheet
            r_args(current)
          })
          observeEvent(input$excel_range, {
            current <- r_args()
            current$range <- if (input$excel_range == "") NULL else input$excel_range
            r_args(current)
          })
          observeEvent(input$excel_skip, {
            current <- r_args()
            current$skip <- if (input$excel_skip == "") 0 else as.numeric(input$excel_skip)
            r_args(current)
          })
          observeEvent(input$excel_n_max, {
            current <- r_args()
            current$n_max <- if (input$excel_n_max == "") Inf else as.numeric(input$excel_n_max)
            r_args(current)
          })
          observeEvent(input$excel_col_names, {
            current <- r_args()
            current$col_names <- input$excel_col_names
            r_args(current)
          })

          # Data directory from board options
          data_dir_reactive <- reactive({
            coal(get_board_option_or_null("data_dir", session), "")
          })

          # Path input module for "Location" tab
          file_path <- path_input_server(
            "file_path",
            data_dir = data_dir_reactive,
            mode = "file"
          )

          # Populate path text input on restore / init
          if (length(path) > 0 && nzchar(path[[1]])) {
            observe({
              display_path <- if (is_valid_url(path[[1]])) {
                path[[1]]
              } else {
                unname(path[1])
              }
              session$sendCustomMessage("blockr-path-set-value", list(
                id = session$ns("file_path-path_text"),
                value = display_path,
                silent = TRUE
              ))
            })
          }

          # Handle path input changes (paths or URLs)
          observeEvent(file_path(), {
            path_val <- file_path()
            req(nzchar(path_val))

            if (is_valid_url(path_val)) {
              # URL: download to temp file
              r_path(path_val)
              tryCatch(
                {
                  temp_file <- download_url_to_temp(path_val)
                  url_display <- basename(
                    strsplit(path_val, "?", fixed = TRUE)[[1]][1]
                  )
                  r_file_paths(set_names(temp_file, url_display))
                  detected_type(detect_file_category(temp_file))
                },
                error = function(e) {
                  r_file_paths(character())
                }
              )
            } else {
              # Local path: resolve relative to data directory
              resolved <- path_val
              data_dir <- data_dir_reactive()
              if (
                nzchar(data_dir) &&
                !grepl("^(/|~|[A-Za-z]:)", path_val)
              ) {
                resolved <- file.path(data_dir, path_val)
              }

              if (file.exists(resolved) && !dir.exists(resolved)) {
                named_path <- set_names(path_val, basename(resolved))
                r_path(named_path)
                r_file_paths(named_path)
                detected_type(detect_file_category(resolved))
              } else if (!dir.exists(resolved)) {
                r_file_paths(character())
                detected_type("unknown")
              }
            }

            r_source("path")
          }, ignoreInit = TRUE)

          # Handle file upload with persistence
          observeEvent(input$file_upload, {
            req(input$file_upload)

            # Create upload directory if it doesn't exist
            upload_dir <- upload_path
            dir.create(upload_dir, recursive = TRUE, showWarnings = FALSE)

            # Clean up old uploads
            cleanup_uploads(upload_dir)

            # Process each uploaded file
            temp_paths <- input$file_upload$datapath
            original_names <- input$file_upload$name

            permanent_paths <- character(length(temp_paths))

            for (i in seq_along(temp_paths)) {
              # Generate unique filename with timestamp
              timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S_%OS3")
              safe_name <- gsub("[^A-Za-z0-9._-]", "_", original_names[i])
              permanent_path <- file.path(
                upload_dir,
                paste0(timestamp, "_", safe_name)
              )

              # Copy file to permanent storage
              file.copy(temp_paths[i], permanent_path, overwrite = FALSE)

              permanent_paths[i] <- permanent_path
            }

            names(permanent_paths) <- original_names

            # For upload mode, r_path and r_file_paths are the same
            r_path(permanent_paths)
            r_file_paths(permanent_paths)

            # Detect file type from first file
            detected_type(detect_file_category(permanent_paths[1]))

            # Update source to "upload" now that we have uploaded files
            r_source("upload")

            # Show uploaded file path in the text input
            display_path <- if (length(permanent_paths) == 1) {
              unname(permanent_paths[1])
            } else {
              paste0(permanent_paths[1], " + ", length(permanent_paths) - 1, " more")
            }
            session$sendCustomMessage("blockr-path-set-value", list(
              id = session$ns("file_path-path_text"),
              value = display_path,
              silent = TRUE
            ))
          })

          # Combination strategy info
          output$combine_info <- renderText({
            current_file_paths <- r_file_paths()
            if (length(current_file_paths) <= 1) {
              return("")
            }

            strategy <- r_combine()
            switch(strategy,
              "auto" = "Will attempt to row-bind files, fallback to first file",
              "rbind" = "Will row-bind files (requires same columns)",
              "cbind" = "Will column-bind files (requires same row count)",
              "first" = "Will use only the first file"
            )
          })

          # Does the current path_text input resolve to an existing file?
          path_resolved <- reactive({
            val <- file_path()
            if (!nzchar(val) || is_valid_url(val)) return(TRUE)
            resolved <- val
            data_dir <- data_dir_reactive()
            if (nzchar(data_dir) && !grepl("^(/|~|[A-Za-z]:)", val)) {
              resolved <- file.path(data_dir, val)
            }
            file.exists(resolved) || dir.exists(resolved)
          })

          # Status badge for file type
          observe({
            type <- detected_type()
            paths <- r_file_paths()
            resolved <- path_resolved()
            type_labels <- c(
              csv = "CSV", excel = "Excel", arrow = "Parquet",
              statistical = "Stats", web = "Web data",
              r_format = "R data", other = "File"
            )
            if (length(paths) > 0 && type != "unknown") {
              label <- unname(type_labels[type]) %||% "File"
              session$sendCustomMessage("blockr-path-status", list(
                id = session$ns("file_path-path_text"),
                text = label,
                state = "success"
              ))
            } else if (!resolved && nzchar(file_path())) {
              session$sendCustomMessage("blockr-path-status", list(
                id = session$ns("file_path-path_text"),
                text = "Not found",
                state = "error"
              ))
            } else {
              session$sendCustomMessage("blockr-path-status", list(
                id = session$ns("file_path-path_text"),
                text = "",
                state = "none"
              ))
            }
          })

          # Show/hide format-specific options based on file type
          output$show_csv_options <- reactive({
            identical(detected_type(), "csv")
          })

          output$show_excel_options <- reactive({
            identical(detected_type(), "excel")
          })

          output$show_multi_file_options <- reactive({
            length(r_file_paths()) > 1
          })

          outputOptions(output, "show_csv_options", suspendWhenHidden = FALSE)
          outputOptions(output, "show_excel_options", suspendWhenHidden = FALSE)
          outputOptions(
            output,
            "show_multi_file_options",
            suspendWhenHidden = FALSE
          )

          list(
            expr = reactive({
              # Resolve data directory for relative paths
              file_paths <- r_file_paths()
              if (length(file_paths) > 0) {
                data_dir <- data_dir_reactive()
                if (nzchar(data_dir)) {
                  file_paths <- vapply(file_paths, function(p) {
                    if (!grepl("^(/|~|[A-Za-z]:)", p) && !is_valid_url(p)) {
                      file.path(data_dir, p)
                    } else {
                      p
                    }
                  }, character(1))
                }
              }

              # Use read_expr() to generate expression, passing args via do.call
              do.call(
                read_expr,
                c(
                  list(
                    paths = file_paths,
                    file_type = detected_type(),
                    combine = r_combine()
                  ),
                  r_args()
                )
              )
            }),
            state = list(
              path = r_path,
              source = r_source,
              combine = r_combine,
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
          class = "block-container read-block-container",
          div(
            class = "block-section blockr-file-location",
            tags$h4("File Location", class = "mb-3"),
            tags$p(
              "Browse server files, paste a URL, or drag & drop to upload",
              class = "blockr-path-hint"
            ),

            tags$style(HTML(
              "
              .blockr-file-input { display: none; }
              .read-block-container .shiny-input-container {
                width: 100% !important;
              }
              .read-block-container .selectize-control {
                width: 100% !important;
              }
            "
            )),

            # Hidden fileInput (Shiny handles upload mechanics)
            div(
              class = "blockr-file-input",
              fileInput(
                inputId = NS(id, "file_upload"),
                label = NULL,
                multiple = TRUE,
                accept = paste0(".", get_rio_extensions())
              )
            ),

            # Unified path input with upload icon
            path_input_ui(
              NS(id, "file_path"),
              upload_id = NS(id, "file_upload")
            )
          ),

          # Wrap Advanced Options in grid
          div(
            class = "block-form-grid mt-3",

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

              tags$p(
                "Change the global data directory in the sidebar",
                class = "blockr-path-hint"
              ),

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
                      textInput(
                        inputId = NS(id, "csv_quote"),
                        label = "Quote character",
                        value = if (!is.null(args$quote)) args$quote else "\"",
                        placeholder = "default: \""
                      )
                    ),
                    div(
                      class = "block-input-wrapper",
                      selectInput(
                        inputId = NS(id, "csv_encoding"),
                        label = "Encoding",
                        choices = c(
                          "UTF-8",
                          "Latin-1",
                          "Windows-1252",
                          "ISO-8859-1"
                        ),
                        selected = if (!is.null(args$encoding)) args$encoding else "UTF-8"
                      )
                    ),
                    div(
                      class = "block-input-wrapper",
                      textInput(
                        inputId = NS(id, "csv_skip"),
                        label = "Skip rows",
                        value = if (!is.null(args$skip)) as.character(args$skip) else "",
                        placeholder = "default: 0"
                      )
                    ),
                    div(
                      class = "block-input-wrapper",
                      textInput(
                        inputId = NS(id, "csv_n_max"),
                        label = "Max rows to read",
                        value = if (!is.null(args$n_max) && !is.infinite(args$n_max)) as.character(args$n_max) else "",
                        placeholder = "default: all rows"
                      )
                    ),
                    div(
                      class = "block-input-wrapper",
                      checkboxInput(
                        inputId = NS(id, "csv_col_names"),
                        label = "First row is header",
                        value = if (!is.null(args$col_names)) args$col_names else TRUE
                      )
                    )
                  )
                ),

                # Excel Options (conditional)
                div(
                  class = "block-section-grid",
                  conditionalPanel(
                    condition = "output['show_excel_options']",
                    ns = NS(id),
                    div(
                      class = "block-input-wrapper",
                      textInput(
                        inputId = NS(id, "excel_sheet"),
                        label = "Sheet name or number",
                        value = if (!is.null(args$sheet)) as.character(args$sheet) else "",
                        placeholder = "default: first sheet"
                      )
                    ),
                    div(
                      class = "block-input-wrapper",
                      textInput(
                        inputId = NS(id, "excel_range"),
                        label = "Cell range",
                        value = if (!is.null(args$range)) args$range else "",
                        placeholder = "default: all cells (e.g., A1:C10)"
                      )
                    ),
                    div(
                      class = "block-input-wrapper",
                      textInput(
                        inputId = NS(id, "excel_skip"),
                        label = "Skip rows",
                        value = if (!is.null(args$skip)) as.character(args$skip) else "",
                        placeholder = "default: 0"
                      )
                    ),
                    div(
                      class = "block-input-wrapper",
                      textInput(
                        inputId = NS(id, "excel_n_max"),
                        label = "Max rows to read",
                        value = if (!is.null(args$n_max) && !is.infinite(args$n_max)) as.character(args$n_max) else "",
                        placeholder = "default: all rows"
                      )
                    ),
                    div(
                      class = "block-input-wrapper",
                      checkboxInput(
                        inputId = NS(id, "excel_col_names"),
                        label = "First row is header",
                        value = if (!is.null(args$col_names)) args$col_names else TRUE
                      )
                    )
                  )
                )
              ),

              # Multi-File Options Section (conditional)
              conditionalPanel(
                condition = "output['show_multi_file_options']",
                ns = NS(id),
                div(
                  class = "block-section",
                  tags$h4("Multi-File Options"),
                  div(
                    class = "block-section-grid",
                    div(
                      class = "block-input-wrapper",
                      selectInput(
                        inputId = NS(id, "combine"),
                        label = "Combination strategy",
                        choices = c(
                          "Auto (rbind with fallback)" = "auto",
                          "Row bind (rbind)" = "rbind",
                          "Column bind (cbind)" = "cbind",
                          "First file only" = "first"
                        ),
                        selected = combine
                      )
                    ),
                    div(
                      class = "block-help-text",
                      textOutput(NS(id, "combine_info"))
                    )
                  )
                )
              )
            )
          ) # Close block-form-grid
        )
      )
    },
    class = "read_block",
    allow_empty_state = TRUE,
    ...
  )
}
