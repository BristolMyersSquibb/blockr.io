#' Unified file reading block
#'
#' A single block for reading files in various formats with smart UI that adapts
#' based on detected file type. Supports "From Browser" (upload) and "From Server"
#' (browse) modes with persistent storage for uploaded files.
#'
#' @param path Character vector of file paths to pre-load. When provided,
#'   automatically switches to "path" mode regardless of the source parameter.
#' @param source Either "upload" for file upload widget, "path" for file browser,
#'   or "url" for URL download. Default: "upload". Automatically set based on path parameter.
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
#' - **volumes**: File browser mount points. Set via `options(blockr.volumes = c(name = "path"))`
#'   or environment variable `BLOCKR_VOLUMES`. Default: `c(home = "~")`
#' - **upload_path**: Directory for persistent file storage. Set via
#'   `options(blockr.upload_path = "/path")` or environment variable `BLOCKR_UPLOAD_PATH`.
#'   Default: `rappdirs::user_data_dir("blockr")`
#'
#' @details
#' ## File Handling Modes
#'
#' The block supports three modes:
#'
#' **From Browser mode** (upload):
#' - User uploads files from their computer via the browser
#' - Files are copied to persistent storage directory (upload_path)
#' - State stores permanent file paths
#' - Works across R sessions with state restoration
#'
#' **From Server mode** (path):
#' - User picks files that already exist on the server
#' - No file copying, reads directly from original location
#' - State stores selected file paths
#' - When running locally, this is your computer's file system
#'
#' **URL mode:**
#' - User provides a URL to a data file
#' - File is downloaded to temporary location each time
#' - Always fetches fresh data from URL
#' - State stores the URL (not file path)
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
#' \dontrun{
#' # Launch interactive app
#' serve(new_read_block())
#' }
#'
#' @importFrom rappdirs user_data_dir
#' @importFrom bslib navset_pill nav_panel
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
  source <- match.arg(source, c("upload", "path", "url"))
  combine <- match.arg(combine, c("auto", "rbind", "cbind", "first"))

  # Get volumes and upload_path from options (not constructor parameters)
  # These are runtime configuration, not persisted state
  # Evaluated once at construction time and captured in closure
  volumes <- blockr_option("volumes", c(home = path.expand("~")))
  upload_path <- blockr_option("upload_path", rappdirs::user_data_dir("blockr"))

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

  # Expand and validate upload path
  upload_path <- path.expand(upload_path)

  new_data_block(
    server = function(id) {
      moduleServer(
        id,
        function(input, output, session) {
          # volumes and upload_path available here via closure

          # Reactive values for state (only constructor parameters)
          r_source <- reactiveVal(source)
          r_combine <- reactiveVal(combine)

          # File type-specific options stored as a single list
          r_args <- reactiveVal(args)

          # Path storage - unified field for both URLs and file paths
          # r_path: State-persisted value (URL string when source="url", file paths otherwise)
          # r_file_paths: Actual file paths for reading (temp file when URL, same as r_path for upload/browse)

          if (source == "url" && length(path) > 0 && nzchar(path[[1]])) {
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
            # Upload/Browse mode: r_path and r_file_paths are the same
            if (length(path) > 0) {
              # Validate that provided paths exist
              missing_files <- path[!file.exists(path)]
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

          # Detected file type - detect on initialization from actual file paths
          # Don't call r_file_paths() here - it's a reactive and we're not in a reactive context yet
          # Use the initial values we just set above instead
          initial_type <- if (
            source == "url" && length(path) > 0 && nzchar(path[[1]])
          ) {
            # For URL mode, detect from temp file if download succeeded
            if (length(initial_file_paths) > 0) {
              detect_file_category(initial_file_paths[1])
            } else {
              "unknown"
            }
          } else if (length(path) > 0) {
            # For upload/browse mode with initial path
            detect_file_category(path[1])
          } else {
            "unknown"
          }
          detected_type <- reactiveVal(initial_type)

          # Update state from inputs
          # Note: source is updated when user actually selects/uploads data, not when accordion changes
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

          # Handle URL input - download to temp file and treat like a path
          observeEvent(input$url_input, {
            url_val <- input$url_input
            req(nzchar(url_val))

            # Validate URL
            if (!is_valid_url(url_val)) {
              return()
            }

            # Store URL in r_path for state persistence
            r_path(url_val)

            # Download to temp file and store in r_file_paths for reading
            tryCatch(
              {
                temp_file <- download_url_to_temp(url_val)

                # Set file path - use URL basename for display
                url_display <- basename(strsplit(url_val, "?", fixed = TRUE)[[
                  1
                ]][1])
                r_file_paths(set_names(temp_file, url_display))

                # Detect file type from actual file path
                detected_type(detect_file_category(temp_file))

                # Update source to "url" now that we have a URL
                r_source("url")
              },
              error = function(e) {
                # Download failed - clear both
                r_path(character())
                r_file_paths(character())
              }
            )
          })

          # Initialize shinyFiles browser
          shinyFiles::shinyFileChoose(
            input,
            "file_browser",
            roots = volumes,
            session = session,
            filetypes = get_rio_extensions()
          )

          # Handle file browser selection
          selected_files <- reactive({
            if (
              !is.null(input$file_browser) && !identical(input$file_browser, "")
            ) {
              shinyFiles::parseFilePaths(volumes, input$file_browser)$datapath
            } else {
              character()
            }
          })

          observeEvent(selected_files(), {
            if (length(selected_files()) > 0) {
              selected_paths <- set_names(
                selected_files(),
                basename(selected_files())
              )
              # For browse mode, r_path and r_file_paths are the same
              r_path(selected_paths)
              r_file_paths(selected_paths)

              # Detect file type from first file
              detected_type(detect_file_category(selected_files()[1]))

              # Update source to "path" now that we have browsed files
              r_source("path")
            }
          })

          # Handle file upload with persistence
          observeEvent(input$file_upload, {
            req(input$file_upload)

            # Create upload directory if it doesn't exist
            upload_dir <- upload_path
            dir.create(upload_dir, recursive = TRUE, showWarnings = FALSE)

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
          })

          # File info for display
          output$file_info <- renderText({
            # Check if URL mode - show the URL string from r_path
            if (identical(r_source(), "url")) {
              url_val <- r_path()
              if (length(url_val) > 0 && any(nzchar(url_val))) {
                return(paste("URL:", url_val))
              } else {
                return("No URL provided")
              }
            }

            # Upload/Browse mode - show file names from r_file_paths
            current_file_paths <- r_file_paths()
            if (length(current_file_paths) == 0) {
              return("No files selected")
            }

            if (length(current_file_paths) == 1) {
              file_name <- names(current_file_paths)[1]
              if (is.null(file_name) || file_name == "") {
                file_name <- basename(current_file_paths[1])
              }
              paste("Selected:", file_name)
            } else {
              paste("Selected", length(current_file_paths), "files")
            }
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

          # File type info
          output$file_type_info <- renderText({
            type <- detected_type()
            if (type == "unknown") {
              return("")
            }

            type_labels <- c(
              csv = "CSV/Text file",
              excel = "Excel spreadsheet",
              statistical = "Statistical software format",
              arrow = "Arrow columnar format",
              web = "Web data format",
              r_format = "R data format",
              other = "Other format"
            )

            paste(
              "Detected:",
              if (is.null(type_labels[type])) {
                "Unknown format"
              } else {
                type_labels[type]
              }
            )
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
              # Use read_expr() to generate expression, passing args via do.call
              do.call(
                read_expr,
                c(
                  list(
                    paths = r_file_paths(),
                    file_type = detected_type(),
                    combine = r_combine()
                  ),
                  r_args()
                )
              )
            }),
            state = list(
              path = r_path, # Reactive itself, not called
              source = r_source,
              combine = r_combine,
              args = r_args
              # Note: volumes and upload_path are runtime configuration, not persisted state
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
            class = "block-section",
            tags$h4("Source", class = "mb-3"),

            # File Source Button Group (full-width, outside grid)
            div(
              class = "mb-3", # Add spacing below buttons
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
                  padding: 6px 10px;;
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

                /* Fix file input styling */
                .blockr-file-input .input-group>.form-control {
                  height: 44px !important;
                }
                .blockr-file-input .form-group {
                  margin-bottom: 0;
                }
                /* Make inputs full width */
                .read-block-container .shiny-input-container {
                  width: 100% !important;
                }
                .read-block-container .selectize-control {
                  width: 100% !important;
                }
              "
              )),
              bslib::navset_pill(
                id = NS(id, "source_pills"),
                selected = source,
                bslib::nav_panel(
                  title = "From Browser",
                  value = "upload",
                  div(
                    class = "block-input-wrapper mt-3",
                    tags$h4("Drag and drop files", class = "mb-2"),
                    div(
                      class = "block-help-text mb-3",
                      "Or click to select. Creates a copy in the app's storage that persists across sessions."
                    ),
                    div(
                      class = "blockr-file-input",
                      fileInput(
                        inputId = NS(id, "file_upload"),
                        label = NULL,
                        multiple = TRUE,
                        accept = paste0(".", get_rio_extensions())
                      )
                    )
                  )
                ),
                bslib::nav_panel(
                  title = "From Server",
                  value = "path",
                  div(
                    class = "block-input-wrapper mt-3",
                    tags$h4("Pick files from the server", class = "mb-2"),
                    div(
                      class = "block-help-text mb-3",
                      "Reads directly from the file path; when running locally, this is your computer."
                    ),
                    shinyFiles::shinyFilesButton(
                      NS(id, "file_browser"),
                      label = "Browse...",
                      title = "Select files to read",
                      multiple = TRUE
                    )
                  )
                ),
                bslib::nav_panel(
                  title = "From URL",
                  value = "url",
                  div(
                    class = "block-input-wrapper mt-3",
                    tags$h4("Fetch data from a web URL", class = "mb-2"),
                    div(
                      class = "block-help-text mb-3",
                      "Downloaded fresh each time the app starts."
                    ),
                    textInput(
                      inputId = NS(id, "url_input"),
                      label = NULL,
                      width = "100%",
                      value = if (source == "url" && length(path) > 0) {
                        path[[1]]
                      } else {
                        ""
                      },
                      placeholder = "https://example.com/data.csv"
                    )
                  )
                )
              )
            )
          ),

          # Wrap File Information and Advanced Options in grid
          div(
            class = "block-form-grid",
            div(
              class = "block-section",
              tags$h4("File Information", class = "mt-3"),

              # File info display (gray info line)
              div(
                class = "block-section-grid",
                div(
                  class = "block-help-text",
                  textOutput(NS(id, "file_info"))
                ),
                div(
                  class = "block-help-text",
                  textOutput(NS(id, "file_type_info"))
                )
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
    allow_empty_state = TRUE, # Allow all state to be empty (format-specific options only used when relevant)
    ...
  )
}
