#' Unified file writing block
#'
#' A variadic block for writing dataframes to files in various formats.
#' Accepts multiple input dataframes and handles single files, multi-sheet
#' Excel, or ZIP archives depending on format and number of inputs.
#'
#' @param directory Character. Default directory for file output. When non-empty,
#'   enables server-side writing. Can be configured via
#'   `options(blockr.write_dir = "/path")` or environment variable
#'   `BLOCKR_WRITE_DIR`. Default: `""` (empty -- download-only until user sets a path).
#' @param filename Character. Optional fixed filename (without extension).
#'   - **If provided**: Writes to the same file path on every save (overwrite)
#'   - **If empty** (default): Manual saves and downloads generate a
#'     timestamped filename (e.g., `data_20250127_143022.csv`); auto-write
#'     uses a fixed `data.{ext}` file so repeated writes overwrite instead
#'     of littering the directory
#' @param format Character. Output format: "csv", "excel", "parquet", or "feather".
#'   Default: "csv"
#' @param auto_write Logical. When TRUE, automatically writes files when data changes
#'   (requires a non-empty directory). When FALSE (default), the user must click
#'   "Save to Server", and each click writes exactly once.
#' @param args Named list of format-specific writing parameters. Only specify values
#'   that differ from defaults. Available parameters:
#'   - **For CSV files:** `sep` (default: ","), `quote` (default: TRUE),
#'     `na` (default: "")
#'   - **For Excel/Arrow:** Minimal options needed (handled by underlying packages)
#' @param mode `r lifecycle::badge("deprecated")` Previously selected between
#'   "browse" and "download" tabs. Now ignored -- both download and server-save
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
#' - Overwrites the file on every save (every data change with auto-write)
#' - Ideal for automated pipelines
#'
#' **Empty filename** (`filename = ""`):
#' - Manual saves and downloads: unique files
#'   `{directory}/data_YYYYMMDD_HHMMSS.{ext}` -- preserves history,
#'   prevents accidental overwrites
#' - Auto-write: fixed `{directory}/data.{ext}`, overwritten on each
#'   change -- a timestamp would create one file per upstream change
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
#' - User types a directory path (committed with Enter, blur, or a
#'   dropdown selection -- an "Enter" chip shows while the typed path is
#'   not yet applied) in the path input
#' - The target directory is created at write time if missing
#' - In manual mode each "Save to Server" click writes exactly once;
#'   later data changes never rewrite the file
#' - Files persist on server; when running locally, this is your
#'   computer's file system
#'
#' ## Pipeline Behavior
#'
#' The block passes its first input through unchanged. Only with
#' `auto_write = TRUE` does the block expression itself contain the write
#' (so exported code reproduces the auto-write); manual saves and downloads
#' happen in their handlers and keep the expression a pure passthrough.
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

  # Strip trailing slashes for consistency, but preserve the path as-is
  # (relative paths are resolved against data_dir at runtime)
  if (nzchar(directory)) {
    directory <- sub("/+$", "", directory)
  }

  new_transform_block(
    server = function(id, ...args) {
      moduleServer(
        id,
        function(input, output, session) {
          # directory, auto_write available here via closure

          # Eval-env reference names for the connected inputs: the link name
          # for named slots, ".arg1", ".arg2", ... for unnamed ones (added via
          # the DAG UI). Values are the symbols the block expression and the
          # imperative save/download handlers bind data under; names are the
          # display names. Reactive on the link set.
          arg_names <- reactive({
            dot_arg_refs(...args)
          })

          # Reactive values for state
          r_directory <- reactiveVal(directory)
          r_filename <- reactiveVal(filename)
          r_format <- reactiveVal(format)
          r_auto_write <- reactiveVal(auto_write)
          r_args <- reactiveVal(args)
          r_write_status <- reactiveVal("") # Status message
          r_dir_ok <- reactiveVal(TRUE) # Deployment file-access policy gate

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

          # Handle directory path changes -- store relative path in state
          observeEvent(dir_path(), {
            path_val <- dir_path()
            req(nzchar(path_val))
            r_directory(path_val)
          }, ignoreInit = TRUE)

          # Resolve r_directory() against data_dir for I/O operations
          resolved_directory <- reactive({
            dir_val <- r_directory()
            if (!nzchar(dir_val)) return("")
            data_dir <- data_dir_reactive()
            if (nzchar(data_dir) && !grepl("^(/|~|[A-Za-z]:)", dir_val)) {
              file.path(data_dir, dir_val)
            } else {
              dir_val
            }
          })

          # Deployment file-access policy: reject write targets outside the
          # allowed roots before anything is written. Gates the submit
          # handler and the auto-write expression below.
          observeEvent(resolved_directory(), {
            dir_val <- resolved_directory()
            if (!nzchar(dir_val)) {
              r_dir_ok(TRUE)
              return()
            }
            tryCatch(
              {
                resolve_and_check(dir_val, "write")
                r_dir_ok(TRUE)
              },
              error = function(e) {
                r_dir_ok(FALSE)
                r_write_status(sprintf("\u2717 %s", conditionMessage(e)))
              }
            )
          }, ignoreNULL = FALSE)

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

          # Auto-write filename: empty means a FIXED "data" file, overwritten
          # on each change \u2014 a timestamp here would litter the directory with
          # one file per upstream invalidation.
          auto_filename <- function() {
            if (nzchar(r_filename())) r_filename() else "data"
          }

          # Full output path for a given base filename (single source for the
          # write expression and the status message \u2014 one computation, no
          # timestamp drift between the reported and the written file).
          output_path <- function(base_filename) {
            needs_zip <- length(arg_names()) > 1 && r_format() != "excel"
            ext <- format_extension(r_format(), needs_zip = needs_zip)
            file.path(resolved_directory(), paste0(base_filename, ext))
          }

          # Submit button for server save (only when auto_write is FALSE).
          # The write happens HERE, imperatively \u2014 exactly once per click.
          # Keeping it out of the block expression means later upstream
          # changes can never silently rewrite the file in manual mode.
          observeEvent(input$submit_write, {
            req(length(arg_names()) > 0)
            req(nzchar(r_directory()))
            req(!r_auto_write()) # Only trigger when auto_write is disabled
            req(r_dir_ok()) # Deployment file-access policy

            # Compute the filename once; write_expr() and the status message
            # both use it, so the reported path is the written path.
            base_filename <- generate_filename(r_filename())

            expr <- write_expr(
              data_names = arg_names(),
              directory = resolved_directory(),
              filename = base_filename,
              format = r_format(),
              args = r_args()
            )

            # Bind each input under the same reference symbol the write
            # expression uses (.arg1 for an unnamed/DAG-UI slot, else the link
            # name). dot_arg_values() reads slots positionally, so it is robust
            # to the unnamed positional keys a live board assigns -- a per-name
            # ...args[[nm]] lookup would miss those and bind NULL.
            eval_env <- new.env(parent = baseenv())
            arg_vals <- dot_arg_values(...args)
            for (nm in names(arg_vals)) {
              val <- arg_vals[[nm]]
              assign(nm, if (is.reactive(val)) val() else val, envir = eval_env)
            }

            tryCatch(
              {
                eval(expr, envir = eval_env)
                r_write_status(sprintf(
                  "\u2713 Saved to %s at %s",
                  output_path(base_filename), format(Sys.time(), "%H:%M:%S")
                ))
              },
              error = function(e) {
                r_write_status(sprintf(
                  "\u2717 Write failed: %s", conditionMessage(e)
                ))
              }
            )
          })

          # Block expression: the write block is a passthrough node. Only in
          # auto-write mode does the expression carry the write itself (its
          # contract is "rewrite on every change", and the exported code
          # should reproduce that). Manual saves and downloads happen
          # imperatively in their handlers, so the expression stays pure.
          r_write_expression <- reactive({
            req(length(arg_names()) > 0)
            # `as_dot_sym()` (not `as.name()`): this expression is exported and
            # re-bquoted by blockr.core, see `expr_type = "bquoted"` below. The
            # imperative save/download paths keep bare symbols.
            first_data <- as_dot_sym(arg_names()[1])

            if (nzchar(r_directory()) && r_auto_write() && r_dir_ok()) {
              expr <- write_expr(
                data_names = arg_names(),
                directory = resolved_directory(),
                filename = auto_filename(),
                format = r_format(),
                args = r_args(),
                as_sym = as_dot_sym
              )

              bquote({
                .(expr)
                .(first_data)
              })
            } else {
              # Wrapped in { } because blockr.core requires a language
              # object (a bare symbol is not one)
              bquote({
                .(first_data)
              })
            }
          })

          # Update status when auto-write generates a new expression
          observe({
            req(nzchar(r_directory()))
            req(r_auto_write())
            req(r_dir_ok())
            req(length(arg_names()) > 0)

            # Depend on all data values to trigger status update when data
            # changes. dot_arg_values() realizes every slot (including unnamed
            # positional ones), establishing the reactive dependency.
            dot_arg_values(...args)

            # Deterministic path (fixed filename in auto mode) \u2014 matches the
            # path baked into the auto-write expression.
            full_path <- output_path(generate_filename(auto_filename()))
            timestamp <- format(Sys.time(), "%H:%M:%S")
            r_write_status(sprintf("\u2713 Saved to %s at %s", full_path, timestamp))
          })


          # Download handler -- always available
          output$download_data <- downloadHandler(
            filename = function() {
              base <- generate_filename(r_filename())
              needs_zip <- length(arg_names()) > 1 && r_format() != "excel"
              paste0(base, format_extension(r_format(), needs_zip = needs_zip))
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

              # Bind each input under its reference symbol (.arg1 for unnamed
              # DAG-UI slots, else the link name) -- the same names write_expr()
              # emits. dot_arg_values() handles both the live-board reactives
              # and the reactiveValues used in tests.
              arg_vals <- dot_arg_values(...args)
              for (nm in names(arg_vals)) {
                data_val <- arg_vals[[nm]]
                assign(
                  nm,
                  if (is.reactive(data_val)) data_val() else data_val,
                  envir = eval_env
                )
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


          # Status badge for directory validation. Runs on committed path
          # changes only (Enter/blur/selection), so the dir.exists() check
          # is cheap. "New directory" signals it will be created on save.
          observe({
            dir <- r_directory()
            if (nzchar(dir) && dir.exists(resolved_directory())) {
              session$sendCustomMessage("blockr-path-status", list(
                id = session$ns("dir_path-path_text"),
                text = "Directory",
                state = "success"
              ))
            } else if (nzchar(dir)) {
              session$sendCustomMessage("blockr-path-status", list(
                id = session$ns("dir_path-path_text"),
                text = "New directory (created on save)",
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
      gear_id <- NS(id, "gear_btn")
      band_id <- NS(id, "gear_band")
      tagList(
        io_block_deps(),
        div(
          class = "block-container io-block write-block-container",

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
            style = "padding-bottom: 0; margin-bottom: 0;",
            div(
              class = "block-section",
              div(
                class = "blockr-gear-row",
                tags$button(
                  type = "button",
                  id = gear_id,
                  class = "blockr-gear-btn",
                  title = "Advanced settings",
                  `aria-label` = "Advanced settings",
                  `aria-controls` = band_id,
                  `aria-expanded` = "false",
                  onclick = sprintf(
                    "window.blockrIoGearToggle && window.blockrIoGearToggle('%s','%s');",
                    gear_id, band_id
                  ),
                  HTML(gear_icon_svg())
                )
              ),

              # Settings band: in-flow panel spanning the form grid (see the
              # .block-form-grid .blockr-settings rule in io-blocks.css).
              # Visibility is class-driven; blockrIoGearToggle() flips it.
              div(
                id = band_id,
                class = "blockr-settings blockr-settings--beak",
                role = "region",
                `aria-label` = "Write settings",

                div(class = "blockr-settings__title", "Format options"),

                conditionalPanel(
                  condition = "output['show_csv_options']",
                  ns = NS(id),
                  class = "blockr-settings__grid",
                  div(
                    class = "blockr-settings__field",
                    tags$label(
                      class = "blockr-label",
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
                    class = "blockr-settings__field",
                    checkboxInput(
                      inputId = NS(id, "csv_quote"),
                      label = "Quote strings",
                      value = if (!is.null(args$quote)) args$quote else TRUE
                    )
                  ),
                  div(
                    class = "blockr-settings__field",
                    tags$label(
                      class = "blockr-label",
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
              ),
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
                  # The placeholder covers "empty means timestamped". These two
                  # consequences it cannot: a fixed name loses every prior
                  # save, and an empty name under auto-save yields "data"
                  # rather than the timestamp the placeholder promises.
                  div(
                    class = "block-help-text",
                    style = "font-size: 0.75rem;",
                    "A fixed name is overwritten on every save.",
                    "Left empty, auto-save writes \"data\" instead of a",
                    "timestamped file."
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
          tags$hr(style = paste(
            "border-top: 1px solid var(--blockr-color-border, #e5e7eb);",
            "margin: 16px 0;"
          )),

          # --- Download to Browser ---
          div(
            class = "block-section",
            # No hint: the sibling section is labelled "Save to Server", so the
            # two labels already draw the contrast a line here would spell out.
            div(class = "io-section-label", "Download to Browser"),
            downloadButton(
              NS(id, "download_data"),
              "Download",
              class = "btn-outline-secondary btn-sm"
            )
          ),

          # --- OR divider ---
          div(
            class = "io-or-divider",
            tags$span("or")
          ),

          # --- Save to Server ---
          div(
            class = "block-section io-file-location",
            div(class = "io-section-label", "Save to Server"),
            # The placeholder says to type a path and the Enter chip says how
            # to commit it. Only the suggestion list is invisible until you
            # start typing.
            tags$p(
              class = "blockr-path-hint",
              "Typing suggests matching directories."
            ),
            path_input_ui(
              NS(id, "dir_path"),
              placeholder = "Enter directory path..."
            ),
            # Mode toggle + save button row
            div(
              class = "mt-2",
              style = "display: flex; align-items: center; gap: 8px;",
              div(
                class = "io-exec-toggle",
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
              # "Auto-save enabled" restates the toggle the user just pressed.
              # The overwrite-per-change consequence is the reason to show it.
              div(
                class = "io-exec-auto-hint mt-2",
                "Writes to a fixed file, overwritten on every data change."
              )
            ),
            # Status message
            div(
              class = "io-exec-status mt-2",
              textOutput(NS(id, "write_status"))
            )
          ),

        )
      )
    },
    dat_valid = function(...args) {
      stopifnot(length(...args) >= 1L)
    },
    allow_empty_state = TRUE,
    class = c("write_block", "rbind_block"),
    expr_type = "bquoted",
    ...
  )
}
