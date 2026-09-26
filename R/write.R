#' Unified file writing block
#'
#' A variadic block for writing data frames to files in various formats.
#' Accepts multiple input data frames and handles single files, multi-sheet
#' Excel, or ZIP archives depending on format and number of inputs.
#'
#' @param directory Character. Folder on the server to save to. Non-empty
#'   turns server saving on; empty (the default) means the block only
#'   downloads. Relative paths are resolved against the board's data
#'   directory.
#' @param filename Character. Optional fixed filename (without extension).
#'   - **If provided**: Writes to the same file path on every save (overwrite)
#'   - **If empty** (default): Manual saves and downloads generate a
#'     timestamped filename (e.g., `data_20250127_143022.csv`); auto-write
#'     uses a fixed `data.{ext}` file so repeated writes overwrite instead
#'     of littering the directory
#' @param format Character. Output format: "csv", "excel", "parquet", or "feather".
#'   Default: "csv"
#' @param auto_write Logical. When TRUE, saves to the server whenever the
#'   data changes ("On change"; requires a non-empty directory). When FALSE
#'   (default), the user clicks "Save to server" ("On click"), and each
#'   click writes exactly once.
#' @param args Named list of format-specific writing parameters. Only specify values
#'   that differ from defaults. Available parameters:
#'   - **For CSV files:** `sep` (default: ","), `quote` (default: TRUE),
#'     `na` (default: "")
#'   - **For Excel/Arrow:** Minimal options needed (handled by underlying packages)
#' @param mode `r lifecycle::badge("deprecated")` Previously selected between
#'   "browse" and "download" tabs. Now ignored. Kept for backwards
#'   compatibility; emits a deprecation warning when non-NULL.
#' @param ... Forwarded to [blockr.core::new_transform_block()]
#'
#' @details
#' ## The block
#'
#' At rest the block shows one button, "Download CSV" (named after the
#' chosen format). Everything else is in the gear: the format, the
#' filename and the format's options, and a section switched on by "Save to
#' the server" with the folder and when to save ("On click" or "On
#' change"). With server saving on and "On click", the block shows "Save
#' to server" beside the download, and a status line under the buttons
#' says where the file goes and when it was last written.
#'
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
#'
#' **Empty filename** (`filename = ""`):
#' - Manual saves and downloads: unique files
#'   `{directory}/data_YYYYMMDD_HHMMSS.{ext}`
#' - Auto-write: fixed `{directory}/data.{ext}`, overwritten on each
#'   change; a timestamp would create one file per upstream change
#'
#' ## Server saving
#'
#' - The folder field commits on Enter, blur or a pick from its
#'   suggestions
#' - The target folder is created at write time if missing
#' - "On click": each click writes exactly once; later data changes never
#'   rewrite the file
#' - Files persist on the server; when running locally, this is your
#'   computer's file system
#' - The deployment's file-access policy ([file_policy]) is checked before
#'   anything is written
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
  write_block_impl(
    directory = directory,
    filename = filename,
    format = format,
    auto_write = auto_write,
    args = args,
    mode = mode,
    class = "write_block",
    default_ctor = "new_write_block",
    ...
  )
}

# The write block, for new_write_block() and new_download_block(). The
# two differ in their class and constructor only: a saved board names both,
# and restores a block only when they come back the same. The arguments up
# to `mode` are the block's state and keep their names, because the state a
# board saves is read off this frame.
write_block_impl <- function(
  directory = "",
  filename = "",
  format = "csv",
  auto_write = FALSE,
  args = list(),
  mode = NULL,
  class,
  default_ctor,
  ...
) {
  # A restored board carries `mode` as an empty value.
  if (length(mode)) {
    .Deprecated(
      msg = paste(
        "The 'mode' parameter of new_write_block() is deprecated.",
        "Both download and server-save are now always available.",
        "Use 'directory' to control server-save behavior."
      )
    )
  }
  mode <- NULL

  # Validate parameters
  format <- match.arg(format, unname(write_formats()))

  # Strip trailing slashes for consistency, but preserve the path as-is
  # (relative paths are resolved against data_dir at runtime)
  if (nzchar(directory)) {
    directory <- sub("/+$", "", directory)
  }

  # The constructor the board records: the public one that was called,
  # unless the caller (a restore, the block registry) names one.
  dots <- list(...)
  if (is.null(dots[["ctor"]])) {
    dots[["ctor"]] <- default_ctor
    dots[["ctor_pkg"]] <- "blockr.io"
  }

  do.call(new_transform_block, c(list(
    server = function(id, ...args) {
      moduleServer(
        id,
        function(input, output, session) {
          # Eval-env reference names for the connected inputs: the link name
          # for named slots, ".arg1", ".arg2", ... for unnamed ones (added via
          # the DAG UI). Values are the symbols the block expression and the
          # imperative save/download handlers bind data under; names are the
          # display names. Reactive on the link set.
          arg_names <- reactive({
            dot_arg_refs(...args)
          })

          r_filename <- reactiveVal(filename)
          r_format <- reactiveVal(format)
          r_auto_write <- reactiveVal(auto_write)
          r_args <- reactiveVal(args)

          # Server saving is on while the block has a folder, so the state
          # needs no switch of its own: `directory` is the folder while the
          # "Save to the server" box is checked and "" while it is not. The
          # folder itself is kept for the session, so unchecking and checking
          # again brings it back.
          r_server <- reactiveVal(nzchar(directory))
          r_folder <- reactiveVal(directory)
          r_directory <- reactive({
            if (r_server()) r_folder() else ""
          })

          # The last manual save, the last auto-write and the last failure,
          # for the status line.
          r_saved <- reactiveVal(NULL)
          r_auto_time <- reactiveVal(NULL)
          r_error <- reactiveVal("")

          # Data directory from board options
          data_dir_reactive <- reactive({
            coal(get_board_option_or_null("data_dir", session), "")
          })

          # Path input for the folder. `value` hands the module the job of
          # keeping the field in step, which it can do after a dock panel
          # mounts and a block cannot. It also strips the data-directory
          # prefix for display.
          dir_path <- path_input_server(
            "dir_path",
            data_dir = data_dir_reactive,
            mode = "directory",
            value = r_folder
          )

          observeEvent(dir_path(), {
            path_val <- dir_path()
            req(nzchar(path_val))
            r_folder(path_val)
          }, ignoreInit = TRUE)

          observeEvent(input$server, {
            r_server(isTRUE(input$server))
          }, ignoreInit = TRUE)

          # Resolve the folder against data_dir for I/O operations
          resolved_directory <- reactive({
            dir_val <- r_directory()
            if (!nzchar(dir_val)) return("")
            resolve_data_dir(dir_val, data_dir_reactive())
          })

          # Deployment file-access policy: a folder outside the allowed
          # roots is rejected before anything is written. Non-empty is the
          # reason. Gates the save handler and the auto-write expression.
          r_policy <- reactive({
            dir_val <- resolved_directory()
            if (!nzchar(dir_val)) return("")
            tryCatch(
              {
                resolve_and_check(dir_val, "write")
                ""
              },
              error = function(e) conditionMessage(e)
            )
          })

          r_dir_ok <- reactive(!nzchar(r_policy()))

          observeEvent(input$save_mode, {
            r_auto_write(identical(input$save_mode, "change"))
          })

          observeEvent(input$filename, r_filename(input$filename))

          # The select shows the formats' names ("CSV"); the state keeps
          # the identifier ("csv").
          observeEvent(input$format, {
            r_format(write_format_value(input$format))
          })

          # The chosen format's options, as declared for writing. The fields
          # report at bind, so a block starts with its options filled in, as
          # it always did.
          write_opts <- reactive(write_format_options(r_format()))
          opt_id <- function(nm) paste0("opt_", nm)

          output$format_opts <- renderUI({
            format_option_fields(
              write_opts(),
              function(nm) session$ns(opt_id(nm)),
              isolate(r_args())
            )
          })
          outputOptions(output, "format_opts", suspendWhenHidden = FALSE)

          observe({
            specs <- write_opts()
            cur <- isolate(r_args())
            nxt <- cur
            for (nm in names(specs)) {
              raw <- input[[opt_id(nm)]]
              if (!is.null(raw)) {
                nxt[[nm]] <- write_opt_value(specs[[nm]], raw)
              }
            }
            if (!identical(nxt, cur)) {
              r_args(nxt)
            }
          })

          # Auto-write filename: empty means a FIXED "data" file, overwritten
          # on each change; a timestamp here would litter the directory with
          # one file per upstream invalidation.
          auto_filename <- function() {
            if (nzchar(r_filename())) r_filename() else "data"
          }

          # Full output path for a given base filename (single source for the
          # write expression and the status line: one computation, no
          # timestamp drift between the reported and the written file).
          output_path <- function(base_filename) {
            needs_zip <- length(arg_names()) > 1 && r_format() != "excel"
            ext <- format_extension(r_format(), needs_zip = needs_zip)
            file.path(resolved_directory(), paste0(base_filename, ext))
          }

          # "Save to server" (On click). The write happens HERE,
          # imperatively, exactly once per click. Keeping it out of the
          # block expression means later upstream changes can never silently
          # rewrite the file.
          observeEvent(input$submit_write, {
            req(length(arg_names()) > 0)
            req(nzchar(r_directory()))
            req(!r_auto_write())
            req(r_dir_ok())

            # Compute the filename once; write_expr() and the status line
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
            # to the unnamed positional keys a live board assigns.
            eval_env <- new.env(parent = baseenv())
            arg_vals <- dot_arg_values(...args)
            for (nm in names(arg_vals)) {
              val <- arg_vals[[nm]]
              assign(nm, if (is.reactive(val)) val() else val, envir = eval_env)
            }

            tryCatch(
              {
                eval(expr, envir = eval_env)
                r_error("")
                r_saved(list(
                  path = output_path(base_filename), time = Sys.time()
                ))
              },
              error = function(e) {
                r_error(sprintf("✗ Write failed: %s", conditionMessage(e)))
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

          # The time of the last auto-write, for the status line. Depends on
          # the data, which is what the auto-write expression reruns on.
          observe({
            req(nzchar(r_directory()))
            req(r_auto_write())
            req(r_dir_ok())
            req(length(arg_names()) > 0)

            dot_arg_values(...args)

            r_auto_time(Sys.time())
          })

          # Download handler -- always available
          output$download_data <- downloadHandler(
            filename = function() {
              base <- generate_filename(r_filename())
              needs_zip <- length(arg_names()) > 1 && r_format() != "excel"
              paste0(base, format_extension(r_format(), needs_zip = needs_zip))
            },
            content = function(file) {
              # One timestamp for the written file and the file search below.
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

              # Bind each input under its reference symbol (.arg1 for unnamed
              # DAG-UI slots, else the link name) -- the same names
              # write_expr() emits.
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
                # Source and destination can be the same file (tests).
                if (normalizePath(generated_file) !=
                      normalizePath(file, mustWork = FALSE)) {
                  file.copy(generated_file, file, overwrite = TRUE)
                }
              }
            }
          )

          # Badge under the folder field: an existing folder, or one that
          # will be created on the first save. Re-sent when the widget says
          # it is on screen, for the same reason the value is.
          observe({
            input[["dir_path-path_text_ready"]]
            dir <- r_folder()
            id <- session$ns("dir_path-path_text")
            full <- if (nzchar(dir)) resolve_data_dir(dir, data_dir_reactive())
            if (nzchar(dir) && dir.exists(full)) {
              session$sendCustomMessage("blockr-path-status", list(
                id = id, text = "Directory", state = "success"
              ))
            } else if (nzchar(dir)) {
              session$sendCustomMessage("blockr-path-status", list(
                id = id, text = "New directory (created on save)",
                state = "info"
              ))
            } else {
              session$sendCustomMessage("blockr-path-status", list(
                id = id, text = "", state = "none"
              ))
            }
          })

          # The face: "Download <FORMAT>", and "Save to server" as the main
          # button beside it while server saving is on and saves on click.
          output$face <- renderUI({
            save_on_click <- r_server() && !r_auto_write()
            div(
              class = "io-write-face",
              if (save_on_click) {
                blockr.ui::blockr_button(
                  session$ns("submit_write"),
                  "Save to server",
                  kind = "main",
                  disabled = !nzchar(r_folder()) || !r_dir_ok()
                )
              },
              blockr.ui::blockr_download_button(
                session$ns("download_data"),
                paste("Download", write_format_label(r_format())),
                kind = if (save_on_click) "secondary" else "main"
              )
            )
          })

          # The status line under the buttons, while server saving is on.
          output$status <- renderUI({
            req(r_server())

            err <- if (nzchar(r_policy())) {
              sprintf("✗ %s", r_policy())
            } else {
              r_error()
            }

            if (nzchar(err)) {
              return(div(class = "io-write-status io-write-status--error", err))
            }

            text <- if (!nzchar(r_folder())) {
              "Not saved yet · no folder chosen"
            } else if (r_auto_write()) {
              last <- r_auto_time()
              paste0(
                "Overwrites ", output_path(generate_filename(auto_filename())),
                " on every change",
                if (!is.null(last)) paste0(" · last ", format(last, "%H:%M"))
              )
            } else if (is.null(r_saved())) {
              base <- if (nzchar(r_filename())) {
                generate_filename(r_filename())
              } else {
                "data_<timestamp>"
              }
              paste0("Not saved yet · ", output_path(base))
            } else {
              saved <- r_saved()
              paste0(
                "Saved ", saved$path, " · ", format(saved$time, "%H:%M")
              )
            }

            div(class = "io-write-status", text)
          })

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
      ns <- NS(id)
      tagList(
        io_block_deps(),
        div(
          class = "io-block io-write-block",
          blockr.ui::gear_tray(
            ns("gear"),
            blockr.ui::tray_section(
              "File",
              blockr.ui::select_input(
                ns("format"),
                "Format",
                choices = names(write_formats()),
                selected = write_format_label(format)
              ),
              blockr.ui::text_input(
                ns("filename"),
                "Filename",
                value = filename,
                placeholder = "timestamped"
              ),
              # The chosen format's options, drawn by the server. Its
              # children are fields of this grid (io-blocks.css).
              uiOutput(
                ns("format_opts"),
                class = "blockr-settings__field io-tray-fields"
              )
            ),
            blockr.ui::tray_section(
              "Save to the server",
              div(
                class = "blockr-settings__field blockr-settings__field--full",
                tags$label(
                  class = "blockr-label",
                  `for` = ns("dir_path-path_text"),
                  "Folder"
                ),
                path_input_ui(
                  ns("dir_path"),
                  placeholder = "Folder on the server"
                )
              ),
              blockr.ui::segmented_input(
                ns("save_mode"),
                "Save",
                choices = c("On click" = "click", "On change" = "change"),
                selected = if (isTRUE(auto_write)) "change" else "click"
              ),
              toggle = ns("server"),
              value = nzchar(directory)
            ),
            label = "Write settings"
          ),
          uiOutput(ns("face")),
          uiOutput(ns("status"))
        )
      )
    },
    dat_valid = function(...args) {
      stopifnot(length(...args) >= 1L)
    },
    allow_empty_state = TRUE,
    class = c(class, "rbind_block"),
    expr_type = "bquoted"
  ), dots))
}

# A write format's name ("CSV") for its identifier ("csv").
write_format_label <- function(format) {
  fmts <- write_formats()
  names(fmts)[match(format, fmts)] %||% format
}

# A write format's identifier for what its select reports: the name the
# select shows, or the identifier itself.
write_format_value <- function(x) {
  fmts <- write_formats()
  if (x %in% names(fmts)) unname(fmts[[x]]) else x
}

# A write option's value as the writer takes it. Unlike a read option, a
# value at its default is kept: the block has always carried its csv
# options in full.
write_opt_value <- function(spec, raw) {
  switch(
    spec$type,
    flag = isTRUE(raw),
    number = suppressWarnings(as.numeric(raw)),
    as.character(raw)
  )
}
