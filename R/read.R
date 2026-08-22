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
#'   that differ from defaults. Which parameters a file accepts is declared by
#'   its format's registry entry, so [format_options()] is the live answer and
#'   the settings band is generated from it. As shipped:
#'   - **For CSV files:** `sep` (default: ","), `quote` (default: '"'),
#'     `encoding` (default: "UTF-8"), `skip` (default: 0),
#'     `n_max` (default: Inf), `col_names` (default: TRUE)
#'   - **For Excel files:** `sheet` (default: NULL), `range` (default: NULL),
#'     `skip` (default: 0), `n_max` (default: Inf), `col_names` (default: TRUE)
#'   - **Everything else:** nothing to set, and no settings affordance shown
#' @param ... Forwarded to [blockr.core::new_data_block()]
#'
#' @section External control:
#' `path`, `source`, `combine` and `args` are externally controllable (see
#' [blockr.core::external_ctrl_vars()]), so a board update, an assistant or a
#' parent app can retarget the block with a `mod` delta instead of replacing
#' it. This holds because the block's expression is a pure function of that
#' state: writing `path` moves the read, and the path field, the type badge
#' and the settings band follow. A path that does not resolve is reported on
#' the badge and as a block error rather than failing the constructor, so a
#' board restores even when its data has not landed yet.
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
#' - The path is committed (and the file read) on Enter, blur, or a
#'   dropdown selection — never while typing; an "Enter" chip shows while
#'   the typed path is not yet applied
#' - For server paths: reads directly from original location
#' - For URLs: downloads to a temporary file each time
#' - When a board-level data directory is set, paths are resolved relative to it
#'
#' ## Smart Adaptive UI
#'
#' After file selection the block asks the registry what this format allows
#' ([format_options()]) and generates the settings band from the answer:
#' - **CSV/TSV:** Delimiter, quote character, encoding, skip, max rows, header
#' - **Excel:** Sheet, cell range, skip, max rows, header
#' - **Other formats:** nothing to set, so no gear at all
#'
#' A format registered by another package ([register_format()]) declares its
#' own options and gets fields here without this block changing.
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

  # Read inside the server closure, i.e. after this call returns: left as
  # promises they carry the caller's environment, and any route that revives
  # the app object in a fresh R process (shinytest2, a callr worker) forces
  # them there, where the caller's locals are gone.
  force(path)
  force(source)
  force(combine)
  force(args)

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

          # `path` is the state -- the URL or file path as given, whether or
          # not it resolves here. A board can be restored before its data
          # lands, and an external controller can point the block at a path
          # this session cannot see; both are reported (badge, block error),
          # never turned into a constructor failure that takes the board down
          # with it.
          initial_path <- if (length(path) > 0 && nzchar(path[[1]])) {
            if (is_valid_url(path[[1]])) {
              path[[1]]
            } else {
              set_names(path, basename(path))
            }
          } else {
            character()
          }

          r_path <- reactiveVal(initial_path)

          # Set by the input observers just before they write the state they
          # own, so the observers mirroring that state back into the widgets
          # can skip their own echo.
          self_write <- new.env(parent = emptyenv())
          self_write$combine <- FALSE
          self_write$args <- FALSE

          # What actually gets read. Derived, not stored: an expression that
          # depends on a separate copy of the path is an expression that can
          # disagree with the state the block reports and serializes -- write
          # `path` and the block would go on reading the previous file.
          # A URL is fetched here, so a failed download surfaces as no data
          # rather than as a silent stale read.
          file_paths <- reactive({
            p <- r_path()

            if (!length(p) || !nzchar(p[[1]])) {
              return(character())
            }

            if (is_valid_url(p[[1]])) {
              return(
                tryCatch(
                  {
                    temp_file <- download_url_to_temp(p[[1]])
                    url_display <- basename(
                      strsplit(p[[1]], "?", fixed = TRUE)[[1]][1]
                    )
                    set_names(temp_file, url_display)
                  },
                  error = function(e) character()
                )
              )
            }

            set_names(unname(p), basename(unname(p)))
          })

          detected_type <- reactive({
            paths <- file_paths()

            if (!length(paths)) {
              return("unknown")
            }

            file_category(resolve_data_dir(paths[[1]], data_dir_reactive()))
          })

          # Non-empty when the deployment's file-access policy rejects the
          # current path. Derived on the same terms for a typed path, a
          # restored one and an externally set one.
          r_path_blocked <- reactive({
            paths <- file_paths()

            if (!length(paths)) {
              return("")
            }

            blocked <- ""

            for (p in resolve_data_dir(paths, data_dir_reactive())) {
              if (is_valid_url(p)) next
              if (in_app_sandbox(p, upload_path)) next
              blocked <- tryCatch(
                {
                  resolve_and_check(p, "read")
                  ""
                },
                error = function(e) conditionMessage(e)
              )
              if (nzchar(blocked)) break
            }

            blocked
          })

          observeEvent(input$combine, {
            self_write$combine <- TRUE
            r_combine(input$combine)
          })

          # What this file's format says can be set when reading it. The
          # block asks rather than knowing: a format registered by another
          # package declares its own options and gets fields here without
          # this block changing (and a format with nothing to tune -- most
          # of them -- gets no fields and no gear).
          opt_specs <- reactive({
            paths <- file_paths()

            if (!length(paths)) {
              return(list())
            }

            source_options(paths[[1]], "single")
          })

          # Fields -> state, for every declared option at once. Values still
          # at their default drop out, so a block nobody has touched carries
          # no options and serializes clean.
          observe({
            specs <- opt_specs()
            req(length(specs) > 0)

            mounted <- any(
              vapply(names(specs), function(nm) !is.null(input[[nm]]), logical(1))
            )

            # Before the band renders every field reads NULL, which is not
            # the user clearing them: writing then would wipe the options a
            # board was constructed or restored with.
            req(mounted)

            vals <- format_options_values(input, specs)

            if (!identical(vals, isolate(r_args()))) {
              self_write$args <- TRUE
              r_args(vals)
            }
          })

          # Data directory from board options
          data_dir_reactive <- reactive({
            coal(get_board_option_or_null("data_dir", session), "")
          })

          # Path input module for "Location" tab. Handing it `value` makes
          # the module responsible for keeping the field in step with the
          # block's path -- including when the block's dock panel mounts
          # long after the first push, which a block pushing on its own
          # cannot get right.
          file_path <- path_input_server(
            "file_path",
            data_dir = data_dir_reactive,
            mode = "file",
            value = r_path
          )

          # JS -> R: the field commits on Enter, blur and browse, so this is
          # one write per user decision. Validation is not this observer's
          # job -- it records what was asked for, and the derived reactives
          # above say what came of it.
          observeEvent(file_path(), {
            path_val <- file_path()
            req(nzchar(path_val))

            # A field reports its value the moment it binds, and inside a
            # dock that happens after the observer above has written the
            # block's own path into it. That echo is not a user choosing a
            # file: acting on it renames an uploaded file to its stored
            # basename and relabels the source as a path.
            current <- r_path()

            if (length(current) && identical(unname(current[[1]]), path_val)) {
              return()
            }

            r_path(
              if (is_valid_url(path_val)) {
                path_val
              } else {
                set_names(path_val, basename(path_val))
              }
            )

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

            # The `r_path()` observer above mirrors this into the text field.
            r_path(permanent_paths)

            # Update source to "upload" now that we have uploaded files
            r_source("upload")
          })

          # R -> JS for the remaining state. Without these an externally set
          # `combine` or `args` would drive the read while the settings band
          # kept showing the old values, and the user's next edit would revert
          # what was set. The guard skips the echo of the user's own edits.
          observeEvent(r_combine(), {
            if (self_write$combine) {
              self_write$combine <- FALSE
              return()
            }
            updateSelectInput(session, "combine", selected = r_combine())
          }, ignoreInit = TRUE)

          # State -> fields, for an external write. Moved rather than
          # re-rendered, so a write mid-edit does not drop focus.
          observeEvent(r_args(), {
            if (self_write$args) {
              self_write$args <- FALSE
              return()
            }
            format_options_update(session, opt_specs(), r_args())
          }, ignoreInit = TRUE, ignoreNULL = FALSE)

          # Combination strategy info
          output$combine_info <- renderText({
            current_file_paths <- file_paths()
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

          # Does the block's path resolve to an existing file? Read off the
          # state, not the input, so the badge follows an externally set path
          # as well as a typed one.
          path_resolved <- reactive({
            paths <- resolve_data_dir(file_paths(), data_dir_reactive())
            if (!length(paths) || is_valid_url(paths[[1]])) return(TRUE)
            all(file.exists(paths) | dir.exists(paths))
          })

          # Status badge for file type. Re-sent when the widget reports it
          # is on screen, for the same reason the value is: the first push
          # can predate the panel that carries the badge.
          observe({
            input[["file_path-path_text_ready"]]
            type <- detected_type()
            paths <- file_paths()
            resolved <- path_resolved()
            type_labels <- c(
              csv = "CSV", excel = "Excel", arrow = "Parquet",
              statistical = "Stats", web = "Web data",
              r_format = "R data", other = "File"
            )
            is_dir <- length(paths) > 0 &&
              any(dir.exists(resolve_data_dir(paths, data_dir_reactive())))
            if (nzchar(r_path_blocked())) {
              session$sendCustomMessage("blockr-path-status", list(
                id = session$ns("file_path-path_text"),
                text = "Blocked",
                state = "error"
              ))
            } else if (is_dir) {
              # A directory resolves (path_resolved() counts dir.exists), but
              # this block cannot read one -- without this branch it badges
              # as a readable "File".
              session$sendCustomMessage("blockr-path-status", list(
                id = session$ns("file_path-path_text"),
                text = "Directory",
                state = "error"
              ))
            } else if (length(paths) > 0 && type != "unknown") {
              label <- unname(type_labels[type]) %||% "File"
              session$sendCustomMessage("blockr-path-status", list(
                id = session$ns("file_path-path_text"),
                text = label,
                state = "success"
              ))
            } else if (!resolved && length(paths) > 0) {
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

          # The format's declared fields, generated. Depends on the spec set
          # only, with the values isolated: re-rendering on every keystroke
          # would fight the user for the cursor. It also means a dock panel
          # that mounts late renders its fields already carrying the block's
          # options, rather than waiting for a push it may have missed.
          output$format_options <- renderUI({
            format_options_ui(opt_specs(), session$ns, isolate(r_args()))
          })
          outputOptions(output, "format_options", suspendWhenHidden = FALSE)

          output$show_format_options <- reactive({
            length(opt_specs()) > 0
          })
          outputOptions(
            output, "show_format_options", suspendWhenHidden = FALSE
          )

          output$show_multi_file_options <- reactive({
            length(file_paths()) > 1
          })
          outputOptions(
            output,
            "show_multi_file_options",
            suspendWhenHidden = FALSE
          )

          # No options, no gear. Both halves are now the registry's answer:
          # a format that declares nothing -- a parquet, a registered rtf,
          # rio's long tail -- opens no band, and the multi-file combine
          # section is the block's own option rather than a format's.
          output$show_gear <- reactive({
            length(opt_specs()) > 0 || length(file_paths()) > 1
          })
          outputOptions(output, "show_gear", suspendWhenHidden = FALSE)

          list(
            expr = reactive({
              paths <- resolve_data_dir(file_paths(), data_dir_reactive())

              # The file-access policy is enforced here, the single point
              # where a path becomes a read, and a rejection rides in the
              # expression so blockr.core's per-block error boundary reports
              # it. URL downloads and uploads land in app-managed sandboxes
              # and are exempt -- only user-chosen paths are policed.
              blocked <- r_path_blocked()

              if (nzchar(blocked)) {
                return(bquote(stop(.(blocked), call. = FALSE)))
              }

              # A directory is a container, not a file: it holds several
              # tables and this block returns one data frame. Said on the
              # block rather than thrown from this reactive -- an unhandled
              # throw here never reaches the per-block error boundary and
              # leaves a stale preview on screen.
              if (any(dir.exists(paths))) {
                msg <- paste0(
                  "'", basename(paths[dir.exists(paths)][[1]]), "' is a ",
                  "directory. This block reads one file into one data ",
                  "frame; a directory holds several tables and needs a ",
                  "multi-table block."
                )
                return(bquote(stop(.(msg), call. = FALSE)))
              }

              # WHAT THE EXPRESSION NAMES WHEN THE SOURCE IS A URL.
              # `file_paths()` resolves a URL to the temp file it downloaded,
              # which is what detection, the size checks and the "did the
              # fetch fail" signal all need. It is not what the code should
              # say: an exported script or qmd carrying
              # `/tmp/RtmpXXXX/fileYYYY.csv` looks reproducible, runs on the
              # machine that wrote it for as long as the temp dir survives,
              # and fails everywhere else. A block whose whole claim is that
              # its output is runnable elsewhere cannot emit that.
              #
              # So a format that declares `url_ok` (readr's delimited
              # readers) gets the URL back for the emitted literal, while
              # the read itself still goes through the download. Formats
              # that cannot open a connection (readxl, arrow) keep the temp
              # path, because for them it is the only thing that works.
              raw <- r_path()
              emit <- NULL
              if (length(paths) == 1L && length(raw) &&
                    is_valid_url(raw[[1]]) &&
                    format_reads_url(tools::file_ext(paths[[1]]))) {
                emit <- unname(raw[[1]])
              }

              # Use read_expr() to generate expression, passing args via
              # do.call. Build failures (an unregistered extension, a
              # registry collision) ride in the expression for the same
              # reason the blocked-path message does.
              tryCatch(
                do.call(
                  read_expr,
                  c(
                    list(
                      paths = paths,
                      file_type = detected_type(),
                      combine = r_combine(),
                      .emit = emit
                    ),
                    r_args()
                  )
                ),
                error = function(e) {
                  bquote(stop(.(conditionMessage(e)), call. = FALSE))
                }
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
      gear_id <- NS(id, "gear_btn")
      band_id <- NS(id, "gear_band")
      tagList(
        shinyjs::useShinyjs(),
        io_block_deps(),
        div(
          class = "block-container io-block read-block-container",
          div(
            class = "block-section io-file-location",
            conditionalPanel(
            condition = "output['show_gear']",
            ns = NS(id),
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

            # Settings band: persistent in-flow panel between the gear row
            # and the path input. Visibility is class-driven (closed = no
            # blockr-settings--open); blockrIoGearToggle() flips it.
            div(
              id = band_id,
              class = "blockr-settings blockr-settings--beak",
              role = "region",
              `aria-label` = "Read settings",

              tags$p(
                "Change the global data directory in the sidebar",
                class = "blockr-path-hint blockr-settings__field--full"
              ),

              conditionalPanel(
                condition = "output['show_format_options']",
                ns = NS(id),
                div(class = "blockr-settings__title", "Format options"),
                uiOutput(NS(id, "format_options"))
              ),

              conditionalPanel(
                condition = "output['show_multi_file_options']",
                ns = NS(id),
                class = "blockr-settings__grid",
                div(class = "blockr-settings__title", "Multi-file options"),
                div(
                  class = "blockr-settings__field",
                  tags$label(
                    class = "blockr-label",
                    `for` = NS(id, "combine"),
                    "Combination strategy"
                  ),
                  selectInput(
                    inputId = NS(id, "combine"),
                    label = NULL,
                    choices = c(
                      "Auto (rbind with fallback)" = "auto",
                      "Row bind (rbind)" = "rbind",
                      "Column bind (cbind)" = "cbind",
                      "First file only" = "first"
                    ),
                    selected = combine,
                    width = "100%"
                  ),
                  div(
                    class = "block-help-text",
                    textOutput(NS(id, "combine_info"))
                  )
                )
              )
            )
            ),

            # The placeholder already covers browsing and uploading, and the
            # Enter chip covers committing. A URL and a dropped file are the
            # two things it accepts that nothing on screen reveals.
            tags$p(
              "Also accepts a pasted URL, or a file dropped here.",
              class = "blockr-path-hint"
            ),

            # Hidden fileInput (Shiny handles upload mechanics)
            div(
              class = "blockr-file-input",
              fileInput(
                inputId = NS(id, "file_upload"),
                label = NULL,
                multiple = TRUE,
                # file_extensions(), not the rio list: a registered format
                # must be uploadable, or it is dead on arrival in upload mode
                accept = paste0(".", file_extensions())
              )
            ),

            # Unified path input with upload icon
            path_input_ui(
              NS(id, "file_path"),
              upload_id = NS(id, "file_upload"),
              required = TRUE
            )
          )
        )
      )
    },
    class = "read_block",
    allow_empty_state = TRUE,
    external_ctrl = c("path", "source", "combine", "args"),
    ...
  )
}
