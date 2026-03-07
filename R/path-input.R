#' Path input widget
#'
#' A Shiny module that provides a text input with server-side file/directory
#' autocomplete. Used by the read and write blocks to replace shinyFiles
#' browser widgets.
#'
#' @param id Module namespace ID.
#' @param prefix Optional initial prefix text shown before the input
#'   (typically the data directory path).
#' @param upload_id Optional ID of a hidden Shiny `fileInput` to wire up
#'   for upload-icon click and drag-and-drop. When non-NULL, an upload icon
#'   button is rendered inside the input field and the container gets a
#'   `data-upload-target` attribute pointing to this ID.
#'
#' @return `path_input_ui()` returns a `tagList` with the widget HTML.
#'   `path_input_server()` returns a `reactive` containing the current
#'   path text value.
#'
#' @importFrom htmltools htmlDependency HTML
#' @importFrom jsonlite toJSON
#' @name path_input
#' @export
path_input_ui <- function(id, prefix = NULL, upload_id = NULL) {
  ns <- NS(id)

  upload_btn <- if (!is.null(upload_id)) {
    tags$button(
      class = "blockr-path-upload-btn",
      type = "button",
      title = "Upload file",
      `aria-label` = "Upload file from computer",
      HTML(paste0(
        '<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" ',
        'fill="currentColor" viewBox="0 0 16 16">',
        '<path d="M.5 9.9a.5.5 0 0 1 .5.5v2.5a1 1 0 0 0 1 1h12a1 1 0 0 0 ',
        '1-1v-2.5a.5.5 0 0 1 1 0v2.5a2 2 0 0 1-2 2H2a2 2 0 0 1-2-2v-2.5',
        'a.5.5 0 0 1 .5-.5"/>',
        '<path d="M7.646 1.146a.5.5 0 0 1 .708 0l3 3a.5.5 0 0 1-.708.708',
        'L8.5 2.707V11.5a.5.5 0 0 1-1 0V2.707L5.354 4.854a.5.5 0 1 1',
        '-.708-.708z"/>',
        '</svg>'
      ))
    )
  }

  tagList(
    path_input_dep(),
    div(
      class = "blockr-path-input",
      `data-upload-target` = upload_id,
      div(
        class = "blockr-path-input-field",
        tags$span(
          id = ns("path_text_prefix"),
          class = "blockr-path-prefix",
          prefix
        ),
        tags$input(
          id = ns("path_text"),
          type = "text",
          class = "blockr-path-text",
          placeholder = if (!is.null(upload_id)) {
            "Browse server or upload file..."
          } else {
            "Enter file path..."
          },
          autocomplete = "off"
        ),
        upload_btn,
        div(
          id = ns("path_text_dropdown"),
          class = "blockr-path-dropdown"
        )
      ),
      div(
        id = ns("path_text_status"),
        class = "blockr-path-status"
      )
    )
  )
}

#' @param data_dir Reactive returning the current data directory path
#'   (from board options). Empty string means no data directory.
#' @param mode Either `"file"` or `"directory"`. Controls which entries
#'   are selectable in the autocomplete dropdown.
#' @param extensions Optional character vector of file extensions (without dots)
#'   to show in autocomplete. Defaults to `NULL`, which shows all
#'   rio-supported formats. Use e.g. `"rtf"` to restrict to RTF files only.
#'
#' @rdname path_input
#' @export
path_input_server <- function(id, data_dir = reactive(""),
                              mode = c("file", "directory"),
                              extensions = NULL) {
  mode <- match.arg(mode)

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Register directory listing data object endpoint
    # NOTE: registerDataObj callbacks run in HTTP context, not reactive context.
    # All reactive reads MUST be wrapped in isolate().
    list_url <- session$registerDataObj("list_dir", NULL, function(data, req) {
      query <- parseQueryString(req$QUERY_STRING)
      path_val <- query$path %||% ""

      # Resolve the browse root (isolate — no reactive context in HTTP handler)
      dir_root <- isolate(data_dir())

      # Build the full path to list
      if (nzchar(dir_root) && !grepl("^(/|~|[A-Za-z]:)", path_val)) {
        full_path <- file.path(dir_root, path_val)
      } else {
        full_path <- path_val
      }

      # Extract directory portion to list
      if (grepl("/$", full_path) || dir.exists(full_path)) {
        dir_to_list <- full_path
        name_filter <- ""
        # Base for JS item selection: the input value as a directory
        query_base <- if (!nzchar(path_val) || grepl("/$", path_val)) {
          path_val
        } else {
          paste0(path_val, "/")
        }
      } else {
        dir_to_list <- dirname(full_path)
        name_filter <- tolower(basename(full_path))
        # Base for JS item selection: directory portion of the input value
        slash_pos <- regexpr("^.*/", path_val)
        query_base <- if (slash_pos > 0) {
          regmatches(path_val, slash_pos)
        } else if (grepl("^(~|[A-Za-z]:)$", path_val)) {
          paste0(path_val, "/")
        } else {
          ""
        }
      }

      if (!nzchar(dir_to_list)) {
        dir_to_list <- "."
      }

      items <- list()

      if (nzchar(dir_to_list) && dir.exists(dir_to_list)) {
        entries <- list.files(dir_to_list, all.files = FALSE, full.names = FALSE)

        # Filter by partial name match
        if (nzchar(name_filter)) {
          entries <- entries[grepl(name_filter, tolower(entries), fixed = TRUE)]
        }

        # Build item list
        for (entry in entries) {
          full <- file.path(dir_to_list, entry)
          is_dir <- dir.exists(full)

          # In directory mode, only show directories
          if (mode == "directory" && !is_dir) next

          # In file mode, show both but only data files + directories
          if (mode == "file" && !is_dir) {
            ext <- tolower(tools::file_ext(entry))
            allowed <- extensions %||% get_rio_extensions()
            if (!ext %in% allowed) next
          }

          fi <- file.info(full)
          items[[length(items) + 1]] <- list(
            name = entry,
            isdir = is_dir,
            size = if (is_dir) NULL else fi$size
          )
        }

        # Sort: directories first, then alphabetical
        is_dir_vec <- vapply(items, function(x) x$isdir, logical(1))
        names_vec <- vapply(items, function(x) x$name, character(1))
        ord <- order(!is_dir_vec, tolower(names_vec))
        items <- items[ord]

        # Limit to 50 entries
        if (length(items) > 50) {
          items <- items[seq_len(50)]
        }
      }

      httpResponse(
        200,
        "application/json",
        jsonlite::toJSON(list(items = items, base = query_base), auto_unbox = TRUE)
      )
    })

    # Send the list_dir URL to JS
    observe({
      session$sendCustomMessage("blockr-path-list-url", list(
        id = ns("path_text"),
        url = list_url
      ))
    })

    # Update prefix when data_dir changes
    observe({
      prefix <- data_dir()
      display_prefix <- if (nzchar(prefix)) paste0(prefix, "/") else ""
      session$sendCustomMessage("blockr-path-prefix", list(
        id = ns("path_text"),
        prefix = display_prefix
      ))
    })

    # Auto-strip data directory prefix from pasted absolute paths
    observeEvent(input$path_text, {
      val <- input$path_text
      dir_val <- data_dir()
      if (nzchar(dir_val) && nzchar(val)) {
        # If user pasted an absolute path that starts with the data dir, strip it
        dir_prefix <- paste0(dir_val, "/")
        if (startsWith(val, dir_prefix)) {
          stripped <- substr(val, nchar(dir_prefix) + 1, nchar(val))
          # Use sendCustomMessage so JS updatePrefixVisibility re-runs
          # (updateTextInput doesn't fire DOM input event, leaving prefix hidden)
          session$sendCustomMessage("blockr-path-set-value", list(
            id = ns("path_text"),
            value = stripped
          ))
        }
      }
    }, ignoreInit = TRUE)

    reactive(input$path_text %||% "")
  })
}

#' htmlDependency for path input widget assets
#' @keywords internal
path_input_dep <- function() {
  htmltools::htmlDependency(
    name = "blockr-path-input",
    version = "0.1.0",
    src = system.file("assets", package = "blockr.io"),
    script = "js/path-input.js",
    stylesheet = "css/path-input.css"
  )
}
