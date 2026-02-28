#' Path input widget
#'
#' A Shiny module that provides a text input with server-side file/directory
#' autocomplete. Used by the read and write blocks to replace shinyFiles
#' browser widgets.
#'
#' @param id Module namespace ID.
#' @param prefix Optional initial prefix text shown before the input
#'   (typically the data directory path).
#'
#' @return `path_input_ui()` returns a `tagList` with the widget HTML.
#'   `path_input_server()` returns a `reactive` containing the current
#'   path text value.
#'
#' @importFrom htmltools htmlDependency
#' @importFrom jsonlite toJSON
#' @name path_input
#' @export
path_input_ui <- function(id, prefix = NULL) {
  ns <- NS(id)

  tagList(
    path_input_dep(),
    div(
      class = "blockr-path-input",
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
          class = "blockr-path-text shiny-bound-input",
          placeholder = "Enter file path...",
          autocomplete = "off"
        )
      ),
      div(
        id = ns("path_text_dropdown"),
        class = "blockr-path-dropdown"
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
#'
#' @rdname path_input
#' @export
path_input_server <- function(id, data_dir = reactive(""),
                              mode = c("file", "directory")) {
  mode <- match.arg(mode)

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Register directory listing data object endpoint
    list_url <- session$registerDataObj("list_dir", NULL, function(data, req) {
      query <- parseQueryString(req$QUERY_STRING)
      path_val <- query$path %||% ""

      # Resolve the browse root
      dir_root <- data_dir()

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
      } else {
        dir_to_list <- dirname(full_path)
        name_filter <- tolower(basename(full_path))
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
            if (!ext %in% get_rio_extensions()) next
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
        jsonlite::toJSON(list(items = items), auto_unbox = TRUE)
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
          updateTextInput(session, "path_text", value = stripped)
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
