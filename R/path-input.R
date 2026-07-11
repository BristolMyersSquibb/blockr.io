#' Path input widget
#'
#' A Shiny module that provides a text input with server-side file/directory
#' autocomplete. Used by the read and write blocks to replace shinyFiles
#' browser widgets.
#'
#' @section Commit model:
#' Typing never commits: keystrokes only drive the autocomplete dropdown.
#' The reactive value returned by `path_input_server()` updates when the user
#' *commits* — by pressing Enter, leaving the field (blur), or selecting a
#' dropdown entry. While the typed text differs from the committed value, the
#' field shows an "Enter ↵" chip; committing collapses it to a faded
#' check mark. This follows the blockr design-system text-commit convention
#' (decided 2026-07-02) and keeps half-typed paths from reaching the pipeline.
#'
#' @param id Module namespace ID.
#' @param prefix Optional initial prefix text shown before the input
#'   (typically the data directory path).
#' @param upload_id Optional ID of a hidden Shiny `fileInput` to wire up
#'   for upload-icon click and drag-and-drop. When non-NULL, an upload icon
#'   button is rendered inside the input field and the container gets a
#'   `data-upload-target` attribute pointing to this ID.
#' @param placeholder Placeholder text for the input. Defaults to an
#'   upload-aware file hint; pass e.g. "Enter directory path..." for
#'   directory-mode inputs.
#' @param required Whether the field must be filled. When `TRUE`, an empty
#'   field carries a soft amber "needs a value" cue via the canonical
#'   `.blockr-field--required-empty` class (mirroring blockr.viz's
#'   required-empty mapping affordance) that clears once a value is entered.
#'
#' @return `path_input_ui()` returns a `tagList` with the widget HTML.
#'   `path_input_server()` returns a `reactive` containing the committed
#'   path text value.
#'
#' @importFrom htmltools htmlDependency HTML
#' @importFrom jsonlite toJSON
#' @name path_input
#' @export
path_input_ui <- function(id, prefix = NULL, upload_id = NULL,
                          placeholder = NULL, required = FALSE) {
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

  if (is.null(placeholder)) {
    placeholder <- if (!is.null(upload_id)) {
      "Browse server or upload file..."
    } else {
      "Enter file path..."
    }
  }

  tagList(
    path_input_dep(),
    div(
      class = "blockr-path-input",
      `data-upload-target` = upload_id,
      `data-required` = if (required) "true",
      div(
        # Required-but-empty file fields render with the amber cue from the
        # start (the input is empty at UI render); JS clears it once a value
        # is entered / committed.
        class = paste(
          "blockr-path-input-field",
          if (required) "blockr-field--required-empty"
        ),
        tags$span(
          id = ns("path_text_prefix"),
          class = "blockr-path-prefix",
          prefix
        ),
        tags$input(
          id = ns("path_text"),
          type = "text",
          class = "blockr-path-text",
          placeholder = placeholder,
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
#' @param policy Which deployment file-access verifier applies to the
#'   directory-listing endpoint: `"read"` or `"write"` (see [file_policy]).
#'   Defaults to `"read"` for file mode and `"write"` for directory mode.
#'
#' @rdname path_input
#' @export
path_input_server <- function(id, data_dir = reactive(""),
                              mode = c("file", "directory"),
                              extensions = NULL,
                              policy = NULL) {
  mode <- match.arg(mode)

  if (is.null(policy)) {
    policy <- if (identical(mode, "file")) "read" else "write"
  }
  policy <- match.arg(policy, c("read", "write"))

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

      list_dir_response(
        path_val,
        dir_root = dir_root,
        mode = mode,
        extensions = extensions,
        policy = policy
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

#' Directory-listing response for path autocomplete
#'
#' Shared implementation behind the `registerDataObj` endpoints of
#' [path_input_server()] and [new_data_dir_option()]. Resolves the typed
#' value to a directory to list, applies the deployment file-access policy
#' (see [file_policy]) to that directory, and returns a JSON `httpResponse`
#' with up to 50 entries plus the total match count.
#'
#' Entries are ranked directories first, then prefix matches on the typed
#' fragment before substring matches, then alphabetically. All entries of a
#' directory are stat-ed with a single vectorized [file.info()] call.
#'
#' @param path_val The raw text typed in the input.
#' @param dir_root Optional data directory against which relative input is
#'   resolved. Empty string means no data directory.
#' @param mode `"file"` lists directories plus files with allowed extensions;
#'   `"directory"` lists directories only.
#' @param extensions Optional file extension allowlist (without dots) for
#'   file mode; `NULL` means all rio-supported formats.
#' @param policy `"read"` or `"write"`, selecting which deployment verifier
#'   gates the listing.
#' @return A [shiny::httpResponse()] with JSON body
#'   `{items, base, total}`.
#' @keywords internal
list_dir_response <- function(path_val, dir_root = "", mode = "file",
                              extensions = NULL, policy = "read") {
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
  total <- 0L

  # Deployment file-access policy: never list a directory the session could
  # not read from / write to — otherwise the autocomplete leaks names and
  # sizes across the whole filesystem even under a within_dirs() allowlist.
  allowed <- dir.exists(dir_to_list) && tryCatch(
    {
      resolve_and_check(dir_to_list, policy)
      TRUE
    },
    error = function(e) FALSE
  )

  if (allowed) {
    entries <- list.files(dir_to_list, all.files = FALSE, full.names = FALSE)

    # Filter by partial name match
    if (nzchar(name_filter)) {
      entries <- entries[grepl(name_filter, tolower(entries), fixed = TRUE)]
    }

    if (length(entries) > 0) {
      # One vectorized stat pass instead of two per-entry calls
      info <- file.info(file.path(dir_to_list, entries))
      is_dir <- !is.na(info$isdir) & info$isdir
      size <- info$size

      keep <- if (identical(mode, "directory")) {
        is_dir
      } else {
        ext <- tolower(tools::file_ext(entries))
        ext_allowed <- extensions %||% get_rio_extensions()
        is_dir | ext %in% ext_allowed
      }
      entries <- entries[keep]
      is_dir <- is_dir[keep]
      size <- size[keep]

      # Rank: directories first, prefix matches before substring matches,
      # then alphabetical — so the entry being typed toward stays visible
      # even when the listing is capped.
      is_prefix <- if (nzchar(name_filter)) {
        startsWith(tolower(entries), name_filter)
      } else {
        rep(TRUE, length(entries))
      }
      ord <- order(!is_dir, !is_prefix, tolower(entries))
      entries <- entries[ord]
      is_dir <- is_dir[ord]
      size <- size[ord]

      total <- length(entries)

      # Limit to 50 entries (total lets the client show what was dropped)
      if (total > 50L) {
        entries <- entries[seq_len(50L)]
        is_dir <- is_dir[seq_len(50L)]
        size <- size[seq_len(50L)]
      }

      items <- lapply(seq_along(entries), function(i) {
        if (is_dir[i]) {
          list(name = entries[i], isdir = TRUE)
        } else {
          list(name = entries[i], isdir = FALSE, size = size[i])
        }
      })
    }
  }

  httpResponse(
    200,
    "application/json",
    jsonlite::toJSON(
      list(items = items, base = query_base, total = total),
      auto_unbox = TRUE
    )
  )
}

#' htmlDependency for path input widget assets
#' @keywords internal
path_input_dep <- memoise0(function() {
  htmltools::htmlDependency(
    name = "blockr-path-input",
    version = "0.3.0",
    src = system.file("assets", package = "blockr.io"),
    script = "js/path-input.js",
    stylesheet = "css/path-input.css"
  )
})
