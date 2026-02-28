#' Data directory board option
#'
#' A board-level option that sets a default data directory for read and write
#' blocks. When set, file paths entered in blocks are resolved relative to
#' this directory.
#'
#' @param value Character. Initial directory path. Default: empty string
#'   (no data directory).
#' @param category Character. Option category for UI grouping.
#' @param ... Forwarded to [blockr.core::new_board_option()].
#'
#' @return A `board_option` object.
#' @export
new_data_dir_option <- function(value = blockr_option("data_dir", ""),
                                category = "Data", ...) {
  new_board_option(
    id = "data_dir",
    default = value,
    update_trigger = NULL,
    ui = function(id) {
      ns <- NS(id)
      tagList(
        path_input_dep(),
        tags$label("Data directory"),
        div(
          class = "blockr-path-input",
          div(
            class = "blockr-path-input-field",
            tags$input(
              id = ns("data_dir_browse"),
              type = "text",
              class = "blockr-path-text",
              placeholder = "e.g. /data/project",
              autocomplete = "off"
            ),
            div(
              id = ns("data_dir_browse_dropdown"),
              class = "blockr-path-dropdown"
            )
          )
        ),
        div(
          style = "margin-top: 6px;",
          actionButton(ns("data_dir_set"), "Set data directory",
            class = "btn-sm blockr-datadir-btn"
          )
        )
      )
    },
    server = function(..., session) {
      ns <- session$ns

      # Register directory listing endpoint (directories only)
      list_url <- session$registerDataObj(
        "list_dir_opt", NULL,
        function(data, req) {
          query <- parseQueryString(req$QUERY_STRING)
          path_val <- query$path %||% ""

          if (grepl("/$", path_val) || dir.exists(path_val)) {
            dir_to_list <- path_val
            name_filter <- ""
          } else {
            dir_to_list <- dirname(path_val)
            name_filter <- tolower(basename(path_val))
          }

          if (!nzchar(dir_to_list)) {
            dir_to_list <- "."
          }

          items <- list()

          if (nzchar(dir_to_list) && dir.exists(dir_to_list)) {
            entries <- list.files(
              dir_to_list, all.files = FALSE, full.names = FALSE
            )

            if (nzchar(name_filter)) {
              entries <- entries[
                grepl(name_filter, tolower(entries), fixed = TRUE)
              ]
            }

            for (entry in entries) {
              full <- file.path(dir_to_list, entry)
              if (!dir.exists(full)) next
              items[[length(items) + 1]] <- list(
                name = entry,
                isdir = TRUE,
                size = NULL
              )
            }

            names_vec <- vapply(items, function(x) x$name, character(1))
            if (length(names_vec)) {
              items <- items[order(tolower(names_vec))]
            }

            if (length(items) > 50) {
              items <- items[seq_len(50)]
            }
          }

          httpResponse(
            200,
            "application/json",
            jsonlite::toJSON(list(items = items), auto_unbox = TRUE)
          )
        }
      )

      # Send endpoint URL to JS + disable button on init
      observe({
        session$sendCustomMessage("blockr-path-list-url", list(
          id = ns("data_dir_browse"),
          url = list_url
        ))
        session$sendCustomMessage("blockr-path-toggle-btn", list(
          id = ns("data_dir_set"),
          enabled = FALSE
        ))
      })

      list(
        # Sync input when option changes externally (e.g. session restore)
        observeEvent(
          get_board_option_or_null("data_dir", session),
          {
            val <- get_board_option_value("data_dir", session)
            session$sendCustomMessage("blockr-path-set-value", list(
              id = ns("data_dir_browse"),
              value = val
            ))
          }
        ),
        # Validate as user types: enable when valid AND changed from saved
        observeEvent(
          session$input[["data_dir_browse"]],
          {
            val <- session$input[["data_dir_browse"]] %||% ""
            saved <- tryCatch(
              get_board_option_value("data_dir", session),
              error = function(e) ""
            )
            norm_val <- normalizePath(val, winslash = "/", mustWork = FALSE)
            valid <- nzchar(val) && dir.exists(val)
            changed <- !identical(norm_val, saved)
            session$sendCustomMessage("blockr-path-toggle-btn", list(
              id = ns("data_dir_set"),
              enabled = valid && changed
            ))
          },
          ignoreInit = TRUE
        ),
        # Confirm button: set the value, show success state
        observeEvent(
          session$input[["data_dir_set"]],
          {
            val <- session$input[["data_dir_browse"]] %||% ""
            val <- normalizePath(val, winslash = "/")
            set_board_option_value("data_dir", val, session)
            # Update input to normalized path
            session$sendCustomMessage("blockr-path-set-value", list(
              id = ns("data_dir_browse"),
              value = val
            ))
            # Trigger success animation on button
            session$sendCustomMessage("blockr-path-btn-success", list(
              id = ns("data_dir_set")
            ))
          }
        )
      )
    },
    category = category,
    ...
  )
}

#' @export
board_options.read_block <- function(x, ...) {
  combine_board_options(
    new_data_dir_option(...),
    NextMethod()
  )
}

#' @export
board_options.write_block <- function(x, ...) {
  combine_board_options(
    new_data_dir_option(...),
    NextMethod()
  )
}
