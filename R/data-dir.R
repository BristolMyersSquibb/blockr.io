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
          style = "margin-bottom: 6px;",
          actionButton(ns("data_dir_set"), "Set data directory",
            class = "btn-sm blockr-datadir-btn"
          )
        ),
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
        )
      )
    },
    server = function(board, ..., session) {
      ns <- session$ns

      # Register directory listing endpoint (directories only) — shares
      # the path-input implementation, including the file-access policy.
      list_url <- session$registerDataObj(
        "list_dir_opt", NULL,
        function(data, req) {
          query <- parseQueryString(req$QUERY_STRING)
          path_val <- query$path %||% ""
          list_dir_response(path_val, mode = "directory", policy = "read")
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
        # Validate committed input (Enter/blur/dropdown-select): enable the
        # button when valid AND changed from saved
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
            set_board_option_value("data_dir", val, board$board, session)
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
