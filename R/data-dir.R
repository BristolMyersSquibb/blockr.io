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
    ui = function(id) {
      tagList(
        path_input_ui(NS(id, "data_dir_path"), prefix = NULL),
        uiOutput(NS(id, "data_dir_status"))
      )
    },
    server = function(..., session) {
      dir_val <- path_input_server(
        "data_dir_path",
        data_dir = reactive(""),
        mode = "directory"
      )

      # Update board option when path changes
      observeEvent(dir_val(), {
        val <- dir_val()
        set_board_option_value("data_dir", val, session = session)
      }, ignoreInit = TRUE)

      # Sync UI when option value changes externally
      observeEvent(
        get_board_option_or_null("data_dir", session),
        {
          current <- get_board_option_value("data_dir", session)
          updateTextInput(session, NS("data_dir_path", "path_text"),
                          value = current)
        }
      )

      # Status output
      output$data_dir_status <- renderUI({
        val <- get_board_option_or_null("data_dir", session)
        if (is.null(val) || !nzchar(val)) {
          tags$small(class = "text-muted", "No data directory set")
        } else if (!dir.exists(val)) {
          tags$small(class = "text-warning", paste("Directory not found:", val))
        } else {
          n <- length(list.files(val))
          tags$small(class = "text-muted", paste0(val, " (", n, " files)"))
        }
      })
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
