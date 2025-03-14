#' Upload and read xpt files
#'
#' This block allows to make avaliable a table from an xpt file to a blockr
#' pipeline.
#'
#' @inheritParams haven::read_xpt
#' @param ... Forwarded to [new_block()]
#'
#' @export
#' @rdname xpt
new_readxpt_block <- function(col_select = NULL, skip = 0, n_max = Inf, ...) {
  new_data_block(
    function(id) {
      moduleServer(
        id,
        function(input, output, session) {
          col_sel <- reactiveVal(col_select)
          skip_lines <- reactiveVal(skip)
          max_lines <- reactiveVal(n_max)

          # Update state
          observeEvent(input$col_select, col_sel(input$col_select))
          observeEvent(input$skip, skip_lines(input$skip))
          observeEvent(input$n_max, max_lines(input$n_max))

          list(
            expr = reactive({
              req(nchar(input$upload$datapath) > 0)
              bquote(
                haven::read_xpt(
                  file = .(file),
                  col_select = .(col_select),
                  skip = .(skip),
                  n_max = .(n_max)
                ),
                list(
                  file = input$upload$datapath,
                  col_select = col_sel(),
                  skip = skip_lines(),
                  n_max = max_lines()
                )
              )
            }),
            state = list(
              col_select = col_sel,
              skip = skip_lines,
              n_max = max_lines
            )
          )
        }
      )
    },
    function(id) {
      tagList(
        fileInput(
          NS(id, "upload"),
          "Upload data"
        ),
        textInput(
          inputId = NS(id, "col_select"),
          label = "Column selection",
          value = col_select
        ),
        numericInput(
          inputId = NS(id, "skip"),
          label = "Number of lines to skip",
          value = skip,
          min = 0,
          max = NA
        ),
        numericInput(
          inputId = NS(id, "n_max"),
          label = "Max number of lines to read",
          value = n_max,
          min = 0,
          max = NA
        )
      )
    },
    class = "readxpt_block",
    allow_empty_state = TRUE,
    ...
  )
}

#' @rdname xpt
#' @export
new_writexpt_block <- function(...) {
  new_transform_block(
    function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {
          output$dl <- downloadHandler(
            xpt_download_filename,
            xpt_download_content(data)
          )

          list(
            expr = reactive(quote(identity(data))),
            state = list()
          )
        }
      )
    },
    function(id) {
      tagList(
        downloadButton(
          NS(id, "dl"),
          "Download"
        )
      )
    },
    class = "writexpt_block",
    ...
  )
}

xpt_download_filename <- function() {
  paste0("blockr_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".xpt")
}

xpt_download_content <- function(data) {
  function(file) {
    haven::write_xpt(data(), file)
  }
}
