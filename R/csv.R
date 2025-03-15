#' Upload and read csv files
#'
#' This block allows to make avaliable a table from an csv file to a blockr
#' pipeline.
#'
#' @inheritParams utils::read.table
#' @param ... Forwarded to [new_block()]
#'
#' @rdname csv
#' @export
new_readcsv_block <- function(sep = ",", quote = "\"", ...) {
  new_data_block(
    function(id) {
      moduleServer(
        id,
        function(input, output, session) {
          sp <- reactiveVal(sep)
          qo <- reactiveVal(quote)

          # Update state
          observeEvent(input$sep, sp(input$sep))
          observeEvent(input$quote, qo(input$quote))

          list(
            expr = reactive({
              req(nchar(input$upload$datapath) > 0)
              cat(input$upload$datapath)
              bquote(
                utils::read.table(
                  file = .(file),
                  header = TRUE,
                  sep = .(sp),
                  quote = .(qo),
                  dec = ".",
                  fill = TRUE,
                  comment.char = ""
                ),
                list(
                  file = input$upload$datapath,
                  sp = sp(),
                  qo = qo()
                )
              )
            }),
            state = list(sep = sp, quote = qo)
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
          inputId = NS(id, "sep"),
          label = "Field separator",
          value = sep
        ),
        textInput(
          inputId = NS(id, "quote"),
          label = "Quoting characters",
          value = quote
        )
      )
    },
    class = "readcsv_block",
    allow_empty_state = TRUE,
    ...
  )
}

#' @rdname csv
#' @export
new_writecsv_block <- function(...) {
  new_transform_block(
    function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {
          output$dl <- downloadHandler(
            csv_download_filename,
            csv_download_content(data)
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
    class = "writecsv_block",
    ...
  )
}

csv_download_filename <- function() {
  paste0("blockr_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".csv")
}

csv_download_content <- function(data) {
  function(file) {
    readr::write_csv(data(), file)
  }
}
