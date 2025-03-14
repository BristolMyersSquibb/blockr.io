#' Upload and read Excel
#'
#' This block allows to make avaliable a table from an Excel file to a blockr
#' pipeline.
#'
#' @param file File name
#' @param sheet,range See [readxl::read_excel()]
#' @param ... Forwarded to [new_block()]
#'
#' @rdname xlsx
#' @export
new_readxlsx_block <- function(sheet = NULL, range = NULL, ...) {

  new_data_block(
    function(id) {
      moduleServer(
        id,
        function(input, output, session) {

          sht <- reactiveVal(sheet)
          rng <- reactiveVal(range)

          observeEvent(input$sheet, sht(input$sheet))
          observeEvent(input$range, rng(input$range))

          list(
            expr = reactive(
              bquote(
                readxl::read_excel(
                  path = .(file),
                  sheet = .(sht),
                  range = .(rng)
                ),
                list(
                  file = input$upload$datapath,
                  sht = zchr_to_null(sht()),
                  rng = zchr_to_null(rng())
                )
              )
            ),
            state = list(sheet = sht, range = rng)
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
          inputId = NS(id, "sheet"),
          label = "Sheet",
          value = sheet
        ),
        textInput(
          inputId = NS(id, "range"),
          label = "Range",
          value = range
        )
      )
    },
    class = "readxlsx_block",
    allow_empty_state = TRUE,
    ...
  )
}

#' @rdname xlsx
#' @export
new_writexlsx_block <- function(...) {

  new_transform_block(
    function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {

          output$dl <- downloadHandler(
            xlsx_download_filename,
            xlsx_download_content(data)
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
    class = "writexlsx_block",
    ...
  )
}

xlsx_download_filename <- function() {
  paste0("blockr_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".xlsx")
}

xlsx_download_content <- function(data) {
  function(file) {
    writexl::write_xlsx(data(), file)
  }
}
