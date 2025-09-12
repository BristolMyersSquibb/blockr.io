#' Read multiple files
#'
#' This block allows to read multiple files at once using [rio::import()].
#'
#' @param paths File path(s)
#' @param volumes Volume path(s)
#' @param ... Forwarded to [new_block()]
#'
#' @rdname mutli
#' @export
new_readmulti_block <- function(paths = character(),
                                volumes = filebrowser_volumes(),
                                ...) {
  new_data_block(
    function(id) {
      moduleServer(
        id,
        function(input, output, session) {

          paths_rv <- reactiveVal(set_names(paths, basename(paths)))

          files <- reactive(
            shinyFiles::parseFilePaths(volumes, input$paths)$datapath
          )

          shinyFiles::shinyFileChoose(input, "paths", roots = volumes)

          observeEvent(
            files(),
            {
              req(files())
              paths_rv(set_names(files(), basename(files())))
            }
          )

          list(
            expr = reactive(
              bquote(
                lapply(.(files), rio::import),
                list(files = paths_rv())
              )
            ),
            state = list(
              paths = paths_rv,
              volumes = volumes
            )
          )
        }
      )
    },
    function(id) {
      shinyFiles::shinyFilesButton(
        NS(id, "paths"),
        label = "File(s) selection",
        title = "Please select file(s)",
        multiple = TRUE
      )
    },
    class = "readmulti_block",
    ...
  )
}

#' @export
block_output.readmulti_block <- function(x, result, session) {

  result <- data.frame(
    Files = names(result),
    Dimensions = chr_ply(lapply(result, dim), paste0, collapse = " x "),
    Names = chr_ply(lapply(result, names), paste0, collapse = ", ")
  )

  NextMethod()
}

#' @param which List selection
#' @rdname mutli
#' @export
new_pick_block <- function(which = character(), ...) {
  new_transform_block(
    function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {

          observeEvent(
            names(data()),
            {
              updateSelectInput(
                session,
                inputId = "which",
                choices = names(data()),
                selected = input$which
              )
            }
          )

          list(
            expr = reactive(
              bquote(data[[.(i)]], list(i = input$which))
            ),
            state = list(
              which = reactive(input$which)
            )
          )
        }
      )
    },
    function(id) {
      selectInput(
        inputId = NS(id, "which"),
        label = "Selection",
        choices = which,
        selected = which
      )
    },
    class = "pick_block",
    ...
  )
}
