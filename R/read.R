# Define supported formats for rio::import
#' @keywords internal
rio_formats <- list(
  tabular = list(
    extensions = c("csv", "tsv", "txt", "fwf"),
    description = "Tabular text files"
  ),
  excel = list(
    extensions = c("xls", "xlsx", "xlsm", "xlsb"),
    description = "Microsoft Excel"
  ),
  statistical = list(
    extensions = c("sav", "zsav", "dta", "sas7bdat", "xpt", "por"),
    description = "Statistical software (SPSS, Stata, SAS)"
  ),
  r_formats = list(
    extensions = c("rds", "rdata", "rda"),
    description = "R data files"
  ),
  modern = list(
    extensions = c("parquet", "feather", "arrow"),
    description = "Modern columnar formats"
  ),
  spreadsheets = list(
    extensions = c("ods", "fods"),
    description = "OpenDocument spreadsheets"
  ),
  web_data = list(
    extensions = c("json", "xml", "html", "yml", "yaml"),
    description = "Web and config formats"
  ),
  database = list(
    extensions = c("dbf", "sqlite", "db"),
    description = "Database files"
  ),
  other = list(
    extensions = c("csvy", "arff", "rec", "mtp", "syd"),
    description = "Other data formats"
  )
)

# Get all supported extensions as a flat vector
get_rio_extensions <- function() {
  unique(unlist(lapply(rio_formats, function(x) x$extensions)))
}

# Format extensions for file input accept parameter
format_accept_extensions <- function() {
  exts <- get_rio_extensions()
  paste0(".", exts)
}

#' Unified read block
#'
#' This block allows reading various file formats using automatic format detection
#' or file browser selection. Supports single files, multiple files, and combining strategies.
#'
#' @param paths Character vector of file paths to pre-load. Only works with files
#'   that exist on the server. When provided, automatically switches to "path" mode
#'   regardless of the source parameter value.
#' @param source Either "upload" for file upload widget or "path" for file browser.
#'   Note: automatically set to "path" when paths parameter is provided.
#' @param combine Strategy for combining multiple files: "auto", "rbind", "cbind", "first", "error"
#' @param volumes Volume paths for file browser mode. Can be set globally via 
#'   `options(blockr.volumes = c(name = "path"))` or environment variable BLOCKR_VOLUMES
#' @param ... Forwarded to [new_block()]
#'
#' @details
#' When multiple files are selected:
#' - "auto": Attempts rbind, falls back to first file if incompatible
#' - "rbind": Row-bind files (requires same columns)
#' - "cbind": Column-bind files (requires same row count)
#' - "first": Returns only the first file
#' - "error": Throws error if more than one file selected
#'
#' @rdname read
#' @export
new_read_block <- function(
  paths = character(),
  source = "upload",
  combine = "auto",
  volumes = blockr_option("volumes", c(home = path.expand("~"))),
  ...
) {
  
  # Auto-switch to path mode if paths are provided
  # This ensures pre-initialized files work correctly
  if (length(paths) > 0 && source == "upload") {
    source <- "path"
  }
  
  # Validate parameters
  source <- match.arg(source, c("upload", "path"))
  combine <- match.arg(combine, c("auto", "rbind", "cbind", "first", "error"))
  
  # Expand paths if needed
  if (is.character(volumes)) {
    volumes <- path.expand(volumes)
  }
  
  if (is_string(volumes) && grepl(":", volumes)) {
    volumes <- strsplit(volumes, ":", fixed = TRUE)[[1L]]
  }
  
  if (is.null(names(volumes))) {
    if (length(volumes) == 1L) {
      names(volumes) <- "volume"
    } else if (length(volumes) > 1L) {
      names(volumes) <- paste0("volume", seq_along(volumes))
    }
  }
  
  new_data_block(
    function(id) {
      moduleServer(
        id,
        function(input, output, session) {
          # Reactive values for state
          r_source <- reactiveVal(source)
          r_combine <- reactiveVal(combine)
          r_volumes <- reactiveVal(volumes)
          
          # Paths storage - initialize with provided paths if any
          initial_paths <- if (length(paths) > 0) {
            set_names(paths, basename(paths))
          } else {
            character()
          }
          paths_rv <- reactiveVal(initial_paths)
          
          # Update state from inputs
          observeEvent(input$source, r_source(input$source))
          observeEvent(input$combine, r_combine(input$combine))
          
          # Initialize shinyFiles browser (always, not just for path mode)
          # This ensures it works when switching modes dynamically
          shinyFiles::shinyFileChoose(input, "file_browser", 
                                      roots = r_volumes(), 
                                      session = session,
                                      filetypes = get_rio_extensions())
          
          selected_files <- reactive({
            if (!is.null(input$file_browser) && !identical(input$file_browser, "")) {
              shinyFiles::parseFilePaths(r_volumes(), input$file_browser)$datapath
            } else {
              character()
            }
          })
          
          observeEvent(selected_files(), {
            if (length(selected_files()) > 0) {
              paths_rv(selected_files())
            }
          })
          
          # Handle file upload
          observeEvent(input$file_upload, {
            req(input$file_upload)
            upload_paths <- input$file_upload$datapath
            names(upload_paths) <- input$file_upload$name
            paths_rv(upload_paths)
          })
          
          # File info for display
          output$file_info <- renderText({
            current_paths <- paths_rv()
            if (length(current_paths) == 0) {
              return("No files selected")
            }
            
            if (length(current_paths) == 1) {
              paste("Selected:", basename(names(current_paths)[1] %||% current_paths[1]))
            } else {
              paste("Selected", length(current_paths), "files")
            }
          })
          
          # Combination strategy info
          output$combine_info <- renderText({
            current_paths <- paths_rv()
            if (length(current_paths) <= 1) return("")
            
            strategy <- r_combine()
            switch(strategy,
              "auto" = "Will attempt to row-bind files, fallback to first file",
              "rbind" = "Will row-bind files (requires same columns)",
              "cbind" = "Will column-bind files (requires same row count)",
              "first" = "Will use only the first file",
              "error" = "Will error with multiple files"
            )
          })
          
          list(
            expr = reactive({
              current_paths <- paths_rv()
              req(length(current_paths) > 0)
              
              strategy <- r_combine()
              
              if (length(current_paths) == 1) {
                # Single file - simple case
                bquote(
                  rio::import(.(path)),
                  list(path = unname(current_paths[1]))
                )
              } else {
                # Multiple files
                if (strategy == "error") {
                  stop("Multiple files selected but combine strategy is 'error'")
                }
                
                if (strategy == "first") {
                  bquote(
                    rio::import(.(path)),
                    list(path = unname(current_paths[1]))
                  )
                } else {
                  # Need to combine files
                  bquote({
                    files_data <- lapply(.(paths), rio::import)
                    
                    if (.(strategy) == "cbind") {
                      do.call(cbind, files_data)
                    } else {
                      # rbind or auto
                      tryCatch({
                        do.call(rbind, files_data)
                      }, error = function(e) {
                        if (.(strategy) == "auto") {
                          # Fallback to first file
                          files_data[[1]]
                        } else {
                          stop("Cannot rbind files: ", e$message)
                        }
                      })
                    }
                  }, list(
                    paths = unname(current_paths),
                    strategy = strategy
                  ))
                }
              }
            }),
            state = list(
              paths = paths_rv,
              source = r_source,
              combine = r_combine,
              volumes = r_volumes
            )
          )
        }
      )
    },
    function(id) {
      tagList(
        div(
          class = "form-group",
          radioButtons(
            inputId = NS(id, "source"),
            label = "File source",
            choices = c("Upload files" = "upload", "Browse files" = "path"),
            selected = source,
            inline = TRUE
          )
        ),
        
        conditionalPanel(
          condition = sprintf("input['%s'] == 'upload'", NS(id, "source")),
          fileInput(
            inputId = NS(id, "file_upload"),
            label = "Select files",
            multiple = TRUE,
            accept = format_accept_extensions()
          )
        ),
        
        conditionalPanel(
          condition = sprintf("input['%s'] == 'path'", NS(id, "source")),
          shinyFiles::shinyFilesButton(
            NS(id, "file_browser"),
            label = "Browse files",
            title = "Select files to read",
            multiple = TRUE
          )
        ),
        
        div(
          class = "form-group",
          selectInput(
            inputId = NS(id, "combine"),
            label = "Multiple files strategy",
            choices = c(
              "Auto (rbind with fallback)" = "auto",
              "Row bind (rbind)" = "rbind", 
              "Column bind (cbind)" = "cbind",
              "First file only" = "first",
              "Error on multiple" = "error"
            ),
            selected = combine
          )
        ),
        
        div(
          class = "alert alert-info",
          style = "margin-top: 10px;",
          strong("Status: "), textOutput(NS(id, "file_info"), inline = TRUE),
          br(),
          textOutput(NS(id, "combine_info"), inline = TRUE)
        ),
        
        # Collapsible panel with supported formats
        tags$details(
          style = "margin-top: 10px; padding: 10px; background-color: #f8f9fa; border-radius: 4px;",
          tags$summary(
            style = "cursor: pointer; font-weight: bold; color: #0066cc;",
            "Supported File Formats ",
            tags$small("(click to expand)")
          ),
          tags$div(
            style = "margin-top: 10px;",
            tags$p("This block can read files in the following formats using rio's auto-detection:"),
            tags$ul(
              style = "columns: 2; -webkit-columns: 2; -moz-columns: 2;",
              lapply(names(rio_formats), function(category) {
                format_info <- rio_formats[[category]]
                tags$li(
                  tags$strong(format_info$description, ": "),
                  tags$code(paste(format_info$extensions, collapse = ", "))
                )
              })
            ),
            tags$p(
              tags$small(
                tags$em("Note: Additional formats may be available if optional packages are installed. ",
                        "Files are imported using rio::import() which auto-detects format based on extension.")
              )
            )
          )
        )
      )
    },
    class = "read_block",
    allow_empty_state = TRUE,
    ...
  )
}

# Helper function for null coalescing
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
