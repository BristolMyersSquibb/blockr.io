devtools::load_all(".")

# Example 1: Single write block with data
# serve(
#   new_write_block(
#     mode = "download",
#     format = "excel",
#     filename = ""
#   ),
#   data = mtcars
# )

# Example 2: Read and write pipeline
temp_csv <- tempfile(fileext = ".csv")
write.csv(mtcars, temp_csv, row.names = FALSE)

serve(
  new_board(
    blocks = c(
      data = new_read_block(
        path = temp_csv,
        source = "path",
        combine = "auto"
      ),
      output = new_write_block(
        mode = "download",
        format = "excel",
        filename = ""
      )
    ),
    links = c(
      data_output = new_link("data", "output")
    )
  )
)
