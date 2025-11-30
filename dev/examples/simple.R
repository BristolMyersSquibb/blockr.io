devtools::load_all(".")

temp_csv <- tempfile(fileext = ".csv")
write.csv(mtcars, temp_csv, row.names = FALSE)
# serve(new_read_block(
#   path = temp_csv,
#   source = "path",
#   combine = "auto"
# ))

# serve(new_dataset_block(
#   dataset = "iris"
# ))

# Serve the read block in a board
serve(
  new_board(
    blocks = c(
      data = new_read_block(
        path = temp_csv,
        source = "path",
        combine = "auto"
      )
    )
  )
)
