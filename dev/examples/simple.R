library(blockr.ui)
library(blockr.core)
devtools::load_all(".")

temp_csv <- tempfile(fileext = ".csv")
write.csv(mtcars, temp_csv, row.names = FALSE)
# blockr.core::serve(new_read_block(
#   path = temp_csv,
#   source = "path",
#   combine = "auto"
# ))

# blockr.core::serve(new_dataset_block(
#   dataset = "iris"
# ))

# Serve the read block in a DAG board
blockr.core::serve(
  blockr.ui::new_dag_board(
    blocks = c(
      data = new_read_block(
        path = temp_csv,
        source = "path",
        combine = "auto"
      )
    )
  )
)
