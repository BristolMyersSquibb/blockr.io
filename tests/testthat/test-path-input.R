# Tests for path input widget

test_that("path_input_dep returns htmlDependency object", {
  dep <- blockr.io:::path_input_dep()

  expect_s3_class(dep, "html_dependency")
  expect_equal(dep$name, "blockr-path-input")
  expect_equal(dep$version, "0.1.0")
})

test_that("path_input_ui returns a tagList", {
  ui <- path_input_ui("test_id")

  # Should be a tag list
  expect_true(inherits(ui, "shiny.tag.list"))
})

test_that("path_input_server returns input value", {
  shiny::testServer(
    path_input_server,
    args = list(
      data_dir = reactive(""),
      mode = "file"
    ),
    {
      # Initially empty
      result <- session$returned()
      expect_equal(result, "")

      # Set path value
      session$setInputs(path_text = "data.csv")
      session$flushReact()

      result <- session$returned()
      expect_equal(result, "data.csv")
    }
  )
})
