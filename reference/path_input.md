# Path input widget

A Shiny module that provides a text input with server-side
file/directory autocomplete. Used by the read and write blocks to
replace shinyFiles browser widgets.

## Usage

``` r
path_input_ui(id, prefix = NULL, upload_id = NULL)

path_input_server(id, data_dir = reactive(""), mode = c("file", "directory"))
```

## Arguments

- id:

  Module namespace ID.

- prefix:

  Optional initial prefix text shown before the input (typically the
  data directory path).

- upload_id:

  Optional ID of a hidden Shiny `fileInput` to wire up for upload-icon
  click and drag-and-drop. When non-NULL, an upload icon button is
  rendered inside the input field and the container gets a
  `data-upload-target` attribute pointing to this ID.

- data_dir:

  Reactive returning the current data directory path (from board
  options). Empty string means no data directory.

- mode:

  Either `"file"` or `"directory"`. Controls which entries are
  selectable in the autocomplete dropdown.

## Value

`path_input_ui()` returns a `tagList` with the widget HTML.
`path_input_server()` returns a `reactive` containing the current path
text value.
