# Path input widget

A Shiny module that provides a text input with server-side
file/directory autocomplete. Used by the read and write blocks to
replace shinyFiles browser widgets.

## Usage

``` r
path_input_ui(
  id,
  prefix = NULL,
  upload_id = NULL,
  placeholder = NULL,
  required = FALSE
)

path_input_server(
  id,
  data_dir = reactive(""),
  mode = c("file", "directory"),
  extensions = NULL,
  policy = NULL
)
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

- placeholder:

  Placeholder text for the input. Defaults to an upload-aware file hint;
  pass e.g. "Enter directory path..." for directory-mode inputs.

- required:

  Whether the field must be filled. When `TRUE`, an empty field carries
  a soft amber "needs a value" cue (mirroring blockr.viz's
  required-empty mapping affordance) that clears once a value is
  entered.

- data_dir:

  Reactive returning the current data directory path (from board
  options). Empty string means no data directory.

- mode:

  Either `"file"` or `"directory"`. Controls which entries are
  selectable in the autocomplete dropdown.

- extensions:

  Optional character vector of file extensions (without dots) to show in
  autocomplete. Defaults to `NULL`, which shows all rio-supported
  formats. Use e.g. `"rtf"` to restrict to RTF files only.

- policy:

  Which deployment file-access verifier applies to the directory-listing
  endpoint: `"read"` or `"write"` (see
  [file_policy](https://bristolmyerssquibb.github.io/blockr.io/reference/file_policy.md)).
  Defaults to `"read"` for file mode and `"write"` for directory mode.

## Value

`path_input_ui()` returns a `tagList` with the widget HTML.
`path_input_server()` returns a `reactive` containing the committed path
text value.

## Commit model

Typing never commits: keystrokes only drive the autocomplete dropdown.
The reactive value returned by `path_input_server()` updates when the
user *commits* — by pressing Enter, leaving the field (blur), or
selecting a dropdown entry. While the typed text differs from the
committed value, the field shows an "Enter ↵" chip; committing collapses
it to a faded check mark. This follows the blockr design-system
text-commit convention (decided 2026-07-02) and keeps half-typed paths
from reaching the pipeline.
