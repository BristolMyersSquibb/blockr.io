# Directory-listing response for path autocomplete

Shared implementation behind the `registerDataObj` endpoints of
[`path_input_server()`](https://bristolmyerssquibb.github.io/blockr.io/reference/path_input.md)
and
[`new_data_dir_option()`](https://bristolmyerssquibb.github.io/blockr.io/reference/new_data_dir_option.md).
Resolves the typed value to a directory to list, applies the deployment
file-access policy (see
[file_policy](https://bristolmyerssquibb.github.io/blockr.io/reference/file_policy.md))
to that directory, and returns a JSON `httpResponse` with up to 50
entries plus the total match count.

## Usage

``` r
list_dir_response(
  path_val,
  dir_root = "",
  mode = "file",
  extensions = NULL,
  policy = "read"
)
```

## Arguments

- path_val:

  The raw text typed in the input.

- dir_root:

  Optional data directory against which relative input is resolved.
  Empty string means no data directory.

- mode:

  `"file"` lists directories plus files with allowed extensions;
  `"directory"` lists directories only.

- extensions:

  Optional file extension allowlist (without dots) for file mode; `NULL`
  means all rio-supported formats.

- policy:

  `"read"` or `"write"`, selecting which deployment verifier gates the
  listing.

## Value

A
[`shiny::httpResponse()`](https://rdrr.io/pkg/shiny/man/httpResponse.html)
with JSON body `{items, base, total}`.

## Details

Entries are ranked directories first, then prefix matches on the typed
fragment before substring matches, then alphabetically. All entries of a
directory are stat-ed with a single vectorized
[`file.info()`](https://rdrr.io/r/base/file.info.html) call.
