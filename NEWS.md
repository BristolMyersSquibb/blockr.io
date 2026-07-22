# blockr.io (development version)

## Internal

- Block argument specs now use blockr.core's renamed `new_arg_specs()` /
  `new_arg_spec()` (blockr.core #295); the old `new_block_args()` /
  `new_block_arg()` names are deprecated upstream and were raising a
  build-time warning.

## Bug fixes

- `write_block`'s exported/eval'd expression now sets `expr_type = "bquoted"`
  and marks its input slot with the `.()` marker, so blockr.core substitutes
  the actual input name on eval and export. Exported code now reads e.g.
  `readr::write_csv(x = sales, ...)` directly instead of being wrapped in
  `with(list(.arg1 = sales), ...)`. The imperative save/download handlers
  keep bare symbols, since their slot values are already bound in the eval
  env.

- `write_expr()` now fills empty (positional) slot display names with
  `data_1`, `data_2`, ... instead of using them verbatim. Variadic links
  added positionally (the blockr.ui / blockr.dock link menus) arrive with
  `""` names, which produced colliding `.csv` entries inside multi-input
  ZIP downloads (each write clobbering the last) and empty Excel sheet
  names.

## Design-system alignment

- The gear button on the read, write, and download blocks now expands a
  full-width, in-flow settings band (the blockr design-system pattern,
  shared with blockr.viz and blockr.ggplot) instead of a floating popover.
  The band is a persistent panel: it stays open while you work and only
  the gear closes it. The settings-band CSS/JS is vendored verbatim from
  blockr.viz (the canonical source).
- All block styling moved out of R-generated `<style>` tags into
  `inst/assets/css/io-blocks.css` and is tokenized against the design
  system: colors, radii, control heights, and transitions are
  `var(--blockr-*, <canonical fallback>)`, so blocks pick up host themes
  while rendering canonically standalone.
- Checkboxes are restyled to the design-system `.blockr-checkbox` recipe
  (16px box, primary fill, inline SVG check) while keeping their Shiny
  bindings; one recipe covers both Bootstrap 3 and Bootstrap 5 markup.
- The required-but-empty amber cue on path inputs uses the canonical
  `.blockr-field--required-empty` class; the old
  `.blockr-path-required-empty` name remains as a CSS alias for one
  release.
- In-block `h4` titles are gone: the block name is the title; section
  headings render as micro-labels (`.io-section-label` /
  `.blockr-settings__title`).

## Path input: commit on Enter

- Path inputs now follow the blockr design-system text-commit convention:
  typing only drives the autocomplete dropdown; the value is committed (and
  the file read / directory applied) on Enter, blur, or a dropdown
  selection. While the typed text is not yet applied, the field shows an
  "Enter" chip that collapses to a faded check mark on commit. This removes
  per-keystroke pipeline recomputes (the main source of autocomplete lag)
  and mid-typing file reads.
- Autocomplete responses are sequence-guarded, so a slow response for an
  older query can no longer overwrite the dropdown for a newer one.
- Directory listings use a single vectorized `file.info()` call, rank
  prefix matches before substring matches (directories first), and show a
  "Showing 50 of N" footer when the listing is capped.
- The listing endpoint now honors the deployment file-access policy
  (`blockr.verify_read_path` / `blockr.verify_write_path`): directories
  outside the allowed roots are no longer enumerable via autocomplete.
- Open dropdowns follow their input on scroll/resize, keyboard navigation
  keeps the active item in view, and dropdowns of removed blocks are
  cleaned up.
- `path_input_ui()` gains a `placeholder` argument; `path_input_server()`
  gains a `policy` argument. `new_data_dir_option()` shares the listing
  implementation instead of duplicating it.

## Write block

- The output directory is created at write time (inside the generated
  write expression) instead of while the path is typed — typing a path no
  longer creates partial directories. The path badge shows
  "New directory (created on save)" for not-yet-existing targets.
- Manual mode is one-shot: each "Save to Server" click writes exactly
  once, imperatively. Upstream data changes no longer silently rewrite the
  file. The block expression is a pure passthrough of the first input
  except in auto-write mode, where it carries the write so exported code
  reproduces it.
- The reported "Saved to ..." path is now always the written path (single
  filename computation; previously two `Sys.time()` calls could disagree).
- Fixed the "File Configuration" header rendering beside (instead of above)
  the filename/format fields in wide panels: the header wrapper is a grid
  item under the responsive form grid and now explicitly spans the full row.
- Auto-write with an empty filename writes a fixed `data.{ext}` file,
  overwritten on each change, instead of creating a new timestamped file
  per upstream change. Manual saves and downloads keep timestamped names.

# blockr.io 0.1.0

Initial CRAN release.

## Features

### Data Import Block
- `new_read_block()`: Read data from various file formats
  - CSV, TSV, and delimited files (via readr)
  - Excel files (via readxl)
  - Arrow formats: Parquet, Feather, IPC (via arrow)
  - Other formats via rio package
  - URL support for remote files
  - Multi-file selection with combine options (rbind, cbind)
  - Advanced CSV options (delimiter, encoding, skip rows, etc.)

### Data Export Block
- `new_write_block()`: Write data to various file formats
  - CSV export (via readr)
  - Excel export with multiple sheets (via writexl)
  - Parquet and Feather export (via arrow)
  - Directory browser for output location
  - Auto-timestamped filenames
  - Multiple dataset export (zipped for CSV/Arrow)

## Documentation
- Full documentation for all exported functions
- Package website with examples
