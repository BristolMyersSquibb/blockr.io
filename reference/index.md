# Package index

## File I/O Blocks

Blocks for reading and writing various file formats

- [`new_read_block()`](https://bristolmyerssquibb.github.io/blockr.io/reference/read.md)
  : Unified file reading block
- [`new_write_block()`](https://bristolmyerssquibb.github.io/blockr.io/reference/write.md)
  : Unified file writing block
- [`new_download_block()`](https://bristolmyerssquibb.github.io/blockr.io/reference/download.md)
  : Download-only file export block

## Format Registry

Teach blockr.io a file format or a container – a location holding
several tables – and build read expressions from what is registered.

- [`register_format()`](https://bristolmyerssquibb.github.io/blockr.io/reference/register_format.md)
  : Register a file format
- [`register_container()`](https://bristolmyerssquibb.github.io/blockr.io/reference/register_container.md)
  : Register a container
- [`format_read_expr()`](https://bristolmyerssquibb.github.io/blockr.io/reference/format_read_expr.md)
  : Read expression for a single file
- [`container_read_expr()`](https://bristolmyerssquibb.github.io/blockr.io/reference/container_read_expr.md)
  : Read expression for a container
- [`container_kinds()`](https://bristolmyerssquibb.github.io/blockr.io/reference/container_kinds.md)
  : What containers are registered
- [`container_list_tables()`](https://bristolmyerssquibb.github.io/blockr.io/reference/container_list_tables.md)
  [`container_table_info()`](https://bristolmyerssquibb.github.io/blockr.io/reference/container_list_tables.md)
  : Discover the tables in a container

## Declared Options

An entry declares what it accepts, and blocks generate their settings
fields from the answer.

- [`opt_choice()`](https://bristolmyerssquibb.github.io/blockr.io/reference/format_opt.md)
  [`opt_text()`](https://bristolmyerssquibb.github.io/blockr.io/reference/format_opt.md)
  [`opt_number()`](https://bristolmyerssquibb.github.io/blockr.io/reference/format_opt.md)
  [`opt_flag()`](https://bristolmyerssquibb.github.io/blockr.io/reference/format_opt.md)
  : Declare a format's reader options
- [`format_options()`](https://bristolmyerssquibb.github.io/blockr.io/reference/format_options.md)
  : The options a format declares
- [`source_options()`](https://bristolmyerssquibb.github.io/blockr.io/reference/source_options.md)
  : The options a source offers
- [`member_options()`](https://bristolmyerssquibb.github.io/blockr.io/reference/member_options.md)
  : The options a set of files understands
- [`format_options_ui()`](https://bristolmyerssquibb.github.io/blockr.io/reference/format_options_ui.md)
  : Fields for a set of declared options
- [`format_options_values()`](https://bristolmyerssquibb.github.io/blockr.io/reference/format_options_values.md)
  : Read declared option fields back off the inputs
- [`format_options_update()`](https://bristolmyerssquibb.github.io/blockr.io/reference/format_options_update.md)
  : Push option values back into declared option fields

## Utilities

Helper functions for file handling and configuration

- [`gear_band_ui()`](https://bristolmyerssquibb.github.io/blockr.io/reference/gear_band_ui.md)
  : Gear button and settings band
- [`file_category()`](https://bristolmyerssquibb.github.io/blockr.io/reference/file_category.md)
  : File category from extension
- [`file_extensions()`](https://bristolmyerssquibb.github.io/blockr.io/reference/file_extensions.md)
  : Supported file extensions
- [`resolve_and_check()`](https://bristolmyerssquibb.github.io/blockr.io/reference/file_policy.md)
  [`within_dirs()`](https://bristolmyerssquibb.github.io/blockr.io/reference/file_policy.md)
  : File-access verification for path-based blocks
- [`format_extension()`](https://bristolmyerssquibb.github.io/blockr.io/reference/format_extension.md)
  : File extension for a write format
- [`new_data_dir_option()`](https://bristolmyerssquibb.github.io/blockr.io/reference/new_data_dir_option.md)
  : Data directory board option
- [`path_input_ui()`](https://bristolmyerssquibb.github.io/blockr.io/reference/path_input.md)
  [`path_input_server()`](https://bristolmyerssquibb.github.io/blockr.io/reference/path_input.md)
  : Path input widget
- [`read_file_expr()`](https://bristolmyerssquibb.github.io/blockr.io/reference/read_file_expr.md)
  : Create a read expression for a single file
- [`resolve_data_dir()`](https://bristolmyerssquibb.github.io/blockr.io/reference/resolve_data_dir.md)
  : Resolve paths against the board's data directory
- [`write_file_expr()`](https://bristolmyerssquibb.github.io/blockr.io/reference/write_file_expr.md)
  : Create a write expression for a single file
- [`write_formats()`](https://bristolmyerssquibb.github.io/blockr.io/reference/write_formats.md)
  : Supported write formats
