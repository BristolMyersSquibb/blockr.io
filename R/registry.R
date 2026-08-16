# The file format registry. Entries are consulted when a block builds its
# read expression and never appear in the expression that is built: a
# registrant hands over a function that RETURNS an expression, and that
# expression may only call the reader package (rio::import(),
# readr::read_csv(), some.pkg::read_special()), never blockr.
#
# Two modes per key. "single" turns one location into a data frame;
# "container" turns a location holding several tables into a bare named list
# of data frames (no class -- dm-ness or anything else is added by the
# consuming block, outside this package). Keys are file extensions, plus the
# one key that is not: "directory".
#
# Registration never throws, so a collision cannot make library() fail.
# Colliding entries (same key and mode from two namespaces) are kept and the
# first lookup errors naming both packages; re-registration from the same
# namespace replaces, so devtools::load_all() does not collide with itself.

.registry <- new.env(parent = emptyenv())
.registry$single <- list()
.registry$container <- list()
.registry$fallback <- NULL

# The namespace a registration call came from, recorded for collision
# messages and for the consuming block's restore hint.
registrant_package <- function(env) {
  pkg <- utils::packageName(env)
  if (is.null(pkg)) "(global)" else pkg
}

register_entry <- function(mode, key, entry) {
  key <- tolower(key)
  tbl <- .registry[[mode]]
  existing <- tbl[[key]]

  if (is.null(existing)) {
    tbl[[key]] <- list(entry)
  } else {
    same <- vapply(
      existing,
      function(e) identical(e$package, entry$package),
      logical(1)
    )
    if (any(same)) {
      existing[[which(same)[1]]] <- entry
    } else {
      existing <- c(existing, list(entry))
    }
    tbl[[key]] <- existing
  }

  .registry[[mode]] <- tbl
  invisible(NULL)
}

# Test seam: drop one namespace's entry for a key, or the whole key.
unregister_entry <- function(mode, key, package = NULL) {
  key <- tolower(key)
  tbl <- .registry[[mode]]

  if (is.null(package)) {
    tbl[[key]] <- NULL
  } else {
    keep <- vapply(
      tbl[[key]],
      function(e) !identical(e$package, package),
      logical(1)
    )
    tbl[[key]] <- if (any(keep)) tbl[[key]][keep] else NULL
  }

  .registry[[mode]] <- tbl
  invisible(NULL)
}

# The single entry for a key, or NULL. A key held by more than one namespace
# errors here, at lookup: this is where a shadowed reader would otherwise go
# unnoticed.
lookup_entry <- function(mode, key) {
  entries <- .registry[[mode]][[tolower(key)]]

  if (is.null(entries)) {
    return(NULL)
  }

  if (length(entries) > 1) {
    pkgs <- vapply(entries, function(e) e$package, character(1))
    stop(
      "Conflicting ", mode, " registrations for \"", key, "\": packages ",
      paste(pkgs, collapse = " and "),
      " both claim it. One of them must stand down.",
      call. = FALSE
    )
  }

  entries[[1]]
}

#' Register a file format
#'
#' Teaches blockr.io a file format at load time. The format then works
#' everywhere blockr.io's format knowledge is consulted: the read block's
#' dispatch, [file_extensions()] (and with it the file browser filter and
#' upload accept lists), and any package that asks the registry rather than
#' carrying its own extension list.
#'
#' Call from your package's `.onLoad()`. The registering namespace is
#' recorded automatically for collision messages. Registration never throws;
#' two packages claiming the same extension and mode error at first lookup,
#' naming both.
#'
#' @param extensions Character vector of file extensions (without dots).
#' @param read `function(path, ...)` returning an unevaluated expression that
#'   reads `path` into a data frame. The expression may only call the reader
#'   package (e.g. `bquote(your.pkg::read_special(.(path)))`), never blockr.
#'   `...` carries format options forwarded by the calling block. `path` is a
#'   resolved location and may be a language object (a container splicing
#'   member reads against a run-time prefix), so splice it, do not inspect it.
#' @param options Named list of [format_opt] specs declaring what `read`
#'   accepts through `...`, or `NULL` for a format with nothing to tune.
#'   Blocks generate their settings fields from this and show no settings
#'   affordance at all when it is empty, so a format declaring options gets
#'   them offered everywhere without any block changing.
#' @return Invisible `NULL`, called for its side effect.
#' @seealso [register_container()], [format_read_expr()], [format_opt]
#' @export
register_format <- function(extensions, read, options = NULL) {
  stopifnot(is.character(extensions), length(extensions) > 0)
  stopifnot(is.function(read))

  pkg <- registrant_package(parent.frame())

  for (ext in extensions) {
    register_entry(
      "single",
      ext,
      list(
        extensions = tolower(extensions), read = read, options = options,
        package = pkg
      )
    )
  }

  invisible(NULL)
}

#' Register a container
#'
#' A container is a location that holds several tables: a directory, a zip,
#' a workbook read sheet by sheet. A container entry produces a bare named
#' list of data frames -- no class. Whatever the consuming block wants the
#' list to become (a dm, via `dm::new_dm()`) is its own wrap, made outside
#' this package.
#'
#' @param opens Character vector of file extensions (without dots), or the
#'   string `"directory"`. Directories are not a special case in the lookup,
#'   only a key that is not an extension.
#' @param list_tables `function(path)` returning the table names available at
#'   `path`, cheaply: discovery must not read table data where the format
#'   allows it (a zip's central directory, a workbook's sheet index). Formats
#'   whose only index is the data itself (an `.rds`) may read; they are local
#'   files read once at configure time. Return a character vector, or a data
#'   frame with a `name` column and an optional `label` column carrying a
#'   short display string per table (`"CSV 2.1 MB"`).
#' @param read `function(path, tables, ...)` returning one unevaluated
#'   expression that evaluates to a named list of data frames, restricted to
#'   `tables` (`NULL` means all). The expression may compose member reads via
#'   [format_read_expr()] at build time, and may only call reader packages.
#'   `...` carries uniform member options: an entry that holds files of other
#'   formats should forward it into each member's [format_read_expr()] call,
#'   where the member's entry picks the parameters it understands (so
#'   `sep = ";"` reaches every csv in a folder and everything else ignores
#'   it).
#' @param options What the container accepts through `read`'s `...`: a named
#'   list of [format_opt] specs, or a `function(path)` returning one, since a
#'   container that holds files of other formats cannot know its options
#'   until it is pointed somewhere ([member_options()] over its members is
#'   the usual answer). `NULL` for a container with nothing to tune.
#' @return Invisible `NULL`, called for its side effect.
#' @seealso [register_format()], [container_list_tables()],
#'   [container_read_expr()], [format_opt]
#' @export
register_container <- function(opens, list_tables, read, options = NULL) {
  stopifnot(is.character(opens), length(opens) > 0)
  stopifnot(is.function(list_tables), is.function(read))

  pkg <- registrant_package(parent.frame())

  for (key in opens) {
    register_entry(
      "container",
      key,
      list(
        opens = tolower(opens),
        list_tables = list_tables,
        read = read,
        options = options,
        package = pkg
      )
    )
  }

  invisible(NULL)
}

# rio's long tail, registered once with lowest precedence. A fallback is
# never a collision: a package registering "csv" shadows rio deliberately
# rather than by accident.
register_format_fallback <- function(extensions, read, options = NULL) {
  .registry$fallback <- list(
    extensions = tolower(extensions),
    read = read,
    options = options,
    package = registrant_package(parent.frame())
  )
  invisible(NULL)
}

# Dispatch on a bare extension, with `path` passed through to the entry
# untouched. This is the seam containers use: a zip member's extension is
# known at build time while its extracted path is a language object
# (file.path(tmp, "member.csv")).
format_read_expr_impl <- function(ext, path, ...) {
  entry <- lookup_entry("single", ext)

  if (is.null(entry)) {
    fb <- .registry$fallback
    if (!is.null(fb) && tolower(ext) %in% fb$extensions) {
      entry <- fb
    }
  }

  if (is.null(entry)) {
    stop(
      "No reader registered for extension \"", ext, "\". ",
      "A package that reads this format must register it with ",
      "blockr.io::register_format() before the expression is built.",
      call. = FALSE
    )
  }

  entry$read(path, ...)
}

# The entry a location resolves to in container mode: a directory beats any
# extension, then the extension in container mode. No fallback here.
container_lookup <- function(path) {
  key <- if (dir.exists(path)) "directory" else tolower(tools::file_ext(path))
  entry <- lookup_entry("container", key)

  if (is.null(entry)) {
    stop(
      "No container registered for \"", key, "\". ",
      "See blockr.io::container_kinds() for what is available.",
      call. = FALSE
    )
  }

  entry
}

#' Read expression for a single file
#'
#' Resolves `path`'s extension against the registry (named entries first,
#' then the rio fallback) and returns the entry's unevaluated read
#' expression. This is what the read block calls when it builds its
#' expression, and what a container's builder calls per member.
#'
#' @param path Character. A resolved file path -- never a user-typed one.
#' @param ... Format options forwarded to the entry (e.g. `sep`, `sheet`,
#'   `skip`).
#' @return A language object that, when evaluated, reads the file into a
#'   data frame.
#' @export
format_read_expr <- function(path, ...) {
  stopifnot(is_string(path), nzchar(path))
  format_read_expr_impl(tolower(tools::file_ext(path)), path, ...)
}

# An entry's list_tables may return a bare character vector or a data frame
# with a `name` column and an optional `label` column (a short secondary
# display string: "CSV 2.1 MB", "12 rows"). Both accessors normalize.
normalize_table_info <- function(x) {
  if (is.data.frame(x)) {
    stopifnot(!is.null(x[["name"]]))
    label <- x[["label"]]
    data.frame(
      name = as.character(x[["name"]]),
      label = if (is.null(label)) rep("", nrow(x)) else as.character(label),
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(
      name = as.character(x),
      label = rep("", length(x)),
      stringsAsFactors = FALSE
    )
  }
}

#' Discover the tables in a container
#'
#' Resolves `path` (a directory, or a file whose extension has a container
#' entry) and returns the table names its entry can see, without reading
#' table data where the format allows. [container_table_info()] returns the
#' same discovery with each table's display label (`"CSV 2.1 MB"`,
#' `"12 rows"`, `""` when the entry has none), for blocks that render a
#' picker.
#'
#' @param path Character. A resolved location.
#' @return For `container_list_tables()`, a character vector of table names.
#'   For `container_table_info()`, a data frame with columns `name` and
#'   `label`.
#' @export
container_list_tables <- function(path) {
  container_table_info(path)$name
}

#' @rdname container_list_tables
#' @export
container_table_info <- function(path) {
  stopifnot(is_string(path), nzchar(path))
  normalize_table_info(container_lookup(path)$list_tables(path))
}

#' Read expression for a container
#'
#' Resolves `path` against the container registry and returns one
#' unevaluated expression that evaluates to a bare named list of data
#' frames, restricted to `tables`.
#'
#' @param path Character. A resolved location.
#' @param tables Character vector of table names to read, or `NULL` for all.
#' @param ... Uniform member options, forwarded by the entry into each
#'   member's format entry, which picks the parameters it understands:
#'   `container_read_expr(dir, sep = ";")` reads every csv in the folder as
#'   semicolon-separated and leaves the other formats untouched.
#' @return A language object that, when evaluated, produces a named list of
#'   data frames.
#' @export
container_read_expr <- function(path, tables = NULL, ...) {
  stopifnot(is_string(path), nzchar(path))
  container_lookup(path)$read(path, tables, ...)
}

#' What containers are registered
#'
#' The keys the container registry currently holds (`"directory"`, `"zip"`,
#' ...). A block that offers container reading should gate its affordances on
#' this, so nothing offers an action that is guaranteed to fail.
#'
#' @return Character vector of container keys.
#' @export
container_kinds <- function() {
  names(.registry$container)
}

# The providing namespace for a location, recorded by consuming blocks in
# their state so a restore with the registrant missing can name the package
# rather than surface a path error.
container_provider <- function(path) {
  container_lookup(path)$package
}

format_provider <- function(path) {
  ext <- tolower(tools::file_ext(path))
  entry <- lookup_entry("single", ext)

  if (is.null(entry)) {
    fb <- .registry$fallback
    if (!is.null(fb) && ext %in% fb$extensions) {
      entry <- fb
    }
  }

  if (is.null(entry)) NULL else entry$package
}

# Extensions readable in single mode: named entries plus the fallback's
# claims. This is what file_extensions() returns, so the file browser filter
# and the upload accept lists grow exactly when the registry does.
registered_extensions <- function() {
  named <- names(.registry$single)
  fb <- .registry$fallback

  unique(c(if (is.null(fb)) character() else fb$extensions, named))
}
