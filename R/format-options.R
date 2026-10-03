# What is tunable about reading a format, declared by the entry that knows.
#
# The registry could already say HOW to read a format; this says WHAT can be
# set when reading it. Without it every block had to carry the knowledge
# itself -- "csv understands sep and skip, excel understands sheet and
# range, everything else understands nothing" -- as a hardcoded type check,
# in as many copies as there were blocks, and a format registered by another
# package could never grow a widget at all: its options existed but nothing
# on screen knew to offer them.
#
# A spec is small on purpose: enough to render one field, read it back, and
# tell whether it is still at its default. Blocks generate their settings
# band from the specs rather than hardcoding fields, so a format registered
# tomorrow gets its options offered everywhere with no block changing.

new_format_opt <- function(type, label, default, ...) {
  structure(
    c(list(type = type, label = label, default = default), list(...)),
    class = "blockr_format_opt"
  )
}

#' Declare a format's reader options
#'
#' The option kinds an entry may declare through [register_format()]'s
#' `options` argument. Each returns one spec: a label, a default, and enough
#' shape for a block to render a field and read it back.
#'
#' A value equal to the declared default is dropped when a block collects
#' its options, so a block nobody has touched carries no options at all and
#' serializes clean.
#'
#' @param label Field label, shown to the user.
#' @param choices Named character vector of choices (names are labels).
#' @param default The value the reader uses when the option is unset.
#' @param create For `opt_choice()`, whether the user may type a value that
#'   is not in `choices`.
#' @param placeholder Hint shown in an empty field. Defaults to naming the
#'   default value.
#' @return A `blockr_format_opt` object.
#' @seealso [register_format()], [format_options()]
#' @name format_opt
#' @export
opt_choice <- function(label, choices, default, create = FALSE) {
  new_format_opt(
    "choice", label, default, choices = choices, create = isTRUE(create)
  )
}

#' @rdname format_opt
#' @export
opt_text <- function(label, default = "", placeholder = NULL) {
  new_format_opt("text", label, default, placeholder = placeholder)
}

#' @rdname format_opt
#' @export
opt_number <- function(label, default = 0, placeholder = NULL) {
  new_format_opt("number", label, default, placeholder = placeholder)
}

#' @rdname format_opt
#' @export
opt_flag <- function(label, default = TRUE) {
  new_format_opt("flag", label, default)
}

# An entry's declaration may be a static named list, or a function of the
# location: a container computes its options from the members it holds, and
# cannot know them before it is pointed somewhere.
resolve_declared_options <- function(opts, path) {
  if (is.null(opts)) {
    return(list())
  }

  if (is.function(opts)) {
    opts <- tryCatch(opts(path), error = function(e) list())
  }

  if (!length(opts)) list() else opts
}

#' The options a format declares
#'
#' Looks up `x` (a path, or a bare extension) in the single-mode registry
#' and returns the entry's declared option specs, or an empty list when the
#' format declares none -- which is the answer for most formats, and the
#' reason a block should ask rather than assume.
#'
#' @param x Character. A file path or a bare extension.
#' @return Named list of [format_opt] specs, possibly empty.
#' @export
format_options <- function(x) {
  ext <- path_ext(x)

  if (!nzchar(ext)) {
    ext <- tolower(x)
  }

  entry <- lookup_entry("single", ext)

  if (is.null(entry)) {
    fb <- .registry$fallback
    if (!is.null(fb) && ext %in% fb$extensions) {
      entry <- fb
    }
  }

  if (is.null(entry)) {
    return(list())
  }

  resolve_declared_options(entry$options, x)
}

#' The options a source offers
#'
#' What can be set when reading `path` in the given mode: the format's own
#' options for a single file, and for a container the options its members
#' understand (a folder of CSVs offers the csv options; a folder of parquet
#' offers none). This is what a block gates its settings affordance on --
#' no options, no gear -- and what it generates the fields from.
#'
#' @param path Character. A resolved location.
#' @param mode `"single"` (one file, one data frame) or `"container"` (a
#'   location holding several tables).
#' @return Named list of [format_opt] specs, possibly empty.
#' @export
source_options <- function(path, mode = c("single", "container")) {
  mode <- match.arg(mode)

  if (!length(path) || !nzchar(path)) {
    return(list())
  }

  if (identical(mode, "single")) {
    return(format_options(path))
  }

  entry <- tryCatch(container_lookup(path), error = function(e) NULL)

  if (is.null(entry)) {
    return(list())
  }

  resolve_declared_options(entry$options, path)
}

#' The options a set of files understands
#'
#' The union of the members' declared options, which is what a container
#' offers: uniform member options are handed to every member, so an option
#' any member takes is settable and the members that do not take it ignore
#' it -- exactly what the emitted read expression does. Containers holding
#' files of other formats declare `options = function(path)` in terms of
#' this.
#'
#' @param paths Character vector of member paths (or names; only the
#'   extension is read).
#' @return Named list of [format_opt] specs, possibly empty.
#' @export
member_options <- function(paths) {
  out <- list()

  # First-seen order, over sorted members: an option keeps the position its
  # format declared it in (delimiter before header, as the field order in a
  # band should read), and the result does not depend on directory order.
  for (p in sort(unique(paths))) {
    specs <- format_options(p)

    for (nm in names(specs)) {
      if (is.null(out[[nm]])) {
        out[[nm]] <- specs[[nm]]
      }
    }
  }

  out
}

# Display value for a field: what the widget should show for this option
# given the block's current (deviations-only) option list. Typed as the
# control reports it: logical for a flag, numeric (NA for empty) for a
# number, character otherwise.
opt_display_value <- function(spec, value) {
  if (is.null(value)) {
    value <- spec$default
  }

  if (identical(spec$type, "flag")) {
    return(isTRUE(value))
  }

  if (identical(spec$type, "number")) {
    num <- suppressWarnings(as.numeric(value))
    if (!length(num) || is.na(num[[1L]]) || is.infinite(num[[1L]])) {
      return(NA_real_)
    }
    return(num[[1L]])
  }

  if (is.null(value) || identical(value, Inf) || !length(value)) {
    return("")
  }

  as.character(value)
}

opt_placeholder <- function(spec) {
  if (!is.null(spec$placeholder)) {
    return(spec$placeholder)
  }

  if (is.null(spec$default) || identical(spec$default, Inf)) {
    return("default: unset")
  }

  paste0("default: ", spec$default)
}

# The choices of a select, with the current value added when it is not one
# of them: a restored board can carry a value typed where typing was
# allowed (a delimiter), and a select cannot show what it does not list.
opt_choices <- function(spec, value) {
  choices <- spec$choices

  if (nzchar(value) && !value %in% as.character(unname(choices))) {
    choices <- c(choices, set_names(value, value))
  }

  choices
}

# One control per spec, each a field of the tray's grid.
format_option_fields <- function(specs, ns, values = list()) {
  lapply(names(specs), function(nm) {
    spec <- specs[[nm]]
    val <- opt_display_value(spec, values[[nm]])

    switch(
      spec$type,
      choice = blockr.ui::select_input(
        ns(nm), spec$label, choices = opt_choices(spec, val), selected = val
      ),
      flag = wide_checkbox(
        blockr.ui::checkbox_input(ns(nm), spec$label, value = val),
        spec$label
      ),
      number = blockr.ui::number_input(
        ns(nm), spec$label, value = val, placeholder = opt_placeholder(spec)
      ),
      blockr.ui::text_input(
        ns(nm), spec$label, value = val, placeholder = opt_placeholder(spec)
      )
    )
  })
}

# A checkbox takes one column of the grid, or two when its label does not
# fit in one (about 15 characters at 14px in a 130px column).
wide_checkbox <- function(field, label) {
  if (nchar(label) <= 15L) {
    return(field)
  }

  small <- names(field$attribs) == "class" &
    vapply(field$attribs, identical, logical(1), "blockr-settings__field--small")
  field$attribs <- field$attribs[!small]
  field
}

#' Fields for a set of declared options
#'
#' Renders one field per spec with the blockr.ui controls: a select for a
#' choice, a checkbox for a flag, and text and number fields that commit on
#' Enter or blur. The fields sit in the gear tray's grid. Values come from
#' a block's option list; anything absent shows its declared default.
#'
#' @param specs Named list of [format_opt] specs, e.g. from
#'   [source_options()].
#' @param ns Namespace function, [shiny::NS()] of the calling module's id.
#'   Field input ids are the spec names, namespaced.
#' @param values Named list of current option values (deviations only).
#' @return A [htmltools::tag], or `NULL` when there are no specs.
#' @export
format_options_ui <- function(specs, ns, values = list()) {
  if (!length(specs)) {
    return(NULL)
  }

  div(
    class = "blockr-settings__grid",
    format_option_fields(specs, ns, values)
  )
}

# A field's value, coerced to what the reader expects, or NULL when it is
# at its default (and so has nothing to say).
opt_value_from_input <- function(spec, raw) {
  if (is.null(raw)) {
    return(NULL)
  }

  if (identical(spec$type, "flag")) {
    val <- isTRUE(raw)
    return(if (identical(val, isTRUE(spec$default))) NULL else val)
  }

  if (identical(spec$type, "number")) {
    if (!length(raw) || is.na(raw[[1L]]) ||
          !nzchar(trimws(as.character(raw[[1L]])))) {
      return(NULL)
    }
    val <- suppressWarnings(as.numeric(raw))
    if (is.na(val) || identical(val, as.numeric(spec$default))) {
      return(NULL)
    }
    return(val)
  }

  if (!nzchar(as.character(raw))) {
    return(NULL)
  }

  if (identical(as.character(raw), as.character(spec$default %||% ""))) {
    return(NULL)
  }

  as.character(raw)
}

# What the server last pushed into each field, per session. The blockr.ui
# controls do not report a pushed value back as input, so after
# format_options_update() a field's input still holds the value from
# before the push. The record says which value the field shows until the
# user changes it.
pushed_values <- function(session) {
  if (is.null(session) || is.null(session$userData)) {
    return(NULL)
  }

  if (is.null(session$userData$blockr_io_pushed)) {
    session$userData$blockr_io_pushed <- new.env(parent = emptyenv())
  }

  session$userData$blockr_io_pushed
}

#' Read declared option fields back off the inputs
#'
#' The inverse of [format_options_ui()]: collects the fields into the named
#' list a block carries as its options, dropping every value still at its
#' declared default so an untouched block stays empty. A field the server
#' moved with [format_options_update()] counts with the value it was moved
#' to until the user changes it.
#'
#' @param input The module's `input`.
#' @param specs Named list of [format_opt] specs.
#' @param session The module's `session`, where the pushed values are
#'   recorded.
#' @return Named list of option values; empty when nothing deviates.
#' @export
format_options_values <- function(input, specs,
                                  session = getDefaultReactiveDomain()) {
  pushed <- pushed_values(session)
  out <- list()

  for (nm in names(specs)) {
    raw <- input[[nm]]

    if (!is.null(pushed)) {
      key <- session$ns(nm)
      rec <- pushed[[key]]
      if (!is.null(rec)) {
        if (identical(raw, rec$seen)) {
          raw <- rec$value
        } else {
          rm(list = key, envir = pushed)
        }
      }
    }

    val <- opt_value_from_input(specs[[nm]], raw)
    if (!is.null(val)) {
      out[[nm]] <- val
    }
  }

  out
}

#' Push option values back into declared option fields
#'
#' For an external write: moves the fields to match `values` without
#' re-rendering them (which would drop focus mid-edit). The fields do not
#' report the move back as input; [format_options_values()] accounts for it.
#'
#' @param session The module's `session`.
#' @param specs Named list of [format_opt] specs.
#' @param values Named list of current option values.
#' @return Invisible `NULL`.
#' @export
format_options_update <- function(session, specs, values = list()) {
  pushed <- pushed_values(session)

  for (nm in names(specs)) {
    spec <- specs[[nm]]
    val <- opt_display_value(spec, values[[nm]])

    switch(
      spec$type,
      choice = blockr.ui::update_select_input(
        nm, choices = opt_choices(spec, val), selected = val,
        session = session
      ),
      flag = blockr.ui::update_checkbox_input(nm, value = val, session = session),
      number = blockr.ui::update_number_input(nm, value = val, session = session),
      blockr.ui::update_text_input(nm, value = val, session = session)
    )

    if (!is.null(pushed)) {
      assign(
        session$ns(nm),
        list(value = val, seen = isolate(session$input[[nm]])),
        envir = pushed
      )
    }
  }

  invisible(NULL)
}
