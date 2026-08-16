# blockr.io's own container entries. A container produces a bare named list
# of data frames, so none of these needs anything beyond base R and packages
# blockr.io already touches; the entries that hold files of other formats
# (directory, zip) compose member reads through the registry at build time
# and stay ignorant of what those formats are.
#
# An .rds holding an actual dm object is deliberately NOT here: that is dm
# reading its own serialization, a native path in the dm read block -- which
# also keeps two packages from claiming the "rds" container key.

# Members are files whose extension has a single-mode reader. Table names
# mirror what blockr.dm derived from file names, so a board moved off the dm
# block's old discovery sees the same names.
member_table_names <- function(files) {
  make.names(tools::file_path_sans_ext(basename(files)), unique = TRUE)
}

format_bytes <- function(bytes) {
  if (is.na(bytes) || bytes < 0) {
    return("")
  }
  if (bytes < 1024) {
    return(paste0(bytes, " B"))
  }
  if (bytes < 1024 * 1024) {
    return(paste0(round(bytes / 1024, 1), " KB"))
  }
  paste0(round(bytes / (1024 * 1024), 1), " MB")
}

member_table_info <- function(files, bytes) {
  data.frame(
    name = member_table_names(files),
    label = trimws(paste(
      toupper(tools::file_ext(files)),
      vapply(bytes, format_bytes, character(1))
    )),
    stringsAsFactors = FALSE
  )
}

readable_pattern <- function() {
  paste0("\\.(", paste(file_extensions(), collapse = "|"), ")$")
}

select_members <- function(files, tables) {
  names(files) <- member_table_names(files)

  if (!is.null(tables)) {
    files <- files[names(files) %in% tables]
  }

  if (length(files) == 0) {
    stop("No readable files match the selection.", call. = FALSE)
  }

  files
}

# --- directory ---------------------------------------------------------------

directory_list_tables <- function(path) {
  files <- list.files(
    path,
    pattern = readable_pattern(),
    ignore.case = TRUE,
    full.names = TRUE
  )
  member_table_info(files, file.size(files))
}

directory_read_expr <- function(path, tables, ...) {
  files <- select_members(
    list.files(
      path,
      pattern = readable_pattern(),
      ignore.case = TRUE,
      full.names = TRUE
    ),
    tables
  )

  as.call(c(quote(list), lapply(files, format_read_expr)))
}

# --- zip ---------------------------------------------------------------------

# Member names come from the central directory: no extraction, so members and
# their formats are known at build time. Only the extraction prefix is a
# run-time tempdir, which the emitted expression creates itself.
zip_members <- function(path) {
  info <- utils::unzip(path, list = TRUE)
  info <- info[!endsWith(info$Name, "/"), , drop = FALSE]
  info[grepl(readable_pattern(), info$Name, ignore.case = TRUE), , drop = FALSE]
}

zip_list_tables <- function(path) {
  info <- zip_members(path)
  member_table_info(info$Name, info$Length)
}

zip_read_expr <- function(path, tables, ...) {
  members <- select_members(zip_members(path)$Name, tables)

  exprs <- lapply(members, function(m) {
    format_read_expr_impl(
      tolower(tools::file_ext(m)),
      bquote(file.path(tmp, .(m)))
    )
  })

  bquote(local({
    tmp <- tempfile("zip_")
    utils::unzip(.(path), exdir = tmp)
    .(as.call(c(quote(list), exprs)))
  }))
}

# --- excel sheets ------------------------------------------------------------

# Same extensions as the single-mode excel entry, different mode: one sheet
# as a data frame is single, all sheets as a named list is container.

sheets_list_tables <- function(path) {
  sheets <- readxl::excel_sheets(path)
  data.frame(
    name = make.names(sheets, unique = TRUE),
    label = rep("Sheet", length(sheets)),
    stringsAsFactors = FALSE
  )
}

sheets_read_expr <- function(path, tables, ...) {
  sheets <- readxl::excel_sheets(path)
  names(sheets) <- make.names(sheets, unique = TRUE)

  if (!is.null(tables)) {
    sheets <- sheets[names(sheets) %in% tables]
  }

  if (length(sheets) == 0) {
    stop("No sheets match the selection.", call. = FALSE)
  }

  exprs <- lapply(sheets, function(s) {
    bquote(readxl::read_excel(.(path), sheet = .(s)))
  })

  as.call(c(quote(list), exprs))
}

# --- rds / rdata lists -------------------------------------------------------

# The one place discovery is not cheap: the file is its own only index, so
# list_tables must read it. Accepted -- these are local files read once at
# configure time. The emitted expression loads once and subsets.

rds_check_tables <- function(obj, path) {
  if (
    !is.list(obj) ||
      is.data.frame(obj) ||
      is.null(names(obj)) ||
      !all(nzchar(names(obj))) ||
      !all(vapply(obj, is.data.frame, logical(1)))
  ) {
    stop(
      "\"", basename(path), "\" does not hold a named list of data frames ",
      "(found ", paste(class(obj), collapse = "/"), "). ",
      "Single tables are for the read block; other object types need a ",
      "block from the package that owns them.",
      call. = FALSE
    )
  }
  invisible(obj)
}

rds_tables <- function(path) {
  obj <- readRDS(path)
  rds_check_tables(obj, path)
  names(obj)
}

rds_list_tables <- function(path) {
  obj <- readRDS(path)
  rds_check_tables(obj, path)
  data.frame(
    name = names(obj),
    label = vapply(obj, function(d) paste0(nrow(d), " rows"), character(1)),
    stringsAsFactors = FALSE
  )
}

rds_read_expr <- function(path, tables, ...) {
  available <- rds_tables(path)
  selected <- if (is.null(tables)) available else intersect(available, tables)

  if (length(selected) == 0) {
    stop("No tables match the selection.", call. = FALSE)
  }

  bquote(readRDS(.(path))[.(selected)])
}

rdata_tables <- function(path) {
  env <- new.env(parent = emptyenv())
  load(path, envir = env)
  objs <- as.list(env)
  names(objs)[vapply(objs, is.data.frame, logical(1))]
}

rdata_list_tables <- function(path) {
  env <- new.env(parent = emptyenv())
  load(path, envir = env)
  objs <- as.list(env)
  objs <- objs[vapply(objs, is.data.frame, logical(1))]
  objs <- objs[sort(names(objs))]
  data.frame(
    name = names(objs),
    label = vapply(objs, function(d) paste0(nrow(d), " rows"), character(1)),
    stringsAsFactors = FALSE
  )
}

rdata_read_expr <- function(path, tables, ...) {
  available <- rdata_tables(path)
  selected <- if (is.null(tables)) {
    sort(available)
  } else {
    intersect(sort(available), tables)
  }

  if (length(selected) == 0) {
    stop("No tables match the selection.", call. = FALSE)
  }

  bquote(local({
    env <- new.env()
    load(.(path), envir = env)
    mget(.(selected), envir = env)
  }))
}

# --- registration ------------------------------------------------------------

# Called from .onLoad. The named single-mode entries are the bespoke builders
# expr.R always had; rio is the fallback for the long tail. Re-registration
# from this namespace replaces, so load_all() is safe.
register_io_formats <- function() {
  register_format(
    extensions = c("csv", "tsv", "txt", "dat", "tab"),
    read = read_expr_csv
  )
  register_format(
    extensions = c("xls", "xlsx", "xlsm", "xlsb"),
    read = read_expr_excel
  )
  register_format(
    extensions = c("parquet", "feather", "arrow"),
    read = read_expr_arrow
  )
  register_format_fallback(
    extensions = get_rio_extensions(),
    read = read_expr_rio
  )

  register_container(
    opens = "directory",
    list_tables = directory_list_tables,
    read = directory_read_expr
  )
  register_container(
    opens = "zip",
    list_tables = zip_list_tables,
    read = zip_read_expr
  )
  register_container(
    opens = c("xls", "xlsx"),
    list_tables = sheets_list_tables,
    read = sheets_read_expr
  )
  register_container(
    opens = "rds",
    list_tables = rds_list_tables,
    read = rds_read_expr
  )
  register_container(
    opens = c("rdata", "rda"),
    list_tables = rdata_list_tables,
    read = rdata_read_expr
  )

  invisible(NULL)
}
