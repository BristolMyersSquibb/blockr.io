zchr_to_null <- function(x) {

  if (nzchar(x)) {
    return(x)
  }

  NULL
}

#' Check if object is a single string
#' @keywords internal
is_string <- function(x) {
  is.character(x) && length(x) == 1L
}

#' Get blockr option with fallback
#' @keywords internal
blockr_option <- function(name, default = NULL) {
  opt_name <- paste0("blockr.", name)
  getOption(opt_name, default)
}

#' Set names helper (base R doesn't export this in older versions)
#' @keywords internal
set_names <- function(x, nm) {
  if (length(x) == 0) {
    return(x)
  }
  names(x) <- nm
  x
}

#' Validate URL format
#' @keywords internal
is_valid_url <- function(url) {
  if (!is_string(url)) return(FALSE)
  if (!nzchar(url)) return(FALSE)
  grepl("^https?://[^\\s]+", url, perl = TRUE)
}

#' Download URL to temporary file
#'
#' Downloads a file from a URL to a temporary location. Extracts file extension
#' from URL if available to help with format detection.
#'
#' @param url Character. URL to download from.
#' @return Path to temporary file containing downloaded data.
#' @keywords internal
download_url_to_temp <- function(url) {
  stopifnot(is_string(url), nzchar(url))

  # Validate URL format
  if (!is_valid_url(url)) {
    stop("Invalid URL format: ", url, call. = FALSE)
  }

  # Extract extension from URL if possible
  # Handle query parameters by taking only the path part
  url_path <- strsplit(url, "?", fixed = TRUE)[[1]][1]
  ext <- tools::file_ext(basename(url_path))
  if (nzchar(ext)) {
    ext <- paste0(".", ext)
  } else {
    ext <- ""
  }

  # Create temp file with extension
  temp_file <- tempfile(fileext = ext)

  # Download with error handling
  tryCatch({
    download.file(url, temp_file, quiet = TRUE, mode = "wb")
    temp_file
  }, error = function(e) {
    stop("Failed to download from URL: ", url, "\n  ", e$message, call. = FALSE)
  })
}
