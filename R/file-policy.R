#' File-access verification for path-based blocks
#'
#' Read/write blocks (blockr.io `read`/`write`, blockr.dm `dm-read`/`dm-write`)
#' resolve a user-supplied path and then read from or write to it. By default
#' any resolvable path is allowed: `data_dir` is a UX convenience, not a
#' security boundary, and an absolute path or a `..` traversal walks straight
#' out of it. These helpers let a *deployment* restrict which paths a running
#' app may touch, without each block re-implementing the check.
#'
#' The deployment sets one or both of the options
#' `blockr.verify_read_path` / `blockr.verify_write_path` to a function of a
#' single resolved path. A block about to use a path calls
#' [resolve_and_check()], which normalizes the path (resolving `.`, `..` and
#' symlinks) and then calls the deployment's verifier on the *normalized* form.
#' The verifier returns normally to allow the path, or `stop()`s to reject it;
#' the block surfaces the message and does not read/write. When the option is
#' unset there is no checking, so existing apps are unaffected.
#'
#' Read and write are separate options so a deployment can make writing
#' stricter than reading (or scope only one). Point both options at the same
#' function to apply one rule to both.
#'
#' [within_dirs()] builds the common verifier: a folder allowlist that rejects
#' anything outside the given roots (trailing-slash- and symlink-safe). A
#' deployment that needs more — per-extension rules, custom messages — writes
#' its own function instead.
#'
#' Normalization happens here, once, rather than inside the deployment's
#' verifier, so the block checks exactly the path it then uses: a verifier that
#' normalized internally could approve one form while the block touched another.
#' The verifier therefore receives an already-resolved path and only decides
#' yes/no.
#'
#' @section Honest ceiling:
#' This stops a user picking the wrong path in a read/write block. Concretely,
#' the check resolves `..`, `.`, `//` and symlinks (via [normalizePath()], then
#' a lexical pass that also collapses `..` in a not-yet-existing tail that
#' `normalizePath()` leaves alone on Linux) before testing the allowlist, so
#' traversal and symlink-escape (including the `symlink/..` cancellation trick)
#' are rejected. It does **not** defend against:
#' * **arbitrary R** in function/transform blocks (`read.csv()`, `system()`, ...)
#'   — out of reach of any in-app path check;
#' * a **symlink swapped between check and read** (a TOCTOU race) — symlinks are
#'   resolved at check time, not re-validated at the moment of the read.
#'
#' The hard boundary for both is a read-only container filesystem except the
#' study mount; this option is the friendly, granular layer on top.
#'
#' @param path A path the block is about to read from or write to. Resolved
#'   relative to `data_dir` by the calling block before it reaches here.
#' @param mode Either `"read"` or `"write"`, selecting which verifier option
#'   applies.
#'
#' @return [resolve_and_check()] returns the normalized path, or signals the
#'   verifier's error. [within_dirs()] returns a verifier function suitable for
#'   the options.
#'
#' @examples
#' # In app.R: restrict reads to one study folder, writes to its output dir.
#' \dontrun{
#' options(
#'   blockr.verify_read_path  = blockr.io::within_dirs("/data/study123"),
#'   blockr.verify_write_path = blockr.io::within_dirs("/data/study123/out")
#' )
#' }
#'
#' # within_dirs() rejects paths outside its roots:
#' verify <- within_dirs(tempdir())
#' ok <- file.path(tempdir(), "ok.csv")
#' file.create(ok)                                   # normalizePath resolves it
#' verify(ok)                                        # allowed (returns NULL)
#' tryCatch(verify("/etc/passwd"), error = conditionMessage)
#'
#' @name file_policy
#' @export
resolve_and_check <- function(path, mode = c("read", "write")) {

  mode <- match.arg(mode)

  real <- normalize_for_policy(path)

  verify <- blockr_option(
    if (identical(mode, "read")) "verify_read_path" else "verify_write_path",
    default = NULL
  )

  if (is.function(verify)) {
    verify(real)
  }

  real
}

#' @rdname file_policy
#'
#' @param ... Allowed root folders. A path is accepted if it equals one of the
#'   roots or sits inside one of them.
#'
#' @export
within_dirs <- function(...) {

  roots <- normalize_for_policy(c(...))

  function(path) {
    real <- normalize_for_policy(path)
    ok <- any(
      startsWith(paste0(real, "/"), paste0(roots, "/")) | real == roots
    )
    if (!isTRUE(ok)) {
      stop("Path outside the allowed folders.", call. = FALSE)
    }
  }
}

#' Normalize a path for policy checks
#'
#' [base::normalizePath()] resolves symlinks and `..` only for the part of a
#' path that exists on disk; for a path (or path prefix) that does not exist —
#' common for write targets and for hand-crafted serialized boards — it leaves
#' `..` segments in place on Linux, which would let `study/../other` slip past a
#' `startsWith()` allowlist check. So after `normalizePath()` we also collapse
#' `.`/`..` lexically, independent of what exists on disk.
#'
#' @param path Character vector of paths.
#' @return Character vector of normalized, dot-collapsed paths.
#' @keywords internal
normalize_for_policy <- function(path) {
  real <- normalizePath(path, winslash = "/", mustWork = FALSE)
  vapply(real, collapse_dots, character(1), USE.NAMES = FALSE)
}

#' Lexically collapse `.` and `..` segments in a path
#' @keywords internal
collapse_dots <- function(path) {
  is_abs <- startsWith(path, "/")
  parts <- strsplit(path, "/", fixed = TRUE)[[1]]
  out <- character()
  for (seg in parts) {
    if (!nzchar(seg) || identical(seg, ".")) {
      next
    }
    if (identical(seg, "..")) {
      if (length(out) > 0L && !identical(out[length(out)], "..")) {
        out <- out[-length(out)]
      } else if (!is_abs) {
        out <- c(out, "..")
      }
      # absolute path: `..` at the root has nowhere to go, drop it
    } else {
      out <- c(out, seg)
    }
  }
  res <- paste(out, collapse = "/")
  if (is_abs) {
    paste0("/", res)
  } else {
    res
  }
}
