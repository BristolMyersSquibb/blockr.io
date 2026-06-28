# File-access verification for path-based blocks

Read/write blocks (blockr.io `read`/`write`, blockr.dm
`dm-read`/`dm-write`) resolve a user-supplied path and then read from or
write to it. By default any resolvable path is allowed: `data_dir` is a
UX convenience, not a security boundary, and an absolute path or a `..`
traversal walks straight out of it. These helpers let a *deployment*
restrict which paths a running app may touch, without each block
re-implementing the check.

## Usage

``` r
resolve_and_check(path, mode = c("read", "write"))

within_dirs(...)
```

## Arguments

- path:

  A path the block is about to read from or write to. Resolved relative
  to `data_dir` by the calling block before it reaches here.

- mode:

  Either `"read"` or `"write"`, selecting which verifier option applies.

- ...:

  Allowed root folders. A path is accepted if it equals one of the roots
  or sits inside one of them.

## Value

`resolve_and_check()` returns the normalized path, or signals the
verifier's error. `within_dirs()` returns a verifier function suitable
for the options.

## Details

The deployment sets one or both of the options `blockr.verify_read_path`
/ `blockr.verify_write_path` to a function of a single resolved path. A
block about to use a path calls `resolve_and_check()`, which normalizes
the path (resolving `.`, `..` and symlinks) and then calls the
deployment's verifier on the *normalized* form. The verifier returns
normally to allow the path, or
[`stop()`](https://rdrr.io/r/base/stop.html)s to reject it; the block
surfaces the message and does not read/write. When the option is unset
there is no checking, so existing apps are unaffected.

Read and write are separate options so a deployment can make writing
stricter than reading (or scope only one). Point both options at the
same function to apply one rule to both.

`within_dirs()` builds the common verifier: a folder allowlist that
rejects anything outside the given roots (trailing-slash- and
symlink-safe). A deployment that needs more — per-extension rules,
custom messages — writes its own function instead.

Normalization happens here, once, rather than inside the deployment's
verifier, so the block checks exactly the path it then uses: a verifier
that normalized internally could approve one form while the block
touched another. The verifier therefore receives an already-resolved
path and only decides yes/no.

## Honest ceiling

This stops a user picking the wrong path in a read/write block.
Concretely, the check resolves `..`, `.`, `//` and symlinks (via
[`normalizePath()`](https://rdrr.io/r/base/normalizePath.html), then a
lexical pass that also collapses `..` in a not-yet-existing tail that
[`normalizePath()`](https://rdrr.io/r/base/normalizePath.html) leaves
alone on Linux) before testing the allowlist, so traversal and
symlink-escape (including the `symlink/..` cancellation trick) are
rejected. It does **not** defend against:

- **arbitrary R** in function/transform blocks
  ([`read.csv()`](https://rdrr.io/r/utils/read.table.html),
  [`system()`](https://rdrr.io/r/base/system.html), ...) — out of reach
  of any in-app path check;

- a **symlink swapped between check and read** (a TOCTOU race) —
  symlinks are resolved at check time, not re-validated at the moment of
  the read.

The hard boundary for both is a read-only container filesystem except
the study mount; this option is the friendly, granular layer on top.

## Examples

``` r
# In app.R: restrict reads to one study folder, writes to its output dir.
if (FALSE) { # \dontrun{
options(
  blockr.verify_read_path  = blockr.io::within_dirs("/data/study123"),
  blockr.verify_write_path = blockr.io::within_dirs("/data/study123/out")
)
} # }

# within_dirs() rejects paths outside its roots:
verify <- within_dirs(tempdir())
verify(file.path(tempdir(), "ok.csv"))            # allowed (returns NULL)
tryCatch(verify("/etc/passwd"), error = conditionMessage)
#> [1] "Path outside the allowed folders."
```
