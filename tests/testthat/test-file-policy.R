# Tests for file-access verification helpers

test_that("within_dirs allows paths inside the roots", {
  root <- normalize_for_policy(tempdir())
  verify <- within_dirs(root)

  expect_null(verify(file.path(root, "study", "adsl.csv")))
  expect_null(verify(root))
})

test_that("within_dirs rejects paths outside the roots", {
  root <- normalize_for_policy(tempdir())
  verify <- within_dirs(root)

  expect_error(verify("/etc/passwd"), "allowed folders")
})

test_that("within_dirs does not treat a prefix sibling as inside", {
  verify <- within_dirs("/data/study1")

  expect_error(verify("/data/study12/x.csv"), "allowed folders")
  expect_null(verify("/data/study1/x.csv"))
  expect_null(verify("/data/study1/sub/x.csv"))
})

test_that("within_dirs accepts multiple roots", {
  verify <- within_dirs("/data/study1", "/data/shared")

  expect_null(verify("/data/study1/a.csv"))
  expect_null(verify("/data/shared/ref.csv"))
  expect_error(verify("/data/other/x.csv"), "allowed folders")
})

test_that("within_dirs blocks .. traversal even for non-existent paths", {
  # normalizePath() alone does not collapse `..` for paths that do not exist
  # on disk; collapse_dots() must catch this.
  verify <- within_dirs("/data/study1")

  expect_error(verify("/data/study1/../other/x.csv"), "allowed folders")
  expect_error(verify("/data/study1/../../etc/passwd"), "allowed folders")
})

test_that("collapse_dots resolves . and .. lexically", {
  expect_identical(collapse_dots("/data/study1/../other"), "/data/other")
  expect_identical(collapse_dots("/data/./study1/x"), "/data/study1/x")
  expect_identical(collapse_dots("/a/b/c/../../d"), "/a/d")
  expect_identical(collapse_dots("/.."), "/")
  expect_identical(collapse_dots("rel/../x"), "x")
  expect_identical(collapse_dots("../x"), "../x")
})

# --- Adversarial: a real allowlist must survive these on a Linux filesystem ---

test_that("within_dirs normalizes . // and trailing slash without leaking", {
  verify <- within_dirs("/data/study1")

  # All of these are still inside the root -> allowed.
  expect_null(verify("/data/study1/"))
  expect_null(verify("/data/study1/./x.csv"))
  expect_null(verify("/data/study1//sub//x.csv"))
  expect_null(verify("/data/study1"))

  # The root's parent and the filesystem root are outside.
  expect_error(verify("/data"), "allowed folders")
  expect_error(verify("/"), "allowed folders")
})

test_that("within_dirs resolves symlinks before the boundary check", {
  skip_on_os("windows")

  base <- file.path(dirname(tempdir()), "blockr_io_symlink_test")
  unlink(base, recursive = TRUE)
  allowed <- file.path(base, "study")
  secret <- file.path(base, "secret")
  dir.create(allowed, recursive = TRUE)
  dir.create(secret, recursive = TRUE)
  writeLines("ok", file.path(allowed, "ok.txt"))
  writeLines("TOPSECRET", file.path(secret, "s.txt"))
  on.exit(unlink(base, recursive = TRUE))

  verify <- within_dirs(allowed)

  # A symlink *inside* the allowed dir pointing at the sibling secret dir must
  # not grant access to the secret: normalizePath resolves it to the real
  # target, which is outside the root.
  file.symlink(secret, file.path(allowed, "link_to_secret"))
  expect_error(
    verify(file.path(allowed, "link_to_secret", "s.txt")),
    "allowed folders"
  )

  # The symlink/.. cancellation trick (link -> dir, then `..` to escape) is
  # caught because the symlink is resolved *before* `..` is collapsed.
  file.symlink(allowed, file.path(base, "link_to_allowed"))
  expect_error(
    verify(file.path(base, "link_to_allowed", "..", "secret", "s.txt")),
    "allowed folders"
  )

  # A symlink to an absolute system path is likewise resolved and rejected.
  file.symlink("/etc", file.path(allowed, "etc_link"))
  expect_error(verify(file.path(allowed, "etc_link", "passwd")), "allowed folders")

  # Sanity: a genuine file inside the root still passes.
  expect_null(verify(file.path(allowed, "ok.txt")))
})

test_that("within_dirs handles ~ consistently with how blocks read", {
  skip_on_os("windows")

  # normalizePath() expands ~ (via path.expand), the same expansion file
  # readers do, so the policy checks the path that is actually read.
  home <- normalizePath("~", winslash = "/", mustWork = FALSE)
  verify <- within_dirs(file.path(home, "study"))

  expect_null(verify("~/study/x.csv"))
  expect_error(verify("~/secret.csv"), "allowed folders")
})

test_that("resolve_and_check is a no-op when no verifier is set", {
  old <- options(blockr.verify_read_path = NULL, blockr.verify_write_path = NULL)
  on.exit(options(old))

  expect_identical(
    resolve_and_check("/any/path", "read"),
    normalize_for_policy("/any/path")
  )
})

test_that("resolve_and_check enforces read and write verifiers independently", {
  old <- options(
    blockr.verify_read_path = within_dirs("/data/study1"),
    blockr.verify_write_path = NULL
  )
  on.exit(options(old))

  expect_error(resolve_and_check("/etc/x", "read"), "allowed folders")
  expect_silent(resolve_and_check("/data/study1/a.csv", "read"))

  # write verifier unset -> writes unrestricted even though read is scoped
  expect_silent(resolve_and_check("/etc/x", "write"))
})

test_that("resolve_and_check normalizes before checking (traversal blocked)", {
  old <- options(blockr.verify_read_path = within_dirs("/data/study1"))
  on.exit(options(old))

  expect_error(
    resolve_and_check("/data/study1/../other/x.csv", "read"),
    "allowed folders"
  )
})
