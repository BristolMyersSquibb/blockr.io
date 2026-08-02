# External control of the read block. The block's expression has to be a
# function of the state it exposes -- anything else means the block can report
# and serialize one path while reading another.

local_csv <- function(dir, name, x) {
  f <- file.path(dir, name)
  utils::write.csv(data.frame(x = x), f, row.names = FALSE)
  f
}

test_that("path, source, combine and args are externally controllable", {
  expect_setequal(
    blockr.core::external_ctrl_vars(new_read_block()),
    c("path", "source", "combine", "args", "block_name")
  )
})

test_that("writing path re-reads: the external control path", {

  dir <- withr::local_tempdir()
  a <- local_csv(dir, "a.csv", 1:2)
  b <- local_csv(dir, "b.csv", 9:10)

  block <- new_read_block(path = a)

  shiny::testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      expect_identical(session$returned$result()$x, c(1, 2))

      # What `apply_block_mod_delta()` does on a board update: write the
      # state reactiveVal of the live block, no reconstruction.
      session$returned$state$path(b)
      session$flushReact()

      expect_identical(session$returned$result()$x, c(9, 10))
      expect_identical(unname(session$returned$state$path()), b)
    },
    args = list(x = block, data = list())
  )
})

test_that("writing args changes how the file is read", {

  dir <- withr::local_tempdir()
  f <- local_csv(dir, "a.csv", 1:5)

  block <- new_read_block(path = f)

  shiny::testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      expect_identical(nrow(session$returned$result()), 5L)

      session$returned$state$args(list(n_max = 2))
      session$flushReact()

      expect_identical(nrow(session$returned$result()), 2L)
    },
    args = list(x = block, data = list())
  )
})

test_that("a board update points the block at another file", {

  dir <- withr::local_tempdir()
  a <- local_csv(dir, "a.csv", 1:2)
  b <- local_csv(dir, "b.csv", 9:10)

  board <- blockr.core::new_board(blocks = c(rd = new_read_block(path = a)))

  expect_silent(
    blockr.core::validate_board_update(
      list(blocks = list(mod = list(rd = list(path = b)))),
      board
    )
  )

  expect_error(
    blockr.core::validate_board_update(
      list(blocks = list(mod = list(rd = list(nonsense = 1)))),
      board
    ),
    class = "board_update_blocks_mod_not_ctrl"
  )

  shiny::testServer(
    blockr.core:::get_s3_method("board_server", board),
    {
      session$flushReact()

      pre_server <- rv$blocks$rd$server
      expect_identical(rv$blocks$rd$server$result()$x, c(1, 2))

      board_update(list(blocks = list(mod = list(rd = list(path = b)))))
      session$flushReact()

      # Same server object: the block was controlled, not replaced.
      expect_identical(rv$blocks$rd$server, pre_server)
      expect_identical(rv$blocks$rd$server$result()$x, c(9, 10))
      expect_identical(unname(rv$blocks$rd$server$state$path()), b)
    },
    args = list(x = board)
  )
})

test_that("a path that is not there is reported, not fatal", {

  # A board can be restored before its data lands, and a controller can point
  # the block somewhere this session cannot see. Failing in the constructor
  # would take the whole board down with it; the block reports instead.
  missing <- file.path(withr::local_tempdir(), "not-there.csv")

  expect_no_error(block <- new_read_block(path = missing))

  shiny::testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      expect_null(session$returned$result())
      expect_identical(unname(session$returned$state$path()), missing)
    },
    args = list(x = block, data = list())
  )
})

test_that("the policy still rejects a path outside the allowed roots", {

  # Derived rather than stamped in by the input observer, so an externally
  # set path is policed on exactly the same terms as a typed one.
  # Outside tempdir(): the block exempts its own sandboxes (tempdir /
  # upload_path) so uploads keep working, so a policed path has to live
  # elsewhere. dirname(tempdir()) is not under tempdir().
  base <- file.path(dirname(tempdir()), "blockr_io_ctrl_policy")
  allowed <- file.path(base, "allowed")
  dir.create(allowed, recursive = TRUE)
  on.exit(unlink(base, recursive = TRUE), add = TRUE)
  inside <- local_csv(allowed, "a.csv", 1:2)
  outside <- local_csv(base, "b.csv", 9:10)

  withr::local_options(blockr.verify_read_path = within_dirs(allowed))

  block <- new_read_block(path = inside)

  shiny::testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      expect_identical(session$returned$result()$x, c(1, 2))

      session$returned$state$path(outside)
      session$flushReact()

      expect_match(
        rlang::expr_text(session$returned$expr()), "allowed folders"
      )
      expect_null(session$returned$result())
    },
    args = list(x = block, data = list())
  )
})

test_that("the path field's bind-time echo is not a user decision", {

  # A field reports its value the moment it binds, and inside a dock that
  # happens after the block has written its own path into it. Taken at face
  # value that echo relabels an uploaded file as a plain path, so a board
  # re-saved after someone merely looked at the block claims a provenance it
  # does not have.
  dir <- withr::local_tempdir()
  f <- local_csv(dir, "20260802_120000_report.csv", 1:2)

  block <- new_read_block(path = f, source = "upload")

  shiny::testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()

      session$setInputs(`expr-file_path-path_text` = f)
      session$flushReact()

      expect_identical(session$returned$state$source(), "upload")
      expect_identical(unname(session$returned$state$path()), f)
    },
    args = list(x = block, data = list())
  )
})
