# The registry is consulted at build time and never appears in what it
# builds: every expression asserted here calls reader packages only.

test_that("named entries dispatch by extension", {
  expect_match(
    deparse1(format_read_expr("/data/a.csv")),
    "readr::read_csv"
  )
  expect_match(
    deparse1(format_read_expr("/data/a.xlsx")),
    "readxl::read_excel"
  )
  expect_match(
    deparse1(format_read_expr("/data/a.parquet")),
    "read_parquet"
  )
})

test_that("the fallback serves the long tail and named entries beat it", {
  expect_match(
    deparse1(format_read_expr("/data/a.sav")),
    "rio::import"
  )
  # csv is claimed by both; the named entry wins
  expect_no_match(
    deparse1(format_read_expr("/data/a.csv")),
    "rio::import"
  )
})

test_that("an unregistered extension errors naming it", {
  expect_error(
    format_read_expr("/data/a.xyz"),
    "No reader registered for extension \"xyz\""
  )
})

test_that("a registered format joins dispatch and file_extensions()", {
  withr::defer(blockr.io:::unregister_entry("single", "rtf"))

  expect_false("rtf" %in% file_extensions())

  register_format(
    extensions = "rtf",
    read = function(path, ...) bquote(some.pkg::read_rtf(.(path)))
  )

  expect_true("rtf" %in% file_extensions())
  expect_identical(
    format_read_expr("/data/tab.rtf"),
    quote(some.pkg::read_rtf("/data/tab.rtf"))
  )
})

test_that("same-namespace re-registration replaces, foreign collides at lookup", {
  withr::defer(blockr.io:::unregister_entry("single", "spx"))

  entry <- function(pkg, marker) {
    list(
      extensions = "spx",
      read = function(path, ...) bquote((.(marker))(.(path))),
      package = pkg
    )
  }

  blockr.io:::register_entry("single", "spx", entry("pkg.a", quote(a_reader)))
  blockr.io:::register_entry("single", "spx", entry("pkg.a", quote(a_reader2)))
  expect_match(deparse1(format_read_expr("x.spx")), "a_reader2")

  blockr.io:::register_entry("single", "spx", entry("pkg.b", quote(b_reader)))
  expect_error(format_read_expr("x.spx"), "pkg.a and pkg.b")
})

test_that("registration itself never throws on a collision", {
  withr::defer(blockr.io:::unregister_entry("single", "clsn"))

  blockr.io:::register_entry(
    "single", "clsn",
    list(extensions = "clsn", read = identity, package = "pkg.a")
  )
  expect_no_error(
    blockr.io:::register_entry(
      "single", "clsn",
      list(extensions = "clsn", read = identity, package = "pkg.b")
    )
  )
})

test_that("blockr.io ships its own container kinds", {
  expect_contains(
    container_kinds(),
    c("directory", "zip", "xls", "xlsx", "rds", "rdata", "rda")
  )
})

test_that("a directory is a container: explicit member reads, bare list", {
  dir <- withr::local_tempdir()
  write.csv(data.frame(x = 1:3), file.path(dir, "adsl.csv"), row.names = FALSE)
  write.csv(data.frame(y = 4:6), file.path(dir, "ae.csv"), row.names = FALSE)
  writeLines("not data", file.path(dir, "readme.md"))

  expect_identical(container_list_tables(dir), c("adsl", "ae"))

  # the labeled form carries format and size for pickers
  info <- container_table_info(dir)
  expect_identical(info$name, c("adsl", "ae"))
  expect_match(info$label, "^CSV \\d")

  expr <- container_read_expr(dir)
  txt <- deparse1(expr)
  expect_match(txt, "^list\\(")
  expect_match(txt, "adsl = readr::read_csv")
  expect_no_match(txt, "blockr")

  val <- eval(expr)
  expect_identical(names(val), c("adsl", "ae"))
  expect_true(all(vapply(val, is.data.frame, logical(1))))

  sel <- eval(container_read_expr(dir, tables = "ae"))
  expect_identical(names(sel), "ae")
})

test_that("a zip's members are known at build time, extraction at run time", {
  dir <- withr::local_tempdir()
  write.csv(data.frame(x = 1:3), file.path(dir, "adsl.csv"), row.names = FALSE)
  write.csv(data.frame(y = 4:6), file.path(dir, "ae.csv"), row.names = FALSE)

  archive <- file.path(dir, "study.zip")
  zip::zip(
    archive,
    c("adsl.csv", "ae.csv"),
    root = dir,
    mode = "cherry-pick"
  )

  expect_identical(container_list_tables(archive), c("adsl", "ae"))

  expr <- container_read_expr(archive)
  txt <- paste(deparse(expr), collapse = " ")
  # members are spliced explicitly against the tempdir prefix
  expect_match(txt, "utils::unzip")
  expect_match(txt, "adsl = readr::read_csv")
  expect_match(txt, 'file.path\\(tmp, "adsl.csv"\\)')
  expect_no_match(txt, "blockr")

  val <- eval(expr)
  expect_identical(names(val), c("adsl", "ae"))
  expect_true(all(vapply(val, is.data.frame, logical(1))))
})

test_that("a workbook's sheets are a container on the same extension", {
  path <- withr::local_tempfile(fileext = ".xlsx")
  writexl::write_xlsx(
    list(adsl = data.frame(x = 1:3), ae = data.frame(y = 4:6)),
    path
  )

  expect_identical(container_list_tables(path), c("adsl", "ae"))

  expr <- container_read_expr(path, tables = "ae")
  expect_match(deparse1(expr), 'sheet = "ae"')

  val <- eval(expr)
  expect_identical(names(val), "ae")
  expect_equal(val$ae$y, c(4, 5, 6))

  # single mode on the same path still reads one sheet as a data frame
  expect_match(deparse1(format_read_expr(path)), "readxl::read_excel")
})

test_that("an rds list is a container; anything else in an rds is refused", {
  path <- withr::local_tempfile(fileext = ".rds")
  saveRDS(list(adsl = data.frame(x = 1:3), ae = data.frame(y = 4:6)), path)

  expect_identical(container_list_tables(path), c("adsl", "ae"))

  expr <- container_read_expr(path, tables = "adsl")
  expect_match(deparse1(expr), "readRDS")

  val <- eval(expr)
  expect_identical(names(val), "adsl")

  bare <- withr::local_tempfile(fileext = ".rds")
  saveRDS(data.frame(x = 1), bare)
  expect_error(
    container_list_tables(bare),
    "named list of data frames"
  )
})

test_that("an rdata file's data frames are a container", {
  path <- withr::local_tempfile(fileext = ".rdata")
  adsl <- data.frame(x = 1:3)
  ae <- data.frame(y = 4:6)
  not_a_table <- "skip me"
  save(adsl, ae, not_a_table, file = path)

  expect_identical(container_list_tables(path), c("adsl", "ae"))

  val <- eval(container_read_expr(path))
  expect_identical(sort(names(val)), c("adsl", "ae"))
  expect_true(all(vapply(val, is.data.frame, logical(1))))
})

test_that("a location without a container entry errors usefully", {
  expect_error(
    container_read_expr("/data/a.sav"),
    "No container registered for \"sav\""
  )
})

test_that("multi-file reads dispatch per path, not per first file", {
  expr <- read_expr(
    paths = c("/data/a.csv", "/data/b.sav"),
    file_type = "csv",
    combine = "rbind"
  )
  txt <- deparse1(expr)
  expect_match(txt, "readr::read_csv")
  expect_match(txt, "rio::import")
})

test_that("providers are recorded for the restore hint", {
  expect_identical(blockr.io:::format_provider("/data/a.csv"), "blockr.io")
  expect_identical(blockr.io:::format_provider("/data/a.sav"), "blockr.io")
  expect_null(blockr.io:::format_provider("/data/a.xyz"))

  dir <- withr::local_tempdir()
  expect_identical(blockr.io:::container_provider(dir), "blockr.io")
})

test_that("container options are uniform member options: semicolon CSVs", {
  dir <- withr::local_tempdir()
  writeLines(c("x;y", "1;a", "2;b"), file.path(dir, "adsl.csv"))
  writeLines(c("z;w", "9;c"), file.path(dir, "adae.csv"))

  expr <- container_read_expr(dir, sep = ";")
  expect_match(deparse1(expr), 'delim = ";"')

  val <- eval(expr)
  expect_identical(names(val), c("adae", "adsl"))
  expect_identical(names(val$adsl), c("x", "y"))
  expect_identical(nrow(val$adsl), 2L)

  # the same folder without the option mis-parses into one column,
  # which is what makes the channel worth having
  bad <- eval(container_read_expr(dir))
  expect_identical(ncol(bad$adsl), 1L)
})

test_that("uniform member options flow through the zip container too", {
  dir <- withr::local_tempdir()
  writeLines(c("junk", "", "x;y", "1;a"), file.path(dir, "adsl.csv"))
  archive <- file.path(dir, "a.zip")
  zip::zip(archive, "adsl.csv", root = dir, mode = "cherry-pick")

  expr <- container_read_expr(archive, sep = ";", skip = 2)
  txt <- paste(deparse(expr), collapse = " ")
  expect_match(txt, 'delim = ";"')
  expect_match(txt, "skip = 2")

  val <- eval(expr)
  expect_identical(names(val$adsl), c("x", "y"))
})

test_that("container keys collide at lookup like format keys do", {
  withr::defer(blockr.io:::unregister_entry("container", "spcont"))

  entry <- function(pkg) {
    list(
      opens = "spcont",
      list_tables = function(path) "t",
      read = function(path, tables, ...) quote(list()),
      package = pkg
    )
  }

  blockr.io:::register_entry("container", "spcont", entry("pkg.a"))
  blockr.io:::register_entry("container", "spcont", entry("pkg.b"))
  expect_error(container_list_tables("x.spcont"), "pkg.a and pkg.b")
})

test_that("a named entry shadows the fallback deliberately", {
  withr::defer(blockr.io:::unregister_entry("single", "sav"))

  # rio claims sav; before the registration the fallback serves it
  expect_match(deparse1(format_read_expr("a.sav")), "rio::import")

  register_format(
    extensions = "sav",
    read = function(path, ...) bquote(haven::read_spss(.(path)))
  )
  expect_match(deparse1(format_read_expr("a.sav")), "haven::read_spss")
})

test_that("rdata containers honor the table selection", {
  path <- withr::local_tempfile(fileext = ".rdata")
  adsl <- data.frame(x = 1:3)
  ae <- data.frame(y = 4:6)
  save(adsl, ae, file = path)

  val <- eval(container_read_expr(path, tables = "ae"))
  expect_identical(names(val), "ae")
})

test_that("gear_band_ui wires the button to the band", {
  html <- as.character(htmltools::tagList(
    gear_band_ui("g1", "b1", htmltools::div("field"), band_label = "Opts")
  ))
  expect_match(html, "blockr-gear-btn")
  expect_match(html, 'aria-controls="b1"')
  expect_match(html, "blockrIoGearToggle\\(&#39;g1&#39;,&#39;b1&#39;\\)")
  expect_match(html, "blockr-settings--beak")
  expect_match(html, 'aria-label="Opts"')
})
