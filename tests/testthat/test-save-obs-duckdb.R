# DuckDB 1.4 reads storage versions 64 to 67; DuckDB 2.0 writes 999 by default,
# which the CyteType server cannot open while it runs DuckDB 1.x.
duckdb_1_4_readable <- 64:67

obs_storage_version <- function(path) {
  con <- file(path, "rb")
  on.exit(close(con))
  header <- readBin(con, "raw", 20L)
  expect_identical(header[9:12], charToRaw("DUCK"))
  sum(as.numeric(header[13:20]) * 256^(0:7))
}

obs_frame <- function() {
  data.frame(
    cluster = factor(c("a", "b", "a")),
    n_genes = c(10L, 20L, 30L),
    row.names = c("c1", "c2", "c3")
  )
}

read_obs <- function(path, sql) {
  con <- duckdb::dbConnect(duckdb::duckdb(), path, read_only = TRUE)
  on.exit(duckdb::dbDisconnect(con, shutdown = TRUE))
  DBI::dbGetQuery(con, sql)
}

test_that(".save_obs_duckdb writes a file DuckDB 1.4 can read", {
  skip_if_not_installed("duckdb")
  out <- file.path(withr::local_tempdir(), "obs.duckdb")

  CyteTypeR:::.save_obs_duckdb(out, obs_frame())

  expect_true(obs_storage_version(out) %in% duckdb_1_4_readable)
  rows <- read_obs(
    out, "SELECT CAST(cluster AS VARCHAR) AS cluster, n_genes FROM obs ORDER BY n_genes"
  )
  expect_identical(rows$cluster, c("a", "b", "a"))
  expect_identical(rows$n_genes, c(10L, 20L, 30L))
})

test_that(".save_obs_duckdb replaces a file left by an earlier run", {
  skip_if_not_installed("duckdb")
  out <- file.path(withr::local_tempdir(), "obs.duckdb")
  # Written without the compatibility setting, as earlier releases did; with
  # duckdb 2.x this file uses a format that DuckDB 1.x cannot open.
  con <- duckdb::dbConnect(duckdb::duckdb(), out)
  DBI::dbExecute(con, "CREATE TABLE leftover AS SELECT 1 AS x")
  duckdb::dbDisconnect(con, shutdown = TRUE)
  # A WAL beside the old file must not be replayed into the new one.
  writeLines("stale wal", paste0(out, ".wal"))

  CyteTypeR:::.save_obs_duckdb(out, obs_frame())

  expect_true(obs_storage_version(out) %in% duckdb_1_4_readable)
  expect_identical(list.files(dirname(out), all.files = TRUE, no.. = TRUE), "obs.duckdb")
  expect_identical(read_obs(out, "SHOW TABLES")$name, "obs")
})

test_that(".save_obs_duckdb keeps the previous file and its WAL when the new one cannot be put in place", {
  skip_if_not_installed("duckdb")
  dir <- withr::local_tempdir()
  out <- file.path(dir, "obs.duckdb")
  CyteTypeR:::.save_obs_duckdb(out, obs_frame())
  writeLines("previous wal", paste0(out, ".wal"))
  previous <- unname(tools::md5sum(out))
  # Writing nothing leaves no new file, so moving it into place fails.
  testthat::local_mocked_bindings(
    .write_duckdb_table = function(...) invisible(NULL),
    .package = "CyteTypeR"
  )

  expect_error(
    suppressWarnings(CyteTypeR:::.save_obs_duckdb(out, obs_frame())),
    "previous file was kept"
  )

  expect_identical(unname(tools::md5sum(out)), previous)
  expect_identical(readLines(paste0(out, ".wal")), "previous wal")
  expect_setequal(
    list.files(dir, all.files = TRUE, no.. = TRUE),
    c("obs.duckdb", "obs.duckdb.wal")
  )
})

test_that(".save_obs_duckdb rejects a named pipe without reading from it", {
  skip_if_not_installed("duckdb")
  skip_on_os("windows")
  skip_if(!nzchar(Sys.which("mkfifo")), "mkfifo is not available")
  out <- file.path(withr::local_tempdir(), "obs.duckdb")
  expect_identical(system2("mkfifo", shQuote(out)), 0L)

  # Before the size check, reading the pipe's header blocked until a writer appeared.
  expect_error(
    CyteTypeR:::.save_obs_duckdb(out, obs_frame()),
    "not a DuckDB database"
  )
})

test_that(".save_obs_duckdb leaves a file that is not a DuckDB database in place", {
  skip_if_not_installed("duckdb")
  out <- file.path(withr::local_tempdir(), "notes.txt")
  writeLines("keep me", out)

  expect_error(
    CyteTypeR:::.save_obs_duckdb(out, obs_frame()),
    "not a DuckDB database"
  )
  expect_identical(readLines(out), "keep me")
})

test_that(".save_obs_duckdb keeps the previous file when writing fails", {
  skip_if_not_installed("duckdb")
  dir <- withr::local_tempdir()
  out <- file.path(dir, "obs.duckdb")
  CyteTypeR:::.save_obs_duckdb(out, obs_frame())
  testthat::local_mocked_bindings(
    .write_duckdb_table = function(path, ...) {
      file.create(path)
      stop("disk full")
    },
    .package = "CyteTypeR"
  )

  expect_error(CyteTypeR:::.save_obs_duckdb(out, obs_frame()[1, ]), "disk full")

  expect_equal(read_obs(out, "SELECT count(*) AS n FROM obs")$n, 3)
  expect_identical(list.files(dir, all.files = TRUE, no.. = TRUE), "obs.duckdb")
})
