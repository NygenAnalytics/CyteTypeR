test_that(".MAX_UPLOAD_BYTES has expected limits", {
  lim <- CyteTypeR:::.MAX_UPLOAD_BYTES
  expect_identical(lim$obs_duckdb, 100L * 1024L * 1024L)
  expect_equal(lim$vars_h5, 10 * 1024 * 1024 * 1024)
  expect_true(is.numeric(lim$vars_h5), info = "vars_h5 must be numeric to avoid integer overflow")
})

test_that("chunked upload retry constants are as expected", {
  expect_identical(CyteTypeR:::.CHUNK_UPLOAD_BACKOFF_SECS, c(1L, 5L, 20L))
  expect_identical(CyteTypeR:::.CHUNK_UPLOAD_TRANSIENT_STATUSES, c(500L, 502L, 503L, 504L))
})

test_that(".upload_file_chunked errors when file does not exist", {
  expect_error(
    CyteTypeR:::.upload_file_chunked("https://api.example.com", NULL, "obs_duckdb", "/nonexistent/path.duckdb", 60L),
    "Upload file not found"
  )
})

test_that(".upload_file_chunked errors for unknown file_kind (no upload limit)", {
  f <- tempfile(fileext = ".bin")
  on.exit(unlink(f), add = TRUE)
  writeBin(raw(1), f)
  expect_error(
    CyteTypeR:::.upload_file_chunked("https://api.example.com", NULL, "unknown_kind", f, 60L),
    "exceeds upload limit"
  )
})
