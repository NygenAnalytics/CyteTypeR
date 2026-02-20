test_that(".MAX_UPLOAD_BYTES has expected limits", {
  lim <- CyteTypeR:::.MAX_UPLOAD_BYTES
  expect_identical(lim$obs_duckdb, 100L * 1024L * 1024L)
  expect_equal(lim$vars_h5, 10 * 1024 * 1024 * 1024)
  expect_true(is.numeric(lim$vars_h5), info = "vars_h5 must be numeric to avoid integer overflow")
})

test_that(".upload_file_chunked errors when file does not exist", {
  expect_error(
    CyteTypeR:::.upload_file_chunked("https://api.example.com", NULL, "obs_duckdb", "/nonexistent/path.duckdb", 60L),
    "Upload file not found"
  )
})
