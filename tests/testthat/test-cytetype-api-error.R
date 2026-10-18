test_that("cytetype_api_error stores message and call", {
  e <- cytetype_api_error(message = "test message", call = "api")
  expect_s3_class(e, "cytetype_api_error")
  expect_identical(e$message, "test message")
  expect_identical(e$call, "api")
})

test_that("print.cytetype_api_error shows message", {
  e <- cytetype_api_error(message = "test message", call = NULL)
  expect_output(print(e), "CyteType API Error:.*test message")
})

test_that("cytetype_api_error carries an optional HTTP status", {
  e <- cytetype_api_error("boom", call = "api", error_code = "HTTP_ERROR", status_code = 503L)
  expect_identical(e$status_code, 503L)
  expect_identical(e$error_code, "HTTP_ERROR")
  expect_true("status_code" %in% names(cytetype_api_error("boom")))
  expect_null(cytetype_api_error("boom")$status_code)
})
