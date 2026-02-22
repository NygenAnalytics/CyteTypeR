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
