test_that(".get_default_api_url returns default when option unset", {
  old <- getOption("cytetype.default.api.url")
  on.exit(options(cytetype.default.api.url = old), add = TRUE)
  options(cytetype.default.api.url = NULL)
  expect_identical(CyteTypeR:::.get_default_api_url(), "https://prod.cytetype.nygen.io")
})

test_that(".get_default_api_url respects option when set", {
  old <- getOption("cytetype.default.api.url")
  on.exit(options(cytetype.default.api.url = old), add = TRUE)
  options(cytetype.default.api.url = "https://custom.example.com")
  expect_identical(CyteTypeR:::.get_default_api_url(), "https://custom.example.com")
})

test_that(".get_default_poll_interval returns default when option unset", {
  old <- getOption("cytetype.default.poll.interval")
  on.exit(options(cytetype.default.poll.interval = old), add = TRUE)
  options(cytetype.default.poll.interval = NULL)
  expect_identical(CyteTypeR:::.get_default_poll_interval(), 10L)
})

test_that(".get_default_timeout returns default when option unset", {
  old <- getOption("cytetype.default.timeout")
  on.exit(options(cytetype.default.timeout = old), add = TRUE)
  options(cytetype.default.timeout = NULL)
  expect_identical(CyteTypeR:::.get_default_timeout(), 7200L)
})
