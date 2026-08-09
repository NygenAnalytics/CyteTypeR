.capture_cytetyper_error <- function(args) {
  messages <- capture.output(
    status <- CyteTypeR:::.cytetyper_cli_main(args),
    type = "message"
  )
  list(status = status, messages = messages)
}

test_that("setup commands pass an explicit development API URL", {
  api_url <- "https://development.example.com"
  setup_urls <- character()
  testthat::local_mocked_bindings(
    SetupCyteTypeR = function(api_url = NULL) {
      setup_urls <<- c(setup_urls, api_url)
      invisible(list())
    },
    .package = "CyteTypeR"
  )

  expect_identical(
    CyteTypeR:::.cytetyper_cli_main(c("setup", "--api-url", api_url)),
    0L
  )
  expect_identical(
    CyteTypeR:::.cytetyper_cli_main(c(
      "get-key",
      paste0("--api-url=", api_url)
    )),
    0L
  )
  expect_identical(setup_urls, c(api_url, api_url))
})

test_that("CLI commands route to the matching R functions", {
  calls <- list()
  testthat::local_mocked_bindings(
    LoginCyteTypeR = function(api_token = NULL, api_url = NULL) {
      calls$login <<- list(apiToken = api_token, apiUrl = api_url)
      invisible(list())
    },
    LogoutCyteTypeR = function() {
      calls$logout <<- TRUE
      invisible(TRUE)
    },
    OpenCyteTypeDashboard = function() {
      calls$dashboard <<- TRUE
      invisible("https://api.example.com/dashboard")
    },
    ViewCyteTypeJob = function(job_id, api_url = NULL) {
      calls$view <<- list(jobId = job_id, apiUrl = api_url)
      invisible("https://api.example.com/login")
    },
    .package = "CyteTypeR"
  )

  expect_identical(
    CyteTypeR:::.cytetyper_cli_main(c(
      "login",
      "--api-url=https://api.example.com"
    )),
    0L
  )
  expect_identical(CyteTypeR:::.cytetyper_cli_main("logout"), 0L)
  expect_identical(CyteTypeR:::.cytetyper_cli_main("dashboard"), 0L)
  expect_identical(
    CyteTypeR:::.cytetyper_cli_main(c(
      "view",
      "job/with space",
      "--api-url",
      "https://api.example.com"
    )),
    0L
  )

  expect_null(calls$login$apiToken)
  expect_identical(calls$login$apiUrl, "https://api.example.com")
  expect_true(calls$logout)
  expect_true(calls$dashboard)
  expect_identical(calls$view$jobId, "job/with space")
  expect_identical(calls$view$apiUrl, "https://api.example.com")
})

test_that("CLI help and version return successfully", {
  testthat::local_mocked_bindings(
    .cytetyper_package_version = function() "0.4.2-test",
    .package = "CyteTypeR"
  )

  help_output <- capture.output(
    help_status <- CyteTypeR:::.cytetyper_cli_main(character())
  )
  command_help <- capture.output(
    command_status <- CyteTypeR:::.cytetyper_cli_main(c("setup", "--help"))
  )
  version_output <- capture.output(
    version_status <- CyteTypeR:::.cytetyper_cli_main("--version")
  )

  expect_identical(help_status, 0L)
  expect_match(paste(help_output, collapse = "\n"), "cytetyper setup")
  expect_identical(command_status, 0L)
  expect_match(
    paste(command_help, collapse = "\n"),
    "Usage: cytetyper setup",
    fixed = TRUE
  )
  expect_identical(version_status, 0L)
  expect_identical(version_output, "cytetyper 0.4.2-test")
})

test_that("CLI parse failures return status one with concise errors", {
  unknown <- .capture_cytetyper_error("unknown")
  missing_url <- .capture_cytetyper_error(c("setup", "--api-url"))
  missing_job <- .capture_cytetyper_error("view")
  extra_argument <- .capture_cytetyper_error(c("logout", "extra"))
  duplicate_url <- .capture_cytetyper_error(c(
    "setup",
    "--api-url=https://one.example.com",
    "--api-url=https://two.example.com"
  ))

  for (result in list(
    unknown,
    missing_url,
    missing_job,
    extra_argument,
    duplicate_url
  )) {
    expect_identical(result$status, 1L)
    expect_match(result$messages[[1]], "^Error: ")
  }
  expect_match(unknown$messages[[1]], "Unknown command")
  expect_match(missing_url$messages[[1]], "requires a value")
  expect_match(missing_job$messages[[1]], "1 positional argument")
  expect_match(extra_argument$messages[[1]], "no positional arguments")
  expect_match(duplicate_url$messages[[1]], "only be provided once")
})

test_that("launcher selection supports Unix and Windows R environments", {
  expect_identical(
    CyteTypeR:::.cytetyper_launcher_name("unix"),
    "cytetyper"
  )
  expect_identical(
    CyteTypeR:::.cytetyper_launcher_name("windows"),
    "cytetyper.cmd"
  )
  expect_true(file.exists(
    CyteTypeR:::.cytetyper_launcher_source("cytetyper")
  ))
  expect_true(file.exists(
    CyteTypeR:::.cytetyper_launcher_source("cytetyper.cmd")
  ))
  launcher <- readLines(
    CyteTypeR:::.cytetyper_launcher_source("cytetyper"),
    warn = FALSE
  )
  expect_false(any(grepl("--args", launcher, fixed = TRUE)))
})

test_that("CLI installer targets the active R environment bin", {
  destination <- tempfile("cytetyper-bin-")
  dir.create(destination)
  on.exit(unlink(destination, recursive = TRUE, force = TRUE), add = TRUE)
  testthat::local_mocked_bindings(
    .cytetyper_rscript_path = function() {
      file.path(destination, "Rscript")
    },
    .cytetyper_os_type = function() "unix",
    .package = "CyteTypeR"
  )

  messages <- capture.output(
    path <- InstallCyteTypeRCli(),
    type = "message"
  )

  expect_identical(path, file.path(destination, "cytetyper"))
  expect_true(file.exists(path))
  expect_identical(readLines(path, n = 1L), "#!/bin/sh")
  expect_true(any(grepl(path, messages, fixed = TRUE)))
  expect_identical(unname(file.access(path, mode = 1L)), 0L)
  expect_error(InstallCyteTypeRCli(), "already exists")
  expect_identical(
    suppressMessages(InstallCyteTypeRCli(overwrite = TRUE)),
    path
  )
})
