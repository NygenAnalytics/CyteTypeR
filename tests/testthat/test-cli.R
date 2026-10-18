.capture_cytetyper_error <- function(args) {
  messages <- capture.output(
    status <- CyteTypeR:::.cytetyper_cli_main(args),
    type = "message"
  )
  list(status = status, messages = messages)
}

test_that("setup commands pass an explicit development API URL", {
  api_url <- "https://development.example.com"
  setup_calls <- list()
  testthat::local_mocked_bindings(
    SetupCyteTypeR = function(api_url = NULL, force = FALSE) {
      setup_calls[[length(setup_calls) + 1L]] <<- list(
        apiUrl = api_url,
        force = force
      )
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
      paste0("--api-url=", api_url),
      "--force"
    )),
    0L
  )
  expect_identical(
    setup_calls,
    list(
      list(apiUrl = api_url, force = FALSE),
      list(apiUrl = api_url, force = TRUE)
    )
  )
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
  expect_match(paste(command_help, collapse = "\n"), "--force", fixed = TRUE)
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
  duplicate_force <- .capture_cytetyper_error(c(
    "setup",
    "--force",
    "--force"
  ))
  unsupported_force <- .capture_cytetyper_error(c("login", "--force"))

  for (result in list(
    unknown,
    missing_url,
    missing_job,
    extra_argument,
    duplicate_url,
    duplicate_force,
    unsupported_force
  )) {
    expect_identical(result$status, 1L)
    expect_match(result$messages[[1]], "^Error: ")
  }
  expect_match(unknown$messages[[1]], "Unknown command")
  expect_match(missing_url$messages[[1]], "requires a value")
  expect_match(missing_job$messages[[1]], "1 positional argument")
  expect_match(extra_argument$messages[[1]], "no positional arguments")
  expect_match(duplicate_url$messages[[1]], "only be provided once")
  expect_match(duplicate_force$messages[[1]], "only be provided once")
  expect_match(unsupported_force$messages[[1]], "Unknown option")
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
  launcher_name <- CyteTypeR:::.cytetyper_launcher_name()
  testthat::local_mocked_bindings(
    .cytetyper_rscript_path = function() {
      file.path(destination, "Rscript")
    },
    .package = "CyteTypeR"
  )

  messages <- capture.output(
    path <- InstallCyteTypeRCli(),
    type = "message"
  )

  expect_identical(
    normalizePath(path, winslash = "/", mustWork = TRUE),
    normalizePath(
      file.path(destination, launcher_name),
      winslash = "/",
      mustWork = TRUE
    )
  )
  expect_true(file.exists(path))
  expect_true(any(grepl(path, messages, fixed = TRUE)))
  if (identical(CyteTypeR:::.cytetyper_os_type(), "windows")) {
    expect_identical(readLines(path, n = 1L), "@echo off")
  } else {
    expect_identical(readLines(path, n = 1L), "#!/bin/sh")
    expect_identical(unname(file.access(path, mode = 1L)), 0L)
  }
  expect_error(InstallCyteTypeRCli(), "already exists")
  expect_identical(
    suppressMessages(InstallCyteTypeRCli(overwrite = TRUE)),
    path
  )
})

local_config_home <- function(env = parent.frame()) {
  config_home <- withr::local_tempdir(.local_envir = env)
  withr::local_envvar(XDG_CONFIG_HOME = config_home, .local_envir = env)
  config_home
}

credentials_response <- function(req) {
  httr2::response(
    status_code = 200L,
    headers = list("Content-Type" = "application/json"),
    body = charToRaw(jsonlite::toJSON(
      list(
        tokenId = "token-id",
        userId = "user-id",
        email = "researcher@example.com",
        dashboardUrl = "https://api.example.com/dashboard"
      ),
      auto_unbox = TRUE
    ))
  )
}

test_that("cytetyper login reads the API key from stdin when R is not interactive", {
  local_config_home()
  authorization <- NULL
  httr2::local_mocked_responses(function(req) {
    authorization <<- httr2::req_get_headers(req, "reveal")$Authorization
    credentials_response(req)
  })
  testthat::local_mocked_bindings(
    .is_interactive = function() FALSE,
    .stdin_is_terminal = function() FALSE,
    .read_stdin_line = function() "  cyt_p_from_stdin  ",
    .package = "CyteTypeR"
  )

  messages <- capture.output(
    status <- CyteTypeR:::.cytetyper_cli_main(c("login", "--api-url=https://api.example.com")),
    type = "message"
  )

  expect_identical(status, 0L)
  expect_identical(CyteTypeR:::.load_credentials()$apiToken, "cyt_p_from_stdin")
  expect_identical(authorization, "Bearer cyt_p_from_stdin")
  expect_true(any(grepl("API key: ", messages, fixed = TRUE)))
  expect_false(any(grepl("cyt_p_from_stdin", messages, fixed = TRUE)))
})

test_that("cytetyper login hides the key typed into a terminal", {
  local_config_home()
  httr2::local_mocked_responses(credentials_response)
  echo <- logical()
  testthat::local_mocked_bindings(
    .is_interactive = function() FALSE,
    .stdin_is_terminal = function() TRUE,
    .set_terminal_echo = function(enabled) {
      echo <<- c(echo, enabled)
      TRUE
    },
    .read_stdin_line = function() "cyt_p_typed",
    .package = "CyteTypeR"
  )

  capture.output(
    status <- CyteTypeR:::.cytetyper_cli_main(c("login", "--api-url=https://api.example.com")),
    type = "message"
  )

  expect_identical(status, 0L)
  expect_identical(echo, c(FALSE, TRUE))
  expect_identical(CyteTypeR:::.load_credentials()$apiToken, "cyt_p_typed")
})

test_that("cytetyper login fails clearly when stdin is empty", {
  local_config_home()
  testthat::local_mocked_bindings(
    .is_interactive = function() FALSE,
    .stdin_is_terminal = function() FALSE,
    .read_stdin_line = function() character(0),
    .package = "CyteTypeR"
  )

  messages <- capture.output(
    status <- CyteTypeR:::.cytetyper_cli_main("login"),
    type = "message"
  )

  expect_identical(status, 1L)
  expect_true(any(grepl("Error: API key is required", messages, fixed = TRUE)))
})

test_that("an interactive login still uses the hidden askpass prompt", {
  local_config_home()
  httr2::local_mocked_responses(credentials_response)
  testthat::local_mocked_bindings(
    .is_interactive = function() TRUE,
    .read_stdin_line = function() stop("stdin must not be read"),
    .package = "CyteTypeR"
  )
  testthat::local_mocked_bindings(askpass = function(prompt) "cyt_p_prompted", .package = "askpass")

  suppressMessages(LoginCyteTypeR(api_url = "https://api.example.com"))

  expect_identical(CyteTypeR:::.load_credentials()$apiToken, "cyt_p_prompted")
})

test_that("the API key prompt reads piped stdin in a non-interactive R process", {
  skip_on_os("windows")
  functions <- c(".is_interactive", ".read_stdin_line", ".stdin_is_terminal", ".set_terminal_echo", ".prompt_api_key")
  script <- tempfile(fileext = ".R")
  on.exit(unlink(script), add = TRUE)
  definitions <- vapply(
    functions,
    function(name) {
      paste0("`", name, "` <- ", paste(deparse(get(name, envir = asNamespace("CyteTypeR"))), collapse = "\n"))
    },
    character(1)
  )
  writeLines(c(definitions, "cat(.prompt_api_key(\"API key: \"))"), script)
  stderr_file <- tempfile()
  on.exit(unlink(stderr_file), add = TRUE)

  output <- system2(
    file.path(R.home("bin"), "Rscript"),
    c("--vanilla", shQuote(script)),
    stdout = TRUE,
    stderr = stderr_file,
    input = "cyt_p_piped_key"
  )

  expect_identical(output, "cyt_p_piped_key")
  expect_match(paste(readLines(stderr_file), collapse = "\n"), "API key: ", fixed = TRUE)
})

test_that("the installed cytetyper launcher saves a key piped to login", {
  skip_on_os("windows")
  skip_on_cran()
  installed <- find.package("CyteTypeR", lib.loc = .libPaths(), quiet = TRUE)
  skip_if(length(installed) == 0L, "CyteTypeR is not installed in a library")
  launcher <- file.path(installed[[1L]], "exec", "cytetyper")
  skip_if_not(file.exists(launcher), "The installed package has no launcher")

  received <- new.env()
  port <- httpuv::randomPort(host = "127.0.0.1")
  api_url <- paste0("http://127.0.0.1:", port)
  server <- httpuv::startServer("127.0.0.1", port, list(call = function(req) {
    received$path <- req$PATH_INFO
    received$authorization <- req$HTTP_AUTHORIZATION
    list(
      status = 200L,
      headers = list("Content-Type" = "application/json"),
      body = as.character(jsonlite::toJSON(
        list(
          tokenId = "token-id",
          userId = "user-id",
          email = "researcher@example.com",
          dashboardUrl = paste0(api_url, "/dashboard")
        ),
        auto_unbox = TRUE
      ))
    )
  }))
  on.exit(httpuv::stopServer(server), add = TRUE)

  bin <- withr::local_tempdir()
  file.copy(launcher, file.path(bin, "cytetyper"))
  Sys.chmod(file.path(bin, "cytetyper"), mode = "0755")
  file.symlink(file.path(R.home("bin"), "Rscript"), file.path(bin, "Rscript"))
  config_home <- local_config_home()
  withr::local_envvar(
    R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep),
    CYTETYPE_API_URL = NA
  )
  key_file <- withr::local_tempfile(lines = "cyt_p_launcher_key")
  output_file <- withr::local_tempfile()

  system2(
    file.path(bin, "cytetyper"),
    c("login", "--api-url", api_url),
    stdin = key_file,
    stdout = output_file,
    stderr = output_file,
    wait = FALSE
  )
  credentials_path <- file.path(config_home, "cytetype", "credentials.json")
  deadline <- Sys.time() + 120
  while (!file.exists(credentials_path) && Sys.time() < deadline) {
    httpuv::service(100)
  }

  expect_true(file.exists(credentials_path))
  expect_identical(received$path, "/auth/cli/credentials")
  expect_identical(received$authorization, "Bearer cyt_p_launcher_key")
  expect_identical(jsonlite::fromJSON(credentials_path)$apiToken, "cyt_p_launcher_key")
})
