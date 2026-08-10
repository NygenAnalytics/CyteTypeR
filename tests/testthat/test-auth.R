.auth_credentials <- function(
  api_url = "https://api.example.com",
  dashboard_url = "https://api.example.com/dashboard",
  api_token = "cyt_p_test_secret"
) {
  list(
    apiUrl = api_url,
    dashboardUrl = dashboard_url,
    apiToken = api_token,
    tokenId = "token-id",
    userId = "user-id",
    email = "researcher@example.com"
  )
}

.with_temp_config_home <- function(code) {
  config_home <- tempfile("cytetype-config-")
  dir.create(config_home)
  old <- Sys.getenv("XDG_CONFIG_HOME", unset = NA_character_)
  on.exit({
    unlink(config_home, recursive = TRUE, force = TRUE)
    if (is.na(old)) {
      Sys.unsetenv("XDG_CONFIG_HOME")
    } else {
      Sys.setenv(XDG_CONFIG_HOME = old)
    }
  }, add = TRUE)
  Sys.setenv(XDG_CONFIG_HOME = config_home)
  force(code)
}

test_that("API URL validation accepts origins and rejects unsafe values", {
  expect_identical(
    CyteTypeR:::.validate_api_url(" https://api.example.com/ "),
    "https://api.example.com"
  )
  expect_identical(
    CyteTypeR:::.validate_api_url("http://127.0.0.1:8000/"),
    "http://127.0.0.1:8000"
  )
  expect_identical(
    CyteTypeR:::.validate_api_url("http://localhost:8000"),
    "http://localhost:8000"
  )

  invalid <- c(
    "http://api.example.com",
    "https://user@api.example.com",
    "https://api.example.com/path",
    "https://api.example.com?query=value",
    "https://api.example.com#fragment",
    "ftp://api.example.com"
  )
  for (value in invalid) {
    expect_error(
      CyteTypeR:::.validate_api_url(value),
      "API URL|Non-local"
    )
  }
})

test_that("credentials round trip through the shared secure path", {
  .with_temp_config_home({
    credentials <- .auth_credentials()
    path <- CyteTypeR:::.save_credentials(credentials)

    expect_identical(
      path,
      file.path(
        Sys.getenv("XDG_CONFIG_HOME"),
        "cytetype",
        "credentials.json"
      )
    )
    expect_identical(
      CyteTypeR:::.load_credentials("https://api.example.com/"),
      credentials
    )
    expect_null(
      CyteTypeR:::.load_credentials("https://other.example.com")
    )

    if (.Platform$OS.type == "unix") {
      expect_identical(as.character(file.info(dirname(path))$mode), "700")
      expect_identical(as.character(file.info(path)$mode), "600")
    }
  })
})

test_that("credential save repairs existing POSIX directory permissions", {
  skip_if(.Platform$OS.type != "unix", "POSIX permissions required")
  .with_temp_config_home({
    directory <- dirname(CyteTypeR:::.credentials_path())
    dir.create(directory, recursive = TRUE)
    Sys.chmod(directory, mode = "0777")

    CyteTypeR:::.save_credentials(.auth_credentials())

    expect_identical(as.character(file.info(directory)$mode), "700")
  })
})

test_that("credential save rejects a directory owned by another user", {
  skip_if(.Platform$OS.type != "unix", "POSIX ownership required")
  .with_temp_config_home({
    testthat::local_mocked_bindings(
      .credentials_directory_owner = function(directory) "another-user",
      .current_effective_user = function() "current-user",
      .package = "CyteTypeR"
    )

    expect_error(
      CyteTypeR:::.save_credentials(.auth_credentials()),
      "not owned by the current user"
    )
    expect_false(file.exists(CyteTypeR:::.credentials_path()))
  })
})

test_that("legacy credentials default to the production dashboard", {
  .with_temp_config_home({
    path <- CyteTypeR:::.credentials_path()
    dir.create(dirname(path), recursive = TRUE)
    jsonlite::write_json(
      list(
        apiUrl = "https://api.example.com",
        apiToken = "cyt_p_test_secret",
        tokenId = "token-id",
        userId = "user-id",
        email = "researcher@example.com"
      ),
      path,
      auto_unbox = TRUE
    )

    credentials <- CyteTypeR:::.load_credentials()

    expect_identical(
      credentials$dashboardUrl,
      "https://cytetype.nygen.io/dashboard"
    )
  })
})

test_that("invalid credentials file has a clear error", {
  .with_temp_config_home({
    path <- CyteTypeR:::.credentials_path()
    dir.create(dirname(path), recursive = TRUE)
    writeLines('{"apiUrl":"https://api.example.com"}', path)

    expect_error(
      CyteTypeR:::.load_credentials(),
      "Invalid CyteType credentials file"
    )
  })
})

test_that("dashboard URL is constrained to the API origin", {
  same_origin <- .auth_credentials(
    dashboard_url = "https://api.example.com/custom-dashboard"
  )
  expect_identical(
    CyteTypeR:::.resolve_dashboard_url(same_origin),
    "https://api.example.com/custom-dashboard"
  )

  other_origin <- .auth_credentials(
    dashboard_url = "https://dashboard.example.com/dashboard"
  )
  expect_identical(
    CyteTypeR:::.resolve_dashboard_url(other_origin),
    "https://api.example.com/dashboard"
  )
})

test_that("PKCE and authorization state satisfy the server contract", {
  pkce <- CyteTypeR:::.create_pkce_pair()
  state <- CyteTypeR:::.create_authorization_state()

  expect_match(pkce$verifier, "^[A-Za-z0-9_-]{43,128}$")
  expect_match(pkce$challenge, "^[A-Za-z0-9_-]{43}$")
  expect_match(state, "^[A-Za-z0-9_-]{16,256}$", perl = TRUE)
  expected_challenge <- CyteTypeR:::.base64url_encode(
    openssl::sha256(charToRaw(pkce$verifier))
  )
  expect_identical(pkce$challenge, expected_challenge)
})

test_that("state comparison checks every equal-length byte", {
  expect_true(CyteTypeR:::.secure_string_equal(
    "state_value_1234567890",
    "state_value_1234567890"
  ))
  expect_false(CyteTypeR:::.secure_string_equal(
    "state_value_1234567890",
    "state_value_1234567891"
  ))
  expect_false(CyteTypeR:::.secure_string_equal(
    "state_value_1234567890",
    "short"
  ))
})

test_that("callback query parsing accepts the httpuv leading question mark", {
  query_string <- paste0(
    "code=header.payload.signature",
    "&state=state_value_1234567890"
  )

  without_prefix <- CyteTypeR:::.parse_callback_query(query_string)
  with_prefix <- CyteTypeR:::.parse_callback_query(
    paste0("?", query_string)
  )

  expect_identical(with_prefix, without_prefix)
  expect_identical(with_prefix$code, "header.payload.signature")
  expect_identical(with_prefix$state, "state_value_1234567890")
})

test_that("callback validates state and exchanges the authorization code", {
  credentials <- .auth_credentials(api_url = "https://api.example.com")
  exchange <- list()
  testthat::local_mocked_bindings(
    .exchange_cli_token = function(api_url, code, verifier, redirect_uri) {
      exchange <<- list(
        api_url = api_url,
        code = code,
        verifier = verifier,
        redirect_uri = redirect_uri
      )
      credentials
    },
    .package = "CyteTypeR"
  )

  result <- new.env(parent = emptyenv())
  result$complete <- FALSE
  result$value <- NULL
  app <- CyteTypeR:::.create_callback_app(
    "https://api.example.com",
    "state_value_1234567890",
    "verifier",
    "http://127.0.0.1:49152/callback",
    result
  )
  response <- app(list(
    PATH_INFO = "/callback",
    QUERY_STRING = "?code=signed-code&state=state_value_1234567890"
  ))

  expect_identical(response$status, 200L)
  expect_true(result$complete)
  expect_identical(result$value, credentials)
  expect_identical(exchange$code, "signed-code")
  expect_identical(exchange$verifier, "verifier")
  expect_false(grepl(credentials$apiToken, response$body, fixed = TRUE))
  expect_false(grepl("signed-code", response$body, fixed = TRUE))
  expect_match(response$body, 'class="card success"', fixed = TRUE)
  expect_match(
    response$body,
    "<h1>CyteType setup is complete.</h1>",
    fixed = TRUE
  )
  expect_match(response$body, credentials$email, fixed = TRUE)
  expect_match(
    response$body,
    'content="5;url=https://api.example.com/dashboard"',
    fixed = TRUE
  )
  expect_match(
    response$body,
    'href="https://api.example.com/dashboard"',
    fixed = TRUE
  )
  expect_match(response$body, 'class="progress"', fixed = TRUE)
  expect_match(
    response$body,
    "CyteType by Nygen Analytics",
    fixed = TRUE
  )
  expect_identical(response$headers[["Cache-Control"]], "no-store")
  expect_match(
    response$headers[["Content-Security-Policy"]],
    "default-src 'none'",
    fixed = TRUE
  )
})

test_that("callback rejects wrong state without exchanging a token", {
  testthat::local_mocked_bindings(
    .exchange_cli_token = function(...) {
      fail("A callback with the wrong state must not exchange a token")
    },
    .package = "CyteTypeR"
  )
  result <- new.env(parent = emptyenv())
  result$complete <- FALSE
  result$value <- NULL
  app <- CyteTypeR:::.create_callback_app(
    "https://api.example.com",
    "expected_state_123456",
    "verifier",
    "http://127.0.0.1:49152/callback",
    result
  )

  response <- app(list(
    PATH_INFO = "/callback",
    QUERY_STRING = "code=signed-code&state=wrong_state_12345678"
  ))

  expect_identical(response$status, 400L)
  expect_false(result$complete)
  expect_match(response$body, "Authorization state did not match", fixed = TRUE)
  expect_match(response$body, 'class="card error"', fixed = TRUE)
  expect_match(response$body, "Return to R and try again.", fixed = TRUE)
  expect_false(grepl('http-equiv="refresh"', response$body, fixed = TRUE))
})

test_that("callback records authorization denial", {
  result <- new.env(parent = emptyenv())
  result$complete <- FALSE
  result$value <- NULL
  app <- CyteTypeR:::.create_callback_app(
    "https://api.example.com",
    "expected_state_123456",
    "verifier",
    "http://127.0.0.1:49152/callback",
    result
  )

  response <- app(list(
    PATH_INFO = "/callback",
    QUERY_STRING = paste0(
      "error=access_denied&state=",
      "expected_state_123456"
    )
  ))

  expect_identical(response$status, 400L)
  expect_true(result$complete)
  expect_s3_class(result$value, "cytetype_auth_error")
  expect_identical(result$value$error_code, "ACCESS_DENIED")
})

test_that("callback template escapes all server-provided values", {
  credentials <- .auth_credentials(
    dashboard_url = paste0(
      "https://api.example.com/dashboard?",
      'next="quoted"&tag=<unsafe>'
    )
  )
  credentials$email <- "<researcher&admin@example.com>"

  body <- CyteTypeR:::.render_callback_page(
    'Setup <failed> & "retry"',
    credentials
  )

  expect_match(body, "Setup &lt;failed&gt; &amp; &quot;retry&quot;", fixed = TRUE)
  expect_match(
    body,
    "&lt;researcher&amp;admin@example.com&gt;",
    fixed = TRUE
  )
  expect_match(body, "next=&quot;quoted&quot;&amp;tag=&lt;unsafe&gt;", fixed = TRUE)
  expect_false(grepl("<researcher&admin@example.com>", body, fixed = TRUE))
})

test_that("auth error parser accepts plain and structured detail", {
  structured <- httr2::response(
    status_code = 401L,
    headers = list("Content-Type" = "application/json"),
    body = charToRaw(jsonlite::toJSON(
      list(detail = list(
        error_code = "INVALID_TOKEN",
        message = "Invalid API token"
      )),
      auto_unbox = TRUE
    ))
  )
  plain <- httr2::response(
    status_code = 401L,
    headers = list("Content-Type" = "application/json"),
    body = charToRaw(jsonlite::toJSON(
      list(detail = "Invalid authorization code"),
      auto_unbox = TRUE
    ))
  )

  expect_identical(
    CyteTypeR:::.auth_error_message(structured, "fallback"),
    "Invalid API token"
  )
  expect_identical(
    CyteTypeR:::.auth_error_message(plain, "fallback"),
    "Invalid authorization code"
  )
})

test_that("manual login validates and saves without displaying the API key", {
  .with_temp_config_home({
    token <- "cyt_p_existing_secret"
    httr2::local_mocked_responses(function(req) {
      expect_match(req$url, "/auth/cli/credentials$", perl = TRUE)
      expect_true("Authorization" %in% names(req$headers))
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
    })

    messages <- capture.output(
      credentials <- LoginCyteTypeR(
        api_token = token,
        api_url = "https://api.example.com"
      ),
      type = "message"
    )

    stored_credentials <- CyteTypeR:::.load_credentials()
    expect_false("apiToken" %in% names(credentials))
    expect_identical(stored_credentials$apiToken, token)
    expect_identical(
      credentials,
      CyteTypeR:::.credential_metadata(stored_credentials)
    )
    expect_false(any(grepl(token, messages, fixed = TRUE)))
  })
})

test_that("failed manual login preserves existing credentials", {
  .with_temp_config_home({
    existing <- .auth_credentials()
    CyteTypeR:::.save_credentials(existing)
    invalid_token <- "cyt_p_invalid_secret"
    httr2::local_mocked_responses(function(req) {
      httr2::response(
        status_code = 401L,
        headers = list("Content-Type" = "application/json"),
        body = charToRaw(jsonlite::toJSON(
          list(detail = list(
            error_code = "INVALID_TOKEN",
            message = "Invalid API token"
          )),
          auto_unbox = TRUE
        ))
      )
    })

    expect_error(
      LoginCyteTypeR(
        api_token = invalid_token,
        api_url = "https://api.example.com"
      ),
      "Invalid API token",
      class = "cytetype_auth_error"
    )
    expect_identical(CyteTypeR:::.load_credentials(), existing)
  })
})

test_that("setup reuses matching credentials without opening a browser", {
  .with_temp_config_home({
    existing <- .auth_credentials()
    CyteTypeR:::.save_credentials(existing)
    httr2::local_mocked_responses(function(req) {
      expect_match(req$url, "/auth/cli/credentials$", perl = TRUE)
      expect_true("Authorization" %in% names(req$headers))
      httr2::response(
        status_code = 200L,
        headers = list("Content-Type" = "application/json"),
        body = charToRaw(jsonlite::toJSON(
          list(
            tokenId = "refreshed-token-id",
            userId = "refreshed-user-id",
            email = "current@example.com",
            dashboardUrl = "https://api.example.com/dashboard"
          ),
          auto_unbox = TRUE
        ))
      )
    })
    testthat::local_mocked_bindings(
      .run_browser_setup = function(...) {
        fail("Existing setup must not open a browser")
      },
      .package = "CyteTypeR"
    )

    console_output <- capture.output(
      messages <- capture.output(
        result <- SetupCyteTypeR("https://api.example.com"),
        type = "message"
      ),
      type = "output"
    )

    stored <- CyteTypeR:::.load_credentials()
    expect_identical(stored$apiToken, existing$apiToken)
    expect_identical(stored$tokenId, "refreshed-token-id")
    expect_identical(stored$userId, "refreshed-user-id")
    expect_identical(stored$email, "current@example.com")
    expect_identical(
      result,
      CyteTypeR:::.credential_metadata(stored)
    )
    expect_true(any(grepl(stored$email, messages, fixed = TRUE)))
    expect_true(any(grepl(
      CyteTypeR:::.CYTETYPE_NYGEN_BANNER[[1]],
      console_output,
      fixed = TRUE
    )))
    expect_true(any(grepl("Nygen Analytics:", console_output, fixed = TRUE)))
    expect_true(any(grepl("https://nygen.io", console_output, fixed = TRUE)))
    expect_false(any(grepl(
      existing$apiToken,
      c(console_output, messages),
      fixed = TRUE
    )))
  })
})

test_that("setup rejects invalid saved credentials without changing them", {
  .with_temp_config_home({
    existing <- .auth_credentials()
    CyteTypeR:::.save_credentials(existing)
    httr2::local_mocked_responses(function(req) {
      httr2::response(
        status_code = 401L,
        headers = list("Content-Type" = "application/json"),
        body = charToRaw(jsonlite::toJSON(
          list(detail = list(
            error_code = "INVALID_TOKEN",
            message = "Invalid API token"
          )),
          auto_unbox = TRUE
        ))
      )
    })
    testthat::local_mocked_bindings(
      .run_browser_setup = function(...) {
        fail("Invalid setup must not open a browser")
      },
      .package = "CyteTypeR"
    )

    expect_error(
      suppressMessages(SetupCyteTypeR("https://api.example.com")),
      "cytetyper setup --force",
      class = "cytetype_auth_error"
    )
    expect_identical(CyteTypeR:::.load_credentials(), existing)
  })
})

test_that("setup preserves inactive-token errors and saved credentials", {
  .with_temp_config_home({
    existing <- .auth_credentials()
    CyteTypeR:::.save_credentials(existing)
    httr2::local_mocked_responses(function(req) {
      httr2::response(
        status_code = 403L,
        headers = list("Content-Type" = "application/json"),
        body = charToRaw(jsonlite::toJSON(
          list(detail = list(
            error_code = "TOKEN_INACTIVE",
            message = "API token has been deactivated"
          )),
          auto_unbox = TRUE
        ))
      )
    })
    testthat::local_mocked_bindings(
      .run_browser_setup = function(...) {
        fail("Inactive setup must not open a browser")
      },
      .package = "CyteTypeR"
    )

    error <- tryCatch(
      suppressMessages(SetupCyteTypeR("https://api.example.com")),
      cytetype_auth_error = identity
    )
    expect_s3_class(error, "cytetype_auth_error")
    expect_identical(error$error_code, "TOKEN_INACTIVE")
    expect_match(conditionMessage(error), "cytetyper setup --force")
    expect_identical(CyteTypeR:::.load_credentials(), existing)
  })
})

test_that("setup reports validation network errors without changing credentials", {
  .with_temp_config_home({
    existing <- .auth_credentials()
    CyteTypeR:::.save_credentials(existing)
    httr2::local_mocked_responses(function(req) {
      stop("offline")
    })
    testthat::local_mocked_bindings(
      .run_browser_setup = function(...) {
        fail("Failed validation must not open a browser")
      },
      .package = "CyteTypeR"
    )

    error <- tryCatch(
      suppressMessages(SetupCyteTypeR("https://api.example.com")),
      cytetype_auth_error = identity
    )
    expect_s3_class(error, "cytetype_auth_error")
    expect_identical(error$error_code, "NETWORK_ERROR")
    expect_match(conditionMessage(error), "Could not validate the saved API key")
    expect_false(grepl("--force", conditionMessage(error), fixed = TRUE))
    expect_identical(CyteTypeR:::.load_credentials(), existing)
  })
})

test_that("forced setup skips validation and replaces saved credentials", {
  .with_temp_config_home({
    existing <- .auth_credentials()
    refreshed <- .auth_credentials(
      api_token = "cyt_p_reauthenticated_secret"
    )
    refreshed$email <- "current@example.com"
    CyteTypeR:::.save_credentials(existing)
    testthat::local_mocked_bindings(
      .perform_auth_request = function(...) {
        fail("Forced setup must not validate saved credentials")
      },
      .run_browser_setup = function(...) refreshed,
      .package = "CyteTypeR"
    )

    messages <- capture.output(
      result <- SetupCyteTypeR(
        "https://api.example.com",
        force = TRUE
      ),
      type = "message"
    )

    expect_identical(CyteTypeR:::.load_credentials(), refreshed)
    expect_identical(
      result,
      CyteTypeR:::.credential_metadata(refreshed)
    )
    expect_false(any(grepl(refreshed$apiToken, messages, fixed = TRUE)))
  })
})

test_that("browser setup orchestrates authorize, callback, and exchange", {
  credentials <- .auth_credentials()
  callback_app <- NULL
  callback_response <- NULL
  opened_url <- NULL
  exchange <- list()
  service_calls <- 0L
  stopped <- FALSE
  testthat::local_mocked_bindings(
    .start_callback_server = function(app_factory) {
      callback_app <<- app_factory(49152L)
      list(server = list(), port = 49152L)
    },
    .stop_callback_server = function(server) {
      stopped <<- TRUE
      invisible(NULL)
    },
    .open_browser_silently = function(url) {
      opened_url <<- url
      TRUE
    },
    .service_callback_server = function(timeout_ms = 100) {
      service_calls <<- service_calls + 1L
      if (service_calls == 1L) {
        authorize <- httr2::url_parse(opened_url)
        callback_response <<- callback_app(list(
          PATH_INFO = "/callback",
          QUERY_STRING = paste0(
            "?code=signed-code&state=",
            authorize$query$state
          )
        ))
      }
      invisible(NULL)
    },
    .exchange_cli_token = function(api_url, code, verifier, redirect_uri) {
      exchange <<- list(
        api_url = api_url,
        code = code,
        verifier = verifier,
        redirect_uri = redirect_uri
      )
      credentials
    },
    .package = "CyteTypeR"
  )

  messages <- capture.output(
    result <- CyteTypeR:::.run_browser_setup(
      "https://api.example.com",
      timeout_seconds = 5
    ),
    type = "message"
  )

  authorize <- httr2::url_parse(opened_url)
  expect_identical(result, credentials)
  expect_identical(
    authorize$path,
    "/auth/cli/authorize"
  )
  expect_identical(
    authorize$query$redirectUri,
    "http://127.0.0.1:49152/callback"
  )
  expect_match(authorize$query$state, "^[A-Za-z0-9_-]+$")
  expect_match(authorize$query$codeChallenge, "^[A-Za-z0-9_-]{43}$")
  expect_false(grepl(exchange$verifier, opened_url, fixed = TRUE))
  expect_identical(exchange$api_url, "https://api.example.com")
  expect_identical(exchange$code, "signed-code")
  expect_identical(
    exchange$redirect_uri,
    "http://127.0.0.1:49152/callback"
  )
  expect_identical(callback_response$status, 200L)
  expect_match(callback_response$body, 'class="card success"', fixed = TRUE)
  expect_true(stopped)
  expect_true(any(grepl(
    "If it does not open automatically",
    messages,
    fixed = TRUE
  )))
})

test_that("browser setup times out cleanly", {
  stopped <- FALSE
  testthat::local_mocked_bindings(
    .open_browser_silently = function(url) TRUE,
    .start_callback_server = function(app_factory) {
      expect_true(is.function(app_factory))
      list(server = list(), port = 49152L)
    },
    .stop_callback_server = function(server) {
      stopped <<- TRUE
      invisible(NULL)
    },
    .service_callback_server = function(timeout_ms = 100) {
      invisible(NULL)
    },
    .package = "CyteTypeR"
  )

  expect_error(
    suppressMessages(CyteTypeR:::.run_browser_setup(
      "https://api.example.com",
      timeout_seconds = 0
    )),
    "timed out",
    class = "cytetype_auth_error"
  )
  expect_true(stopped)
})

test_that("WSL browser launch uses the Windows URL handler", {
  launched <- list()
  url <- "https://api.example.com/auth/cli/authorize?state=test"
  testthat::local_mocked_bindings(
    .is_wsl_environment = function() TRUE,
    .find_browser_launcher = function(command) {
      expect_identical(command, "rundll32.exe")
      "/mnt/c/Windows/System32/rundll32.exe"
    },
    .launch_browser_process = function(command, args) {
      launched <<- list(command = command, args = args)
      TRUE
    },
    .system_name = function() {
      fail("WSL must not use the native platform branch")
    },
    .browse_url_silently = function(url) {
      fail("WSL must not use the fallback browser")
    },
    .package = "CyteTypeR"
  )

  expect_true(CyteTypeR:::.open_browser_silently(url))
  expect_identical(
    launched$command,
    "/mnt/c/Windows/System32/rundll32.exe"
  )
  expect_identical(
    launched$args,
    c("url.dll,FileProtocolHandler", shQuote(url))
  )
})

test_that("WSL browser launch does not fall back when handler is missing", {
  testthat::local_mocked_bindings(
    .is_wsl_environment = function() TRUE,
    .find_browser_launcher = function(command) "",
    .launch_browser_process = function(...) {
      fail("Missing launchers must not be invoked")
    },
    .browse_url_silently = function(url) {
      fail("WSL must not use the fallback browser")
    },
    .package = "CyteTypeR"
  )

  expect_false(CyteTypeR:::.open_browser_silently(
    "https://api.example.com/auth/cli/authorize"
  ))
})

test_that("dashboard, job view, and logout do not expose the token", {
  .with_temp_config_home({
    credentials <- .auth_credentials(
      dashboard_url = "https://other.example.com/dashboard"
    )
    CyteTypeR:::.save_credentials(credentials)
    opened <- character()
    testthat::local_mocked_bindings(
      .open_browser_silently = function(url) {
        opened <<- c(opened, url)
        TRUE
      },
      .package = "CyteTypeR"
    )

    dashboard <- suppressMessages(OpenCyteTypeDashboard())
    job <- suppressMessages(ViewCyteTypeJob("job/with space"))

    expect_identical(dashboard, "https://api.example.com/dashboard")
    expect_false(any(grepl(credentials$apiToken, opened, fixed = TRUE)))
    parsed_job <- httr2::url_parse(job)
    expect_identical(parsed_job$hostname, "api.example.com")
    expect_identical(
      parsed_job$query$redirect,
      "/report/job%2Fwith%20space"
    )

    expect_true(suppressMessages(LogoutCyteTypeR()))
    expect_null(CyteTypeR:::.load_credentials())
    expect_false(suppressMessages(LogoutCyteTypeR()))
  })
})

test_that("auth token resolution is explicit or server-specific", {
  .with_temp_config_home({
    credentials <- .auth_credentials()
    CyteTypeR:::.save_credentials(credentials)

    expect_identical(
      CyteTypeR:::.resolve_auth_token(
        "https://api.example.com",
        "explicit-token"
      ),
      "explicit-token"
    )
    expect_identical(
      CyteTypeR:::.resolve_auth_token("https://api.example.com"),
      credentials$apiToken
    )
    expect_error(
      CyteTypeR:::.resolve_auth_token("https://other.example.com"),
      "SetupCyteTypeR",
      class = "cytetype_auth_error"
    )
  })
})

test_that("GetResults resolves credentials for stored remote jobs", {
  obj <- Seurat::CreateSeuratObject(
    Matrix::sparseMatrix(
      i = c(1L, 2L),
      j = c(1L, 2L),
      x = c(1, 1),
      dims = c(2L, 2L)
    )
  )
  obj@misc$cytetype_jobDetails <- list(
    job_id = "job-123",
    api_url = "https://api.example.com"
  )
  credentials <- .auth_credentials()
  request <- list()
  testthat::local_mocked_bindings(
    .load_credentials = function(api_url = NULL) {
      expect_identical(api_url, "https://api.example.com")
      credentials
    },
    .make_results_request = function(job_id, api_url, auth_token = NULL) {
      request <<- list(
        job_id = job_id,
        api_url = api_url,
        auth_token = auth_token
      )
      list(status = "pending", result = NULL)
    },
    .package = "CyteTypeR"
  )

  expect_null(GetResults(obj))
  expect_identical(request$job_id, "job-123")
  expect_identical(request$api_url, "https://api.example.com")
  expect_identical(request$auth_token, credentials$apiToken)
})

test_that("GetResults rejects an untrusted stored server before network access", {
  obj <- Seurat::CreateSeuratObject(
    Matrix::sparseMatrix(
      i = c(1L, 2L),
      j = c(1L, 2L),
      x = c(1, 1),
      dims = c(2L, 2L)
    )
  )
  obj@misc$cytetype_jobDetails <- list(
    job_id = "job-123",
    api_url = "https://modified.example.com/path"
  )
  testthat::local_mocked_bindings(
    .load_credentials = function(...) {
      fail("Invalid stored URLs must be rejected before credential lookup")
    },
    .make_results_request = function(...) {
      fail("Invalid stored URLs must be rejected before network access")
    },
    .package = "CyteTypeR"
  )

  expect_error(GetResults(obj), "server origin")
})

test_that("GetResults will not send an explicit token to a stored origin", {
  obj <- Seurat::CreateSeuratObject(
    Matrix::sparseMatrix(
      i = c(1L, 2L),
      j = c(1L, 2L),
      x = c(1, 1),
      dims = c(2L, 2L)
    )
  )
  obj@misc$cytetype_jobDetails <- list(
    job_id = "job-123",
    api_url = "https://modified.example.com"
  )
  testthat::local_mocked_bindings(
    .make_results_request = function(...) {
      fail("A mismatched token must be rejected before network access")
    },
    .package = "CyteTypeR"
  )

  expect_error(
    GetResults(obj, auth_token = "explicit-token"),
    "api_url is required",
    class = "cytetype_auth_error"
  )
  expect_error(
    GetResults(
      obj,
      auth_token = "explicit-token",
      api_url = "https://api.example.com"
    ),
    "must match",
    class = "cytetype_auth_error"
  )
})

test_that("GetResults sends an explicit token only with a matching origin", {
  obj <- Seurat::CreateSeuratObject(
    Matrix::sparseMatrix(
      i = c(1L, 2L),
      j = c(1L, 2L),
      x = c(1, 1),
      dims = c(2L, 2L)
    )
  )
  obj@misc$cytetype_jobDetails <- list(
    job_id = "job-123",
    api_url = "https://api.example.com"
  )
  request <- list()
  testthat::local_mocked_bindings(
    .load_credentials = function(...) {
      fail("Explicit tokens must not read stored credentials")
    },
    .make_results_request = function(job_id, api_url, auth_token = NULL) {
      request <<- list(
        job_id = job_id,
        api_url = api_url,
        auth_token = auth_token
      )
      list(status = "pending", result = NULL)
    },
    .package = "CyteTypeR"
  )

  expect_null(GetResults(
    obj,
    auth_token = "explicit-token",
    api_url = "https://api.example.com/"
  ))
  expect_identical(request$auth_token, "explicit-token")
  expect_identical(request$api_url, "https://api.example.com")
})

test_that("GetResults reports remote request errors", {
  obj <- Seurat::CreateSeuratObject(
    Matrix::sparseMatrix(
      i = c(1L, 2L),
      j = c(1L, 2L),
      x = c(1, 1),
      dims = c(2L, 2L)
    )
  )
  obj@misc$cytetype_jobDetails <- list(
    job_id = "job-123",
    api_url = "https://api.example.com"
  )
  testthat::local_mocked_bindings(
    .load_credentials = function(api_url = NULL) .auth_credentials(),
    .make_results_request = function(...) {
      list(
        status = "error",
        result = NULL,
        message = "Authentication failed"
      )
    },
    .package = "CyteTypeR"
  )

  expect_error(GetResults(obj), "Authentication failed")
})

test_that("GetResults returns wrapped local results without authentication", {
  obj <- Seurat::CreateSeuratObject(
    Matrix::sparseMatrix(
      i = c(1L, 2L),
      j = c(1L, 2L),
      x = c(1, 1),
      dims = c(2L, 2L)
    )
  )
  expected <- list(annotations = list())
  obj@misc$cytetype_results <- list(
    job_id = "job-123",
    result = expected
  )
  testthat::local_mocked_bindings(
    .load_credentials = function(...) {
      fail("Local results must not require credentials")
    },
    .package = "CyteTypeR"
  )

  expect_identical(GetResults(obj), expected)
})

test_that("GetResults supports locally transformed result tables", {
  obj <- Seurat::CreateSeuratObject(
    Matrix::sparseMatrix(
      i = c(1L, 2L),
      j = c(1L, 2L),
      x = c(1, 1),
      dims = c(2L, 2L)
    )
  )
  expected <- data.frame(
    clusterId = "1",
    annotation = "T cell"
  )
  obj@misc$cytetype_results <- expected
  testthat::local_mocked_bindings(
    .load_credentials = function(...) {
      fail("Local results must not require credentials")
    },
    .package = "CyteTypeR"
  )

  expect_identical(GetResults(obj), expected)
})
