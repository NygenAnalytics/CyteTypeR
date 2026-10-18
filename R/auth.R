.CYTETYPE_DEFAULT_API_URL <- "https://cytetype.nygen.io"
.CYTETYPE_DEFAULT_DASHBOARD_URL <- "https://cytetype.nygen.io/dashboard"
.CYTETYPE_SETUP_TIMEOUT_SECONDS <- 300

.create_nygen_banner <- function() {
  glyphs <- list(
    n = c("      ", "# ### ", "##   #", "#    #", "#    #", "#    #", "#    #"),
    y = c("#   #", "#   #", "#   #", " ####", "    #", "#   #", " ### "),
    g = c(" ### ", "#   #", "#   #", "#   #", " ####", "    #", " ### "),
    e = c("#### ", "#   #", "#####", "#    ", "#    ", "#   #", " ### ")
  )
  letters <- strsplit("nygen", "", fixed = TRUE)[[1]]
  pixel_rows <- vapply(
    seq_len(7L),
    function(row) {
      paste(
        vapply(
          letters,
          function(letter) glyphs[[letter]][[row]],
          character(1)
        ),
        collapse = " "
      )
    },
    character(1)
  )

  vapply(
    pixel_rows,
    function(row) {
      pixels <- strsplit(row, "", fixed = TRUE)[[1]]
      rendered <- paste0(
        ifelse(pixels == "#", "\u2588\u2588", "  "),
        collapse = ""
      )
      sub("[[:space:]]+$", "", rendered)
    },
    character(1)
  )
}

.CYTETYPE_NYGEN_BANNER <- unname(.create_nygen_banner())

.is_scalar_string <- function(value, allow_empty = FALSE) {
  is.character(value) &&
    length(value) == 1L &&
    !is.na(value) &&
    (allow_empty || nzchar(value))
}

.normalize_api_url <- function(api_url) {
  sub("/+$", "", trimws(api_url))
}

.validate_api_url <- function(value) {
  error_message <- "API URL must be an HTTP or HTTPS server origin"
  if (!.is_scalar_string(value)) {
    stop(error_message, call. = FALSE)
  }

  api_url <- .normalize_api_url(value)
  parsed <- tryCatch(
    httr2::url_parse(api_url),
    error = function(e) NULL
  )
  if (
    is.null(parsed) ||
      !parsed$scheme %in% c("http", "https") ||
      is.null(parsed$hostname) ||
      !nzchar(parsed$hostname) ||
      !is.null(parsed$username) ||
      !is.null(parsed$password) ||
      !is.null(parsed$query) ||
      !is.null(parsed$fragment) ||
      !is.null(parsed$path) && !parsed$path %in% c("", "/")
  ) {
    stop(error_message, call. = FALSE)
  }

  hostname <- tolower(parsed$hostname)
  if (parsed$scheme == "http" && !hostname %in% c("127.0.0.1", "localhost")) {
    stop("Non-local API URLs must use HTTPS", call. = FALSE)
  }

  api_url
}

.resolve_api_url <- function(api_url = NULL) {
  .validate_api_url(api_url %||% .get_default_api_url())
}

.credentials_path <- function() {
  config_home <- Sys.getenv("XDG_CONFIG_HOME", unset = "")
  if (nzchar(config_home)) {
    return(file.path(path.expand(config_home), "cytetype", "credentials.json"))
  }

  app_data <- Sys.getenv("APPDATA", unset = "")
  if (.Platform$OS.type == "windows" && nzchar(app_data)) {
    return(file.path(path.expand(app_data), "cytetype", "credentials.json"))
  }

  file.path(path.expand("~"), ".config", "cytetype", "credentials.json")
}

.validate_credentials <- function(credentials) {
  if (!is.list(credentials)) {
    stop("Credentials must be a JSON object", call. = FALSE)
  }

  required <- c("apiUrl", "apiToken", "tokenId", "userId", "email")
  missing <- required[!vapply(
    required,
    function(name) .is_scalar_string(credentials[[name]]),
    logical(1)
  )]
  if (length(missing) > 0L) {
    stop("Credentials are missing required fields", call. = FALSE)
  }

  dashboard_url <- credentials$dashboardUrl
  if (is.null(dashboard_url)) {
    dashboard_url <- .CYTETYPE_DEFAULT_DASHBOARD_URL
  }
  if (!.is_scalar_string(dashboard_url)) {
    stop("Credentials contain an invalid dashboard URL", call. = FALSE)
  }

  list(
    apiUrl = .validate_api_url(credentials$apiUrl),
    dashboardUrl = dashboard_url,
    apiToken = credentials$apiToken,
    tokenId = credentials$tokenId,
    userId = credentials$userId,
    email = credentials$email
  )
}

.credential_metadata <- function(credentials) {
  credentials <- .validate_credentials(credentials)
  credentials[c(
    "apiUrl",
    "dashboardUrl",
    "tokenId",
    "userId",
    "email"
  )]
}

.credentials_directory_owner <- function(directory) {
  file.info(directory)$uname
}

.current_effective_user <- function() {
  unname(Sys.info()[["effective_user"]])
}

.save_credentials <- function(credentials) {
  credentials <- .validate_credentials(credentials)
  path <- .credentials_path()
  directory <- dirname(path)

  if (!dir.exists(directory) &&
      !dir.create(directory, recursive = TRUE, mode = "0700", showWarnings = FALSE)) {
    stop("Could not create the CyteType credentials directory", call. = FALSE)
  }

  if (.Platform$OS.type == "unix") {
    directory_owner <- .credentials_directory_owner(directory)
    effective_user <- .current_effective_user()
    if (
      .is_scalar_string(directory_owner) &&
        .is_scalar_string(effective_user) &&
        !identical(directory_owner, effective_user)
    ) {
      stop(
        paste(
          "CyteType credentials directory is not owned by the current user:",
          directory
        ),
        call. = FALSE
      )
    }
    if (!isTRUE(Sys.chmod(directory, mode = "0700"))) {
      stop("Could not secure the CyteType credentials directory", call. = FALSE)
    }
  }

  temporary_path <- tempfile(pattern = ".credentials-", tmpdir = directory)
  on.exit(unlink(temporary_path, force = TRUE), add = TRUE)
  if (.Platform$OS.type == "unix") {
    old_umask <- Sys.umask(mode = "0077")
    tryCatch(
      jsonlite::write_json(
        credentials,
        path = temporary_path,
        auto_unbox = TRUE,
        pretty = FALSE
      ),
      finally = Sys.umask(old_umask)
    )
  } else {
    jsonlite::write_json(
      credentials,
      path = temporary_path,
      auto_unbox = TRUE,
      pretty = FALSE
    )
  }

  if (.Platform$OS.type == "unix" &&
      !isTRUE(Sys.chmod(temporary_path, mode = "0600"))) {
    stop("Could not secure the CyteType credentials file", call. = FALSE)
  }

  replaced <- file.rename(temporary_path, path)
  if (!replaced && .Platform$OS.type == "windows") {
    replaced <- file.copy(temporary_path, path, overwrite = TRUE)
    if (replaced) {
      unlink(temporary_path, force = TRUE)
    }
  }
  if (!replaced) {
    stop("Could not save the CyteType credentials file", call. = FALSE)
  }

  if (.Platform$OS.type == "unix" &&
      !isTRUE(Sys.chmod(path, mode = "0600"))) {
    stop("Could not secure the CyteType credentials file", call. = FALSE)
  }

  path
}

.load_credentials <- function(api_url = NULL) {
  path <- .credentials_path()
  if (!file.exists(path)) {
    return(NULL)
  }

  credentials <- tryCatch(
    {
      parsed <- jsonlite::fromJSON(path, simplifyVector = TRUE)
      .validate_credentials(parsed)
    },
    error = function(e) {
      stop(
        paste0("Invalid CyteType credentials file: ", path),
        call. = FALSE
      )
    }
  )

  if (!is.null(api_url)) {
    normalized_api_url <- .validate_api_url(api_url)
    if (!identical(credentials$apiUrl, normalized_api_url)) {
      return(NULL)
    }
  }

  credentials
}

.delete_credentials <- function() {
  path <- .credentials_path()
  if (!file.exists(path)) {
    return(FALSE)
  }
  if (unlink(path, force = TRUE) != 0L) {
    stop("Could not remove the local CyteType credentials", call. = FALSE)
  }
  TRUE
}

.url_origin <- function(value) {
  parsed <- httr2::url_parse(value)
  list(
    scheme = tolower(parsed$scheme %||% ""),
    hostname = tolower(parsed$hostname %||% ""),
    port = parsed$port,
    username = parsed$username,
    password = parsed$password
  )
}

.resolve_dashboard_url <- function(credentials) {
  credentials <- .validate_credentials(credentials)
  dashboard_origin <- tryCatch(
    .url_origin(credentials$dashboardUrl),
    error = function(e) NULL
  )
  api_origin <- .url_origin(credentials$apiUrl)
  if (is.null(dashboard_origin) || !identical(dashboard_origin, api_origin)) {
    return(.url_path(credentials$apiUrl, "dashboard"))
  }
  credentials$dashboardUrl
}

.new_authentication_error <- function(message, error_code, status_code = NULL) {
  structure(
    list(
      message = message,
      call = NULL,
      error_code = error_code,
      status_code = status_code
    ),
    class = c("cytetype_auth_error", "cytetype_api_error", "error", "condition")
  )
}

.stop_authentication <- function(
  message,
  error_code = "AUTHENTICATION_REQUIRED",
  status_code = NULL
) {
  stop(.new_authentication_error(message, error_code, status_code))
}

.resolve_auth_token <- function(api_url, auth_token = NULL) {
  api_url <- .validate_api_url(api_url)
  if (!is.null(auth_token)) {
    if (!.is_scalar_string(auth_token)) {
      stop("auth_token must be a non-empty character string", call. = FALSE)
    }
    return(auth_token)
  }

  credentials <- tryCatch(
    .load_credentials(api_url),
    error = function(e) {
      .stop_authentication(
        paste0(
          conditionMessage(e),
          ". Run LogoutCyteTypeR(), then SetupCyteTypeR()."
        )
      )
    }
  )
  if (is.null(credentials)) {
    .stop_authentication(
      "CyteType sign-in is required. Run SetupCyteTypeR() first."
    )
  }

  credentials$apiToken
}

.base64url_encode <- function(value) {
  encoded <- openssl::base64_encode(value, linebreaks = FALSE)
  encoded <- sub("=+$", "", encoded)
  chartr("+/", "-_", encoded)
}

.create_pkce_pair <- function() {
  verifier <- .base64url_encode(openssl::rand_bytes(64L))
  challenge <- .base64url_encode(
    openssl::sha256(charToRaw(verifier))
  )
  list(verifier = verifier, challenge = challenge)
}

.create_authorization_state <- function() {
  .base64url_encode(openssl::rand_bytes(32L))
}

.secure_string_equal <- function(left, right) {
  if (!.is_scalar_string(left, allow_empty = TRUE) ||
      !.is_scalar_string(right, allow_empty = TRUE)) {
    return(FALSE)
  }

  left_raw <- charToRaw(enc2utf8(left))
  right_raw <- charToRaw(enc2utf8(right))
  if (length(left_raw) != length(right_raw)) {
    return(FALSE)
  }

  difference <- 0L
  for (index in seq_along(left_raw)) {
    difference <- bitwOr(
      difference,
      bitwXor(
        as.integer(left_raw[[index]]),
        as.integer(right_raw[[index]])
      )
    )
  }
  identical(difference, 0L)
}

.parse_callback_query <- function(query_string) {
  if (!.is_scalar_string(query_string)) {
    return(list())
  }
  query_string <- sub("^\\?", "", query_string)
  if (!nzchar(query_string)) {
    return(list())
  }
  parsed <- httr2::url_parse(
    paste0("http://127.0.0.1/?", query_string)
  )
  parsed$query %||% list()
}

.auth_error_message <- function(response, fallback) {
  body <- tryCatch(
    httr2::resp_body_json(response, simplifyVector = TRUE),
    error = function(e) NULL
  )
  if (!is.list(body)) {
    return(fallback)
  }

  detail <- body$detail
  if (is.list(detail) && .is_scalar_string(detail$message)) {
    return(detail$message)
  }
  if (.is_scalar_string(detail)) {
    return(detail)
  }
  if (.is_scalar_string(body$message)) {
    return(body$message)
  }
  fallback
}

.perform_auth_request <- function(request, fallback) {
  response <- tryCatch(
    request |>
      httr2::req_error(is_error = function(resp) FALSE) |>
      httr2::req_perform(),
    error = function(e) {
      .stop_authentication(fallback, "NETWORK_ERROR")
    }
  )

  status <- httr2::resp_status(response)
  if (status < 200L || status >= 300L) {
    error_code <- if (status == 401L) {
      "INVALID_TOKEN"
    } else {
      "AUTHENTICATION_FAILED"
    }
    body <- tryCatch(
      httr2::resp_body_json(response, simplifyVector = TRUE),
      error = function(e) NULL
    )
    if (is.list(body) && is.list(body$detail) &&
        .is_scalar_string(body$detail$error_code)) {
      error_code <- body$detail$error_code
    }
    .stop_authentication(
      .auth_error_message(response, fallback),
      error_code
    )
  }
  response
}

.exchange_cli_token <- function(api_url, code, verifier, redirect_uri) {
  response <- .perform_auth_request(
    httr2::request(.url_path(api_url, "auth", "cli", "token")) |>
      httr2::req_method("POST") |>
      httr2::req_body_json(
        list(
          code = code,
          codeVerifier = verifier,
          redirectUri = redirect_uri
        ),
        auto_unbox = TRUE
      ) |>
      httr2::req_timeout(30),
    "Could not exchange the authorization code"
  )

  data <- tryCatch(
    httr2::resp_body_json(response, simplifyVector = TRUE),
    error = function(e) NULL
  )
  tryCatch(
    .validate_credentials(c(
      list(apiUrl = api_url),
      data
    )),
    error = function(e) {
      .stop_authentication(
        "Server returned invalid CLI credentials",
        "INVALID_RESPONSE"
      )
    }
  )
}

.html_escape <- function(value) {
  value <- gsub("&", "&amp;", value, fixed = TRUE)
  value <- gsub("<", "&lt;", value, fixed = TRUE)
  value <- gsub(">", "&gt;", value, fixed = TRUE)
  value <- gsub('"', "&quot;", value, fixed = TRUE)
  gsub("'", "&#39;", value, fixed = TRUE)
}

.replace_template_placeholder <- function(
  template,
  placeholder,
  value,
  expected_count
) {
  matches <- gregexpr(placeholder, template, fixed = TRUE)[[1]]
  count <- if (identical(matches, -1L)) 0L else length(matches)
  if (!identical(count, expected_count)) {
    stop(
      paste0("Invalid callback template placeholder: ", placeholder),
      call. = FALSE
    )
  }

  if (count == 0L) {
    return(template)
  }

  match_lengths <- attr(matches, "match.length")
  pieces <- character(count * 2L + 1L)
  source_start <- 1L
  piece_index <- 1L
  for (index in seq_len(count)) {
    pieces[[piece_index]] <- substr(
      template,
      source_start,
      matches[[index]] - 1L
    )
    pieces[[piece_index + 1L]] <- value
    source_start <- matches[[index]] + match_lengths[[index]]
    piece_index <- piece_index + 2L
  }
  pieces[[piece_index]] <- substr(
    template,
    source_start,
    nchar(template)
  )
  paste0(pieces, collapse = "")
}

.load_callback_template <- function() {
  path <- system.file(
    "templates",
    "cli_callback.html",
    package = "CyteTypeR",
    mustWork = TRUE
  )
  paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

.render_callback_page <- function(message, credentials = NULL) {
  substitutions <- list(
    "__META_REFRESH__" = "",
    "__STATE_CLASS__" = "error",
    "__ICON__" = "!",
    "__MESSAGE__" = .html_escape(message),
    "__SUCCESS_HIDDEN__" = "hidden",
    "__ERROR_HIDDEN__" = "",
    "__EMAIL__" = "",
    "__DASHBOARD_URL__" = ""
  )

  if (!is.null(credentials)) {
    dashboard_url <- .html_escape(
      .resolve_dashboard_url(credentials)
    )
    substitutions[["__META_REFRESH__"]] <- paste0(
      '<meta http-equiv="refresh" content="5;url=',
      dashboard_url,
      '">'
    )
    substitutions[["__STATE_CLASS__"]] <- "success"
    substitutions[["__ICON__"]] <- "\u2713"
    substitutions[["__SUCCESS_HIDDEN__"]] <- ""
    substitutions[["__ERROR_HIDDEN__"]] <- "hidden"
    substitutions[["__EMAIL__"]] <- .html_escape(credentials$email)
    substitutions[["__DASHBOARD_URL__"]] <- dashboard_url
  }

  expected_counts <- c(
    "__META_REFRESH__" = 1L,
    "__STATE_CLASS__" = 1L,
    "__ICON__" = 1L,
    "__MESSAGE__" = 2L,
    "__SUCCESS_HIDDEN__" = 1L,
    "__ERROR_HIDDEN__" = 1L,
    "__EMAIL__" = 1L,
    "__DASHBOARD_URL__" = 1L
  )
  template <- .load_callback_template()
  for (placeholder in names(substitutions)) {
    template <- .replace_template_placeholder(
      template,
      placeholder,
      substitutions[[placeholder]],
      expected_counts[[placeholder]]
    )
  }
  template
}

.callback_response <- function(status, message, credentials = NULL) {
  list(
    status = as.integer(status),
    headers = list(
      "Content-Type" = "text/html; charset=utf-8",
      "Cache-Control" = "no-store",
      "Referrer-Policy" = "no-referrer",
      "X-Content-Type-Options" = "nosniff",
      "X-Frame-Options" = "DENY",
      "Content-Security-Policy" = paste(
        "default-src 'none'; style-src 'unsafe-inline';",
        "base-uri 'none'; form-action 'none'; frame-ancestors 'none'"
      )
    ),
    body = .render_callback_page(message, credentials)
  )
}

.create_callback_app <- function(
  api_url,
  state,
  verifier,
  redirect_uri,
  result
) {
  function(request) {
    if (!identical(request$PATH_INFO, "/callback")) {
      return(.callback_response(404L, "Unknown callback path."))
    }

    query <- tryCatch(
      .parse_callback_query(request$QUERY_STRING),
      error = function(e) list()
    )
    returned_state <- query$state %||% ""
    if (!.is_scalar_string(returned_state, allow_empty = TRUE) ||
        !.secure_string_equal(returned_state, state)) {
      return(.callback_response(400L, "Authorization state did not match."))
    }

    callback_error <- query$error %||% ""
    if (.is_scalar_string(callback_error)) {
      result$value <- .new_authentication_error(
        "CyteType authorization was denied",
        "ACCESS_DENIED"
      )
      result$complete <- TRUE
      return(.callback_response(400L, "CyteType authorization failed."))
    }

    code <- query$code %||% ""
    if (!.is_scalar_string(code)) {
      return(.callback_response(400L, "Authorization code was missing."))
    }

    credentials <- tryCatch(
      .exchange_cli_token(api_url, code, verifier, redirect_uri),
      error = function(e) e
    )
    result$value <- credentials
    result$complete <- TRUE
    if (inherits(credentials, "condition")) {
      return(.callback_response(400L, "CyteType setup failed."))
    }

    .callback_response(
      200L,
      "CyteType setup is complete.",
      credentials
    )
  }
}

.is_wsl_environment <- function() {
  Sys.info()[["sysname"]] == "Linux" &&
    (nzchar(Sys.getenv("WSL_DISTRO_NAME", unset = "")) ||
       nzchar(Sys.getenv("WSL_INTEROP", unset = "")))
}

.system_name <- function() {
  unname(Sys.info()[["sysname"]])
}

.find_browser_launcher <- function(command) {
  unname(Sys.which(command))
}

.launch_browser_process <- function(command, args) {
  tryCatch(
    {
      status <- system2(
        command,
        args = args,
        stdout = FALSE,
        stderr = FALSE,
        wait = FALSE
      )
      is.null(status) || identical(status, 0L)
    },
    error = function(e) FALSE
  )
}

.browse_url_silently <- function(url) {
  tryCatch(
    {
      suppressWarnings(suppressMessages(utils::browseURL(url)))
      TRUE
    },
    error = function(e) FALSE
  )
}

.open_browser_silently <- function(url) {
  if (.is_wsl_environment()) {
    launcher <- .find_browser_launcher("rundll32.exe")
    if (!nzchar(launcher)) {
      return(FALSE)
    }
    return(.launch_browser_process(
      launcher,
      c("url.dll,FileProtocolHandler", shQuote(url))
    ))
  }

  system_name <- .system_name()
  if (identical(system_name, "Linux")) {
    launcher <- .find_browser_launcher("xdg-open")
    if (!nzchar(launcher)) {
      return(FALSE)
    }
    return(.launch_browser_process(launcher, shQuote(url)))
  }
  if (identical(system_name, "Darwin")) {
    launcher <- .find_browser_launcher("open")
    if (!nzchar(launcher)) {
      return(FALSE)
    }
    return(.launch_browser_process(launcher, shQuote(url)))
  }

  .browse_url_silently(url)
}

.authorization_url <- function(api_url, redirect_uri, state, challenge) {
  url <- httr2::url_parse(
    .url_path(api_url, "auth", "cli", "authorize")
  )
  url$query <- list(
    redirectUri = redirect_uri,
    state = state,
    codeChallenge = challenge
  )
  httr2::url_build(url)
}

.start_callback_server <- function(app_factory, max_attempts = 10L) {
  for (attempt in seq_len(max_attempts)) {
    port <- httpuv::randomPort(
      min = 1024L,
      max = 49151L,
      host = "127.0.0.1"
    )
    app <- app_factory(port)
    server <- tryCatch(
      httpuv::startServer(
        "127.0.0.1",
        port,
        list(call = app),
        quiet = TRUE
      ),
      error = function(e) NULL
    )
    if (!is.null(server)) {
      return(list(server = server, port = port))
    }
  }

  .stop_authentication(
    "Could not start the local CyteType callback server",
    "CALLBACK_SERVER_ERROR"
  )
}

.stop_callback_server <- function(server) {
  httpuv::stopServer(server)
}

.service_callback_server <- function(timeout_ms = 100) {
  httpuv::service(timeoutMs = timeout_ms)
}

.monotonic_time <- function() {
  unname(proc.time()[["elapsed"]])
}

.print_setup_banner <- function() {
  cat(
    "\n",
    paste(.CYTETYPE_NYGEN_BANNER, collapse = "\n"),
    "\n\nNygen Analytics:      ",
    cli::col_blue("https://nygen.io"),
    "\n\n",
    sep = ""
  )
}

.run_browser_setup <- function(
  api_url,
  timeout_seconds = .CYTETYPE_SETUP_TIMEOUT_SECONDS
) {
  state <- .create_authorization_state()
  pkce <- .create_pkce_pair()
  result <- new.env(parent = emptyenv())
  result$complete <- FALSE
  result$value <- NULL
  callback_server <- .start_callback_server(
    function(port) {
      redirect_uri <- paste0(
        "http://127.0.0.1:",
        port,
        "/callback"
      )
      .create_callback_app(
        api_url,
        state,
        pkce$verifier,
        redirect_uri,
        result
      )
    }
  )
  on.exit(.stop_callback_server(callback_server$server), add = TRUE)
  redirect_uri <- paste0(
    "http://127.0.0.1:",
    callback_server$port,
    "/callback"
  )

  authorize_url <- .authorization_url(
    api_url,
    redirect_uri,
    state,
    pkce$challenge
  )
  cli::cli_inform(c(
    "Opening CyteType sign-in in your browser.",
    "If it does not open automatically, use this URL:",
    "{.url {authorize_url}}"
  ))
  if (!.open_browser_silently(authorize_url)) {
    cli::cli_warn("Could not launch a web browser.")
  }

  started_at <- .monotonic_time()
  while (
    !isTRUE(result$complete) &&
      .monotonic_time() - started_at < timeout_seconds
  ) {
    .service_callback_server(timeout_ms = 100)
  }

  if (!isTRUE(result$complete)) {
    .stop_authentication("CyteType setup timed out", "TIMEOUT")
  }
  .service_callback_server(timeout_ms = 100)
  if (inherits(result$value, "condition")) {
    stop(result$value)
  }
  result$value
}

.is_interactive <- function() {
  interactive()
}

# First line of the process's standard input (not R's console), or
# character(0) at the end of input.
.read_stdin_line <- function() {
  con <- file("stdin")
  on.exit(close(con), add = TRUE)
  readLines(con, n = 1L, warn = FALSE)
}

.stdin_is_terminal <- function() {
  if (!identical(.Platform$OS.type, "unix")) {
    return(FALSE)
  }
  status <- suppressWarnings(system(
    "test -t 0",
    ignore.stdout = TRUE,
    ignore.stderr = TRUE
  ))
  identical(as.integer(status), 0L)
}

.set_terminal_echo <- function(enabled) {
  status <- suppressWarnings(system(
    if (enabled) "stty echo" else "stty -echo",
    ignore.stdout = TRUE,
    ignore.stderr = TRUE
  ))
  identical(as.integer(status), 0L)
}

# Read an API key. Rscript, and so the cytetyper command, is not interactive,
# where askpass returns NULL: read the key from standard input instead. When
# the input is a terminal, `stty` hides the key on Linux and macOS; Windows has
# no `stty`, so the key is shown there.
.prompt_api_key <- function(prompt = "API key: ") {
  if (.is_interactive()) {
    return(askpass::askpass(prompt))
  }
  cat(prompt, file = stderr())
  terminal <- .stdin_is_terminal()
  echo_disabled <- terminal && .set_terminal_echo(FALSE)
  on.exit(
    {
      if (echo_disabled) {
        .set_terminal_echo(TRUE)
      }
      if (echo_disabled || !terminal) {
        cat("\n", file = stderr())
      }
    },
    add = TRUE
  )
  line <- .read_stdin_line()
  if (length(line) == 0L || is.na(line[[1L]])) {
    return(NULL)
  }
  line[[1L]]
}

#' Set up CyteType authentication
#'
#' Validates matching saved credentials. When none are available, opens
#' CyteType sign-in in a browser and stores the resulting personal API key.
#'
#' @param api_url Optional CyteType server origin. When `NULL`, uses
#'   `CYTETYPE_API_URL` or `https://cytetype.nygen.io`.
#' @param force Whether to skip saved credential validation and authenticate
#'   with a new key.
#' @return The saved credential metadata, invisibly.
#' @details
#' The browser flow verifies the user's email with a one-time code and returns
#' to a local callback server. CyteTypeR and the Python client use the same
#' credentials file. For development, pass the development server origin as
#' `api_url`. If a saved key is invalid or inactive, use `force = TRUE`.
#' @examples
#' \dontrun{
#' SetupCyteTypeR()
#' SetupCyteTypeR(force = TRUE)
#' SetupCyteTypeR(api_url = "<URL>")
#' }
#' @export
SetupCyteTypeR <- function(api_url = NULL, force = FALSE) {
  if (!is.logical(force) || length(force) != 1L || is.na(force)) {
    stop("force must be TRUE or FALSE", call. = FALSE)
  }
  api_url <- .resolve_api_url(api_url)
  .print_setup_banner()
  existing <- if (force) NULL else .load_credentials(api_url)
  if (!is.null(existing)) {
    credentials <- tryCatch(
      {
        response <- .perform_auth_request(
          httr2::request(.url_path(
            api_url,
            "auth",
            "cli",
            "credentials"
          )) |>
            httr2::req_auth_bearer_token(existing$apiToken) |>
            httr2::req_timeout(30),
          "Could not validate the saved API key"
        )
        data <- tryCatch(
          httr2::resp_body_json(response, simplifyVector = TRUE),
          error = function(e) NULL
        )
        tryCatch(
          .validate_credentials(c(
            list(apiUrl = api_url, apiToken = existing$apiToken),
            data
          )),
          error = function(e) {
            .stop_authentication(
              "Server returned invalid CLI credentials",
              "INVALID_RESPONSE"
            )
          }
        )
      },
      cytetype_auth_error = function(error) {
        if (error$error_code %in% c("INVALID_TOKEN", "TOKEN_INACTIVE")) {
          .stop_authentication(
            paste0(
              conditionMessage(error),
              ". Run SetupCyteTypeR(force = TRUE) or ",
              "`cytetyper setup --force` to re-authenticate with a new key"
            ),
            error$error_code
          )
        }
        stop(error)
      }
    )
    .save_credentials(credentials)
    dashboard_url <- .resolve_dashboard_url(credentials)
    cli::cli_inform(c(
      "CyteType is already configured for {credentials$email}.",
      "Dashboard: {.url {dashboard_url}}"
    ))
    return(invisible(.credential_metadata(credentials)))
  }

  credentials <- .run_browser_setup(api_url)
  path <- .save_credentials(credentials)
  dashboard_url <- .resolve_dashboard_url(credentials)
  cli::cli_inform(c(
    "CyteType API key saved for {credentials$email} in {path}.",
    "Dashboard: {.url {dashboard_url}}"
  ))
  invisible(.credential_metadata(credentials))
}

#' Save an existing CyteType API key
#'
#' Validates an API key and stores it for later CyteTypeR requests.
#'
#' @param api_token Optional API key. When omitted, a hidden prompt is shown
#'   in an interactive session. Otherwise the key is read from the first line
#'   of standard input, so `cytetyper login` and scripts can pipe it in. Typed
#'   into a terminal, the key is hidden on Linux and macOS but shown on
#'   Windows.
#' @param api_url Optional CyteType server origin. When `NULL`, uses
#'   `CYTETYPE_API_URL` or `https://cytetype.nygen.io`.
#' @return The saved credential metadata, invisibly.
#' @examples
#' \dontrun{
#' LoginCyteTypeR()
#' LoginCyteTypeR(api_url = "<URL>")
#' }
#' @export
LoginCyteTypeR <- function(api_token = NULL, api_url = NULL) {
  api_url <- .resolve_api_url(api_url)
  if (is.null(api_token)) {
    api_token <- .prompt_api_key("API key: ")
  }
  if (!.is_scalar_string(api_token) || !nzchar(trimws(api_token))) {
    stop("API key is required", call. = FALSE)
  }
  api_token <- trimws(api_token)

  response <- .perform_auth_request(
    httr2::request(.url_path(api_url, "auth", "cli", "credentials")) |>
      httr2::req_auth_bearer_token(api_token) |>
      httr2::req_timeout(30),
    "Could not validate the API key"
  )
  data <- tryCatch(
    httr2::resp_body_json(response, simplifyVector = TRUE),
    error = function(e) NULL
  )
  credentials <- tryCatch(
    .validate_credentials(c(
      list(apiUrl = api_url, apiToken = api_token),
      data
    )),
    error = function(e) {
      .stop_authentication(
        "Server returned invalid CLI credentials",
        "INVALID_RESPONSE"
      )
    }
  )

  path <- .save_credentials(credentials)
  dashboard_url <- .resolve_dashboard_url(credentials)
  cli::cli_inform(c(
    "Signed in as {credentials$email}.",
    "CyteType API key saved in {path}.",
    "Dashboard: {.url {dashboard_url}}"
  ))
  invisible(.credential_metadata(credentials))
}

#' Remove local CyteType credentials
#'
#' @return `TRUE` when credentials were removed, otherwise `FALSE`, invisibly.
#' @details
#' This removes only the local credentials file. Revoke an API key from the
#' dashboard when it must also be invalidated on the server.
#' @examples
#' \dontrun{
#' LogoutCyteTypeR()
#' }
#' @export
LogoutCyteTypeR <- function() {
  removed <- .delete_credentials()
  if (removed) {
    cli::cli_inform(
      "Local CyteType API key removed. Revoke it from the dashboard if needed."
    )
  } else {
    cli::cli_inform("No local CyteType API key was found.")
  }
  invisible(removed)
}

#' Open the CyteType dashboard
#'
#' @return The dashboard URL, invisibly.
#' @details
#' Uses the dashboard associated with saved credentials. When credentials are
#' absent, opens the production dashboard at
#' `https://cytetype.nygen.io/dashboard`.
#' @examples
#' \dontrun{
#' OpenCyteTypeDashboard()
#' }
#' @export
OpenCyteTypeDashboard <- function() {
  credentials <- .load_credentials()
  target <- if (is.null(credentials)) {
    .CYTETYPE_DEFAULT_DASHBOARD_URL
  } else {
    .resolve_dashboard_url(credentials)
  }
  cli::cli_inform("{.url {target}}")
  .open_browser_silently(target)
  invisible(target)
}

#' Open a CyteType job
#'
#' @param job_id CyteType job identifier.
#' @param api_url Optional CyteType server origin. When `NULL`, uses the origin
#'   from saved credentials or the configured default.
#' @return The sign-in URL for the job, invisibly.
#' @examples
#' \dontrun{
#' ViewCyteTypeJob("<JOB_ID>")
#' ViewCyteTypeJob("<JOB_ID>", api_url = "<URL>")
#' }
#' @export
ViewCyteTypeJob <- function(job_id, api_url = NULL) {
  if (!.is_scalar_string(job_id)) {
    stop("job_id must be a non-empty character string", call. = FALSE)
  }

  credentials <- .load_credentials()
  if (is.null(api_url)) {
    api_url <- if (is.null(credentials)) {
      .get_default_api_url()
    } else {
      credentials$apiUrl
    }
  }
  api_url <- .validate_api_url(api_url)

  redirect <- paste0(
    "/report/",
    utils::URLencode(job_id, reserved = TRUE)
  )
  target_parts <- httr2::url_parse(.url_path(api_url, "login"))
  target_parts$query <- list(redirect = redirect)
  target <- httr2::url_build(target_parts)
  cli::cli_inform("{.url {target}}")
  .open_browser_silently(target)
  invisible(target)
}
