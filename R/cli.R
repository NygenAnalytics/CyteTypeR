.CYTETYPER_PROGRAM_NAME <- "cytetyper"

.cytetyper_help <- function() {
  paste(
    "Authenticate with CyteType and open your jobs.",
    "",
    "Usage:",
    "  cytetyper setup [--api-url URL] [--force]",
    "  cytetyper get-key [--api-url URL] [--force]",
    "  cytetyper login [--api-url URL]",
    "  cytetyper logout",
    "  cytetyper dashboard",
    "  cytetyper view JOB_ID [--api-url URL]",
    "  cytetyper --version",
    "",
    "Commands:",
    "  setup       Sign in and save a personal API key.",
    "  get-key     Alias for setup.",
    "  login       Save and validate an existing API key.",
    "  logout      Remove the locally saved API key.",
    "  dashboard   Open the CyteType dashboard.",
    "  view        Open a CyteType job report.",
    "",
    "Options:",
    "  --api-url URL  CyteType server origin. Defaults to",
    "                 CYTETYPE_API_URL or the production server.",
    "  --force        Re-authenticate without validating saved credentials.",
    "  -h, --help     Show help.",
    "  --version      Show the installed CyteTypeR version.",
    sep = "\n"
  )
}

.cytetyper_command_help <- function(command) {
  switch(
    command,
    setup = paste(
      "Usage: cytetyper setup [--api-url URL] [--force]",
      "",
      "Sign in through the browser and save a personal API key.",
      "",
      "--force re-authenticates without validating saved credentials.",
      sep = "\n"
    ),
    `get-key` = paste(
      "Usage: cytetyper get-key [--api-url URL] [--force]",
      "",
      "Alias for cytetyper setup.",
      sep = "\n"
    ),
    login = paste(
      "Usage: cytetyper login [--api-url URL]",
      "",
      "Prompt for, validate, and save an existing API key. The key is read",
      "from standard input and hidden while you type on Linux and macOS (not",
      "on Windows). It can also be piped in, for example:",
      "printf '%s\\n' \"$CYTETYPE_API_KEY\" | cytetyper login",
      sep = "\n"
    ),
    logout = paste(
      "Usage: cytetyper logout",
      "",
      "Remove the locally saved API key.",
      sep = "\n"
    ),
    dashboard = paste(
      "Usage: cytetyper dashboard",
      "",
      "Open the CyteType dashboard.",
      sep = "\n"
    ),
    view = paste(
      "Usage: cytetyper view JOB_ID [--api-url URL]",
      "",
      "Open a CyteType job report.",
      sep = "\n"
    ),
    .cytetyper_help()
  )
}

.cytetyper_stdout <- function(text) {
  cat(text, "\n", sep = "", file = stdout())
}

.cytetyper_stderr <- function(text) {
  cat(text, "\n", sep = "", file = stderr())
}

.parse_cytetyper_command <- function(
  args,
  command,
  allow_api_url = FALSE,
  allow_force = FALSE,
  positional_count = 0L
) {
  api_url <- NULL
  force <- FALSE
  positionals <- character()
  show_help <- FALSE
  index <- 1L

  while (index <= length(args)) {
    argument <- args[[index]]
    if (argument %in% c("-h", "--help")) {
      show_help <- TRUE
      index <- index + 1L
      next
    }

    if (identical(argument, "--force")) {
      if (!allow_force) {
        stop(
          paste0("Unknown option for ", command, ": --force"),
          call. = FALSE
        )
      }
      if (force) {
        stop("--force may only be provided once", call. = FALSE)
      }
      force <- TRUE
      index <- index + 1L
      next
    }

    if (identical(argument, "--api-url")) {
      if (!allow_api_url) {
        stop(
          paste0("Unknown option for ", command, ": --api-url"),
          call. = FALSE
        )
      }
      if (!is.null(api_url)) {
        stop("--api-url may only be provided once", call. = FALSE)
      }
      index <- index + 1L
      if (index > length(args) || !nzchar(args[[index]])) {
        stop("--api-url requires a value", call. = FALSE)
      }
      api_url <- args[[index]]
      index <- index + 1L
      next
    }

    if (startsWith(argument, "--api-url=")) {
      if (!allow_api_url) {
        stop(
          paste0("Unknown option for ", command, ": --api-url"),
          call. = FALSE
        )
      }
      if (!is.null(api_url)) {
        stop("--api-url may only be provided once", call. = FALSE)
      }
      api_url <- substring(argument, nchar("--api-url=") + 1L)
      if (!nzchar(api_url)) {
        stop("--api-url requires a value", call. = FALSE)
      }
      index <- index + 1L
      next
    }

    if (startsWith(argument, "-")) {
      stop(
        paste0("Unknown option for ", command, ": ", argument),
        call. = FALSE
      )
    }

    positionals <- c(positionals, argument)
    index <- index + 1L
  }

  if (!show_help && length(positionals) != positional_count) {
    expected <- if (positional_count == 0L) {
      "no positional arguments"
    } else {
      paste0(positional_count, " positional argument")
    }
    stop(
      paste0(command, " requires ", expected),
      call. = FALSE
    )
  }

  list(
    apiUrl = api_url,
    force = force,
    positionals = positionals,
    showHelp = show_help
  )
}

.cytetyper_package_version <- function() {
  as.character(utils::packageVersion("CyteTypeR"))
}

.dispatch_cytetyper_command <- function(args) {
  if (length(args) == 0L || identical(args, "--help") ||
      identical(args, "-h")) {
    .cytetyper_stdout(.cytetyper_help())
    return(0L)
  }
  if (identical(args, "--version")) {
    .cytetyper_stdout(
      paste(.CYTETYPER_PROGRAM_NAME, .cytetyper_package_version())
    )
    return(0L)
  }

  command <- args[[1]]
  command_args <- args[-1]
  supported_commands <- c(
    "setup",
    "get-key",
    "login",
    "logout",
    "dashboard",
    "view"
  )
  if (!command %in% supported_commands) {
    stop(paste0("Unknown command: ", command), call. = FALSE)
  }

  allows_api_url <- command %in% c("setup", "get-key", "login", "view")
  allows_force <- command %in% c("setup", "get-key")
  positional_count <- if (identical(command, "view")) 1L else 0L
  parsed <- .parse_cytetyper_command(
    command_args,
    command,
    allow_api_url = allows_api_url,
    allow_force = allows_force,
    positional_count = positional_count
  )
  if (parsed$showHelp) {
    .cytetyper_stdout(.cytetyper_command_help(command))
    return(0L)
  }

  if (command %in% c("setup", "get-key")) {
    SetupCyteTypeR(api_url = parsed$apiUrl, force = parsed$force)
  } else if (identical(command, "login")) {
    LoginCyteTypeR(api_url = parsed$apiUrl)
  } else if (identical(command, "logout")) {
    LogoutCyteTypeR()
  } else if (identical(command, "dashboard")) {
    OpenCyteTypeDashboard()
  } else if (identical(command, "view")) {
    ViewCyteTypeJob(
      job_id = parsed$positionals[[1]],
      api_url = parsed$apiUrl
    )
  }

  0L
}

.cytetyper_cli_main <- function(args = commandArgs(trailingOnly = TRUE)) {
  tryCatch(
    .dispatch_cytetyper_command(args),
    interrupt = function(error) {
      .cytetyper_stderr("Cancelled.")
      130L
    },
    error = function(error) {
      .cytetyper_stderr(paste0("Error: ", conditionMessage(error)))
      1L
    }
  )
}

.cytetyper_os_type <- function() {
  .Platform$OS.type
}

.cytetyper_launcher_name <- function(os_type = .cytetyper_os_type()) {
  if (identical(os_type, "windows")) {
    "cytetyper.cmd"
  } else {
    "cytetyper"
  }
}

.cytetyper_rscript_path <- function() {
  path <- unname(Sys.which("Rscript"))
  if (!.is_scalar_string(path)) {
    stop("Could not locate Rscript on PATH", call. = FALSE)
  }
  normalizePath(path, mustWork = TRUE)
}

.cytetyper_launcher_source <- function(
  launcher_name = .cytetyper_launcher_name()
) {
  system.file(
    "exec",
    launcher_name,
    package = "CyteTypeR",
    mustWork = TRUE
  )
}

#' Install the cytetyper terminal command
#'
#' @param destination Directory in which to install the command. Defaults to
#'   the directory containing the active `Rscript` executable. The default is
#'   recommended because the launcher uses the adjacent `Rscript` executable.
#' @param overwrite Whether to replace an existing command.
#' @return The installed command path, invisibly.
#' @details
#' After installation, run `cytetyper setup` in a terminal to authenticate
#' against `https://cytetype.nygen.io`. During development, use
#' `cytetyper setup --api-url "<URL>"`.
#' @examples
#' \dontrun{
#' InstallCyteTypeRCli()
#' }
#' @export
InstallCyteTypeRCli <- function(destination = NULL, overwrite = FALSE) {
  if (is.null(destination)) {
    destination <- dirname(.cytetyper_rscript_path())
  }
  if (!.is_scalar_string(destination)) {
    stop("destination must be a directory path", call. = FALSE)
  }
  destination <- normalizePath(destination, mustWork = FALSE)
  if (!dir.exists(destination)) {
    stop("CLI destination directory does not exist", call. = FALSE)
  }
  if (!is.logical(overwrite) || length(overwrite) != 1L ||
      is.na(overwrite)) {
    stop("overwrite must be TRUE or FALSE", call. = FALSE)
  }

  launcher_name <- .cytetyper_launcher_name()
  source <- .cytetyper_launcher_source(launcher_name)
  target <- file.path(destination, launcher_name)
  if (file.exists(target) && !overwrite) {
    stop(
      paste0(
        "The cytetyper command already exists at ",
        target,
        ". Set overwrite = TRUE to replace it."
      ),
      call. = FALSE
    )
  }

  copied <- suppressWarnings(file.copy(
    source,
    target,
    overwrite = overwrite,
    copy.mode = TRUE,
    copy.date = FALSE
  ))
  if (!copied) {
    stop(
      paste0(
        "Could not install cytetyper in ",
        destination,
        ". Check that the directory is writable."
      ),
      call. = FALSE
    )
  }

  if (!identical(.cytetyper_os_type(), "windows")) {
    Sys.chmod(target, mode = "0755")
    if (file.access(target, mode = 1L) != 0L) {
      unlink(target, force = TRUE)
      stop("Could not make the cytetyper command executable", call. = FALSE)
    }
  }

  cli::cli_inform("Installed {.file {target}}.")
  invisible(target)
}
