# Null-coalescing operator - internal use only
`%||%` <- function(x, y) if (is.null(x)) y else x

# Load defaults
.onLoad <- function(libname, pkgname) {
  # Set default configuration options
  options(
    cytetype.default.api.url = "https://prod.cytetype.nygen.io",
    cytetype.default.poll.interval = 10L,
    cytetype.default.timeout = 7200L
  )
  # Configure logging if available
  if (requireNamespace("cli", quietly = TRUE)) {
    options(cytetype.use.cli = TRUE)
  }

}

# check if version installed is the latest github release
.onAttach <- function(libname, pkgname) {
  current <- as.character(utils::packageVersion(pkgname))

  latest <- tryCatch({
    url <- "https://api.github.com/repos/NygenAnalytics/CyteTypeR/releases/latest"
    response <- httr2::request(url) |>
      httr2::req_timeout(2) |>
      httr2::req_perform()
    tag <- httr2::resp_body_json(response)$tag_name
    sub("^v", "", tag)
  }, error = function(e) NULL)

  if (!is.null(latest)) {
    tryCatch({
      if (package_version(latest) > package_version(current)) {
        packageStartupMessage(
          "A newer version of CyteTypeR is available (", current, " -> ", latest, "). ",
          "Run remotes::install_github('NygenAnalytics/CyteTypeR') to update."
        )
      }
    }, error = function(e) NULL)
  }
}

# Get Default API URL
.get_default_api_url <- function() {
  getOption("cytetype.default.api.url", "https://prod.cytetype.nygen.io")
}

# Get Default Poll Interval
.get_default_poll_interval <- function() {
  getOption("cytetype.default.poll.interval", 10L)
}

# Get Default Timeout
.get_default_timeout <- function() {
  getOption("cytetype.default.timeout", 7200L)
}

utils::globalVariables(c(
  ".",           # magrittr pronoun
  "gene",        # column name in marker_table
  "cluster",     # column name in marker_table
  "avg_log2FC"   # column name in marker_table
))
