# Load defaults
.onLoad <- function(libname, pkgname) {
  # Set default configuration options
  options(
    cytetype.default.api.url = "https://nygen-labs--cytetype-api.modal.run",
    cytetype.default.poll.interval = 10L,
    cytetype.default.timeout = 7200L
  )
  # Configure logging if available
  if (requireNamespace("cli", quietly = TRUE)) {
    options(cytetype.use.cli = TRUE)
  }
}

# Get Default API URL
.get_default_api_url <- function() {
  getOption("cytetype.default.api.url", "https://nygen-labs--cytetype-api.modal.run")
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
