#' Custom API Error
#'
#' @param message Error message
#' @param call Call that generated the error
#' @return An error object of class cytetype_api_error
#' @export
cytetype_api_error <- function(message, call = NULL) {
  structure(
    list(message = message, call = call),
    class = c("cytetype_api_error", "error", "condition")
  )
}

#' @export
print.cytetype_api_error <- function(x, ...) {
  cat("CyteType API Error: ", x$message, "\n")
}

.parse_server_error <- function(resp) {
  body <- tryCatch(httr2::resp_body_json(resp), error = function(e) NULL)
  if (is.null(body)) return(NULL)
  if ("detail" %in% names(body) && is.list(body$detail)) body <- body$detail
  if (!is.null(body$error_code) && !is.null(body$message)) {
    return(list(error_code = body$error_code, message = body$message))
  }
  NULL
}

.stop_if_rate_limited <- function(e) {
  if (inherits(e, "httr2_http") && !is.null(e$resp)) {
    parsed <- .parse_server_error(e$resp)
    if (!is.null(parsed) && parsed$error_code == "RATE_LIMIT_EXCEEDED") {
      stop(
        "Rate limit exceeded: ", parsed$message,
        "\nUse your own LLM API key via llm_configs to bypass free-tier limits, ",
        "or wait before retrying.",
        call. = FALSE
      )
    }
  }
}
