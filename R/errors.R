#' Custom API Error
#'
#' @description
#' Creates the condition that CyteTypeR signals when the status or the results
#' of a job cannot be fetched: by [GetResults()], and by [CyteTypeR()] when it
#' stops waiting for a job because of such errors. Errors about the API key are
#' signalled as `cytetype_auth_error`, a subclass.
#'
#' @param message Error message.
#' @param call Where the error came from: `"network"` when the server could not
#'   be reached, `"api"` for other errors. Default is `NULL`.
#' @param error_code The error code the server sent, such as
#'   `"REPORT_DELETED"`, or `NULL` when there is none. Default is `NULL`.
#' @param status_code The HTTP status of the server's response, such as `503`,
#'   or `NULL` when there is none, for example when the server could not be
#'   reached. Default is `NULL`.
#' @return An error condition of class `cytetype_api_error`, with the elements
#'   `message`, `call`, `error_code` and `status_code`. Printing it shows
#'   `CyteType API Error: <message>`.
#' @examples
#' error <- cytetype_api_error(
#'   paste(
#'     "Error while checking job status: HTTP 410 [REPORT_DELETED]",
#'     "This report has been deleted by the owner"
#'   ),
#'   call = "api",
#'   error_code = "REPORT_DELETED",
#'   status_code = 410L
#' )
#' print(error)
#'
#' # Handle the condition by its class and read the server's code.
#' tryCatch(
#'   stop(error),
#'   cytetype_api_error = function(e) e$error_code
#' )
#' @export
cytetype_api_error <- function(message, call = NULL, error_code = NULL,
                               status_code = NULL) {
  structure(
    list(
      message = message,
      call = call,
      error_code = error_code,
      status_code = status_code
    ),
    class = c("cytetype_api_error", "error", "condition")
  )
}

#' @export
print.cytetype_api_error <- function(x, ...) {
  cat("CyteType API Error: ", x$message, "\n")
}

# `call` holds "network" or "api", not a call, so report the error as
# "Error: <message>" rather than "Error in "api": <message>".
#' @export
conditionCall.cytetype_api_error <- function(c) {
  NULL
}

# One FastAPI validation error, such as
# {"loc": ["body", "visibility"], "msg": "Input should be 'private', ..."}.
.validation_error_text <- function(item) {
  if (.is_nonblank_string(item)) {
    return(item)
  }
  message <- .json_get(item, "msg")
  if (!.is_nonblank_string(message)) {
    return(NULL)
  }
  location <- .json_get(item, "loc")
  parts <- if (is.list(location)) {
    unlist(Filter(function(part) is.atomic(part) && length(part) == 1L && !is.na(part), location))
  } else {
    NULL
  }
  if (length(parts) == 0L) {
    return(message)
  }
  paste0(paste(as.character(parts), collapse = "."), ": ", message)
}

# The error code and message of an error response, or NULL. The server sends
# {"detail": {"error_code": ..., "message": ...}} for its own errors, including
# plain HTTP errors raised inside its routes. FastAPI sends {"detail": "..."}
# for HTTP errors raised elsewhere and, for invalid requests (HTTP 422),
# {"detail": [{"loc": [...], "msg": ...}, ...]}. error_code is NULL when the
# server sent none.
.parse_server_error <- function(resp) {
  body <- tryCatch(httr2::resp_body_json(resp), error = function(e) NULL)
  if (is.null(body)) {
    return(NULL)
  }
  detail <- if (.is_json_object(body) && "detail" %in% names(body)) body$detail else body
  if (.is_json_object(detail)) {
    message <- .json_get(detail, "message")
    if (!.is_nonblank_string(message)) {
      return(NULL)
    }
    error_code <- .json_get(detail, "error_code")
    return(list(
      error_code = if (.is_nonblank_string(error_code)) error_code else NULL,
      message = message
    ))
  }
  if (.is_nonblank_string(detail)) {
    return(list(error_code = NULL, message = detail))
  }
  if (.is_json_array(detail)) {
    messages <- unlist(lapply(detail, .validation_error_text))
    if (length(messages) > 0L) {
      return(list(error_code = NULL, message = paste(messages, collapse = "; ")))
    }
  }
  NULL
}

# "[ERROR_CODE] message", or the message alone when there is no code.
.format_parsed_error <- function(parsed) {
  if (is.null(parsed$error_code)) {
    parsed$message
  } else {
    paste0("[", parsed$error_code, "] ", parsed$message)
  }
}

.format_server_error <- function(e) {
  if (!inherits(e, "httr2_http") || is.null(e$resp)) return(NULL)
  parsed <- .parse_server_error(e$resp)
  if (is.null(parsed)) return(NULL)
  .format_parsed_error(parsed)
}

.rate_limit_message <- function(server_message) {
  paste0(
    server_message,
    "\nThe limits apply to every job, including jobs that use your own LLM ",
    "through llm_configs. Wait for the limit to reset before retrying."
  )
}

.stop_if_rate_limited <- function(e) {
  if (inherits(e, "httr2_http") && !is.null(e$resp)) {
    parsed <- .parse_server_error(e$resp)
    if (!is.null(parsed) && identical(parsed$error_code, "RATE_LIMIT_EXCEEDED")) {
      stop(.rate_limit_message(parsed$message), call. = FALSE)
    }
  }
}

.stop_with_server_error <- function(e, context = NULL) {
  .stop_if_rate_limited(e)
  detail <- .format_server_error(e)
  msg <- detail %||% conditionMessage(e)
  if (!is.null(context)) msg <- paste0(context, ": ", msg)
  stop(msg, call. = FALSE)
}
