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
