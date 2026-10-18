# HTTP primitives, request utilities, and shared constants for CyteType API communication.

# Upload size limits (numeric to avoid integer overflow)
.MAX_UPLOAD_BYTES <- list(
  obs_duckdb = 2 * 1024 * 1024 * 1024,   # 2 GB
  vars_h5    = 50 * 1024 * 1024 * 1024   # 50 GB
)

# Chunked upload retry: delays (sec) after 1st, 2nd, 3rd failure; status codes treated as transient (incl. network/gateway).
# The same statuses are retried while waiting for the results of a completed job.
.CHUNK_UPLOAD_BACKOFF_SECS <- c(1L, 5L, 20L)
.CHUNK_UPLOAD_TRANSIENT_STATUSES <- c(500L, 502L, 503L, 504L)

# URL path builder (avoids file.path backslashes on Windows)
.url_path <- function(...) {
  x <- vapply(c(...), function(seg) gsub("^/+|/+$", "", as.character(seg)), character(1))
  paste(x[nzchar(x)], collapse = "/")
}

.make_req <- function(base_url, path, auth_token) {
  req <- request(paste0(base_url, "/", path))
  if (!is.null(auth_token)) {
    req <- req_headers(req, Authorization = paste("Bearer", auth_token))
  }
  return(req)
}

#' @importFrom httr2 req_auth_bearer_token req_body_json req_headers req_method req_perform req_timeout request resp_body_json resp_body_string resp_status
# GET /status/{job_id} or /results/{job_id}. Returns list(status_code, data)
# for 2xx responses and 404. Every other failure is signalled once, with one
# message: cytetype_auth_error for HTTP 401 and 403, cytetype_api_error
# (carrying the server's error_code when it sent one) otherwise. Both carry
# the HTTP status as status_code when the server answered with an error.
.api_response_helper <- function(job_id, api_url, req_item, auth_token = NULL) {

  req_api_url <- .url_path(api_url, req_item, job_id)
  context <- if (identical(req_item, "results")) {
    "Error while fetching job results"
  } else {
    "Error while checking job status"
  }

  tryCatch({
    # Build and execute request
    req <- request(req_api_url) |>
      req_method("GET") |>
      req_timeout(30)

    if (!is.null(auth_token)) {
      req <- req |> req_auth_bearer_token(auth_token)
    }

    response <- req |>
      httr2::req_error(is_error = function(resp) FALSE) |>
      req_perform()
    status_code <- resp_status(response)

    # Handle specific status codes with switch for clarity
    switch(as.character(status_code),
           "404" = {
             return(list(status_code = 404, data = list()))
           },
           "401" = {
             log_debug("Authentication failed for job {job_id}")
             .stop_authentication(
               "Authentication failed: Invalid or expired auth token",
               "INVALID_TOKEN",
               status_code = status_code
             )
           },
           "403" = {
             log_debug("Authorization failed for job {job_id}")
             .stop_authentication(
               "Authorization failed: Access denied",
               "ACCESS_DENIED",
               status_code = status_code
             )
           }
    )

    # Check for other non-success status codes
    if (status_code < 200 || status_code >= 300) {
      parsed <- .parse_server_error(response)
      if (!is.null(parsed) && identical(parsed$error_code, "RATE_LIMIT_EXCEEDED")) {
        stop(cytetype_api_error(
          message = .rate_limit_message(parsed$message),
          call = "api",
          error_code = "RATE_LIMIT_EXCEEDED",
          status_code = status_code
        ))
      }
      detail <- if (!is.null(parsed)) {
        .format_parsed_error(parsed)
      } else {
        error_body <- tryCatch(trimws(resp_body_string(response)), error = function(e) "")
        if (nzchar(error_body)) error_body else "Unknown error"
      }
      stop(cytetype_api_error(
        message = paste0(context, ": HTTP ", status_code, " ", detail),
        call = "api",
        error_code = parsed$error_code,
        status_code = status_code
      ))
    }

    # Parse successful response. The results endpoint answers 202 with a
    # JOB_PROCESSING body while the job runs; callers check status_code.
    response_data <- tryCatch({
      resp_body_json(response)
    }, error = function(e) {
      log_debug("Error parsing JSON response for job {job_id}: {conditionMessage(e)}")
      stop(cytetype_api_error(
        message = paste0(context, ": invalid JSON response: ", conditionMessage(e)),
        call = "api"
      ))
    })

    return(list(
      status_code = as.integer(status_code),
      data = response_data
    ))

  }, error = function(e) {
    # Signalled above (including authentication and authorization errors):
    # pass on as they are, so the message is never prefixed twice.
    if (inherits(e, "cytetype_api_error")) {
      stop(e)
    }

    # Categorize and wrap other errors. httr2 signals requests that received
    # no response (DNS, refused connections, timeouts) as httr2_failure.
    error_type <- if (
      inherits(e, "httr2_failure") ||
        grepl("network|connection|resolve|timeout", tolower(conditionMessage(e)))
    ) {
      "network"
    } else {
      "api"
    }

    log_debug("Error during {req_item} request for job {job_id}: {conditionMessage(e)}")
    stop(cytetype_api_error(message = paste0(context, ": ", conditionMessage(e)), call = error_type))
  })
}

# PUT raw bytes to a presigned URL with retry, ETag validation, and proper error classification.
.put_to_presigned_url <- function(presigned_url, chunk_data, timeout_seconds) {
  resp <- request(presigned_url) |>
    req_method("PUT") |>
    httr2::req_body_raw(chunk_data, type = "application/octet-stream") |>
    req_timeout(timeout_seconds) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_retry(
      max_tries = length(.CHUNK_UPLOAD_BACKOFF_SECS) + 1L,
      retry_on_failure = TRUE,
      is_transient = function(resp) resp_status(resp) %in% .CHUNK_UPLOAD_TRANSIENT_STATUSES,
      backoff = function(tries) .CHUNK_UPLOAD_BACKOFF_SECS[min(tries, length(.CHUNK_UPLOAD_BACKOFF_SECS))]
    ) |>
    req_perform()

  status <- resp_status(resp)
  if (status >= 400) {
    stop(cytetype_api_error(
      message = paste0("Presigned upload rejected with HTTP ", status),
      call = "api"
    ))
  }

  etag <- httr2::resp_header(resp, "ETag")
  if (is.null(etag) || !nzchar(etag)) {
    stop(cytetype_api_error(
      message = "Presigned PUT succeeded but response is missing ETag header",
      call = "network"
    ))
  }

  etag
}
