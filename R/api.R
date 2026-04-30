# HTTP primitives, request utilities, and shared constants for CyteType API communication.

# Upload size limits (numeric to avoid integer overflow)
.MAX_UPLOAD_BYTES <- list(
  obs_duckdb = 2 * 1024 * 1024 * 1024,   # 2 GB
  vars_h5    = 50 * 1024 * 1024 * 1024   # 50 GB
)

# Chunked upload retry: delays (sec) after 1st, 2nd, 3rd failure; status codes treated as transient (incl. network/gateway)
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
.api_response_helper <- function(job_id, api_url, req_item, auth_token = NULL) {

  req_api_url <- .url_path(api_url, req_item, job_id)

  tryCatch({
    # Build and execute request
    req <- request(req_api_url) |>
      req_method("GET") |>
      req_timeout(30)

    if (!is.null(auth_token)) {
      req <- req |> req_auth_bearer_token(auth_token)
    }

    response <- req |> req_perform()
    status_code <- resp_status(response)

    # Handle specific status codes with switch for clarity
    switch(as.character(status_code),
           "404" = {
             return(list(status_code = 404, data = list()))
           },
           "401" = {
             log_debug("Authentication failed for job {job_id}")
             stop("Authentication failed: Invalid or expired auth token")
           },
           "403" = {
             log_debug("Authorization failed for job {job_id}")
             stop("Authorization failed: Access denied")
           }
    )

    # Check for other non-success status codes
    if (status_code < 200 || status_code >= 300) {
      parsed <- .parse_server_error(response)
      if (!is.null(parsed)) {
        stop(paste0("HTTP ", status_code, " [", parsed$error_code, "] ", parsed$message))
      }
      error_body <- tryCatch(resp_body_string(response), error = function(e) "Unknown error")
      stop(paste("HTTP", status_code, "error:", error_body))
    }

    # Parse successful response
    response_data <- tryCatch({
      resp_body_json(response)
    }, error = function(e) {
      log_debug("Error parsing JSON response for job {job_id}: {e$message}")
      stop(paste("Invalid JSON response:", e$message))
    })

    return(list(
      status_code = as.integer(status_code),
      data = response_data
    ))

  }, error = function(e) {
    .stop_if_rate_limited(e)

    # Preserve authentication/authorization errors
    if (grepl("Authentication failed|Authorization failed", e$message)) {
      stop(e)
    }

    # Don't double-wrap custom errors
    if (inherits(e, "cytetype_api_error")) {
      stop(e)
    }

    # Categorize and wrap other errors
    error_type <- if (grepl("network|connection|resolve|timeout", tolower(e$message))) {
      "network"
    } else {
      "api"
    }

    log_debug("Error during status check for job {job_id}: {e$message}")
    stop(cytetype_api_error(message = paste("Error while checking job status:", e$message), call = error_type))
  })
}

# PUT raw bytes to a presigned URL with retry, ETag validation, and proper error classification.
.put_to_presigned_url <- function(presigned_url,
                                  chunk_data,
                                  timeout_seconds,
                                  file_kind = "vars_h5",
                                  chunk_idx = NULL,
                                  n_chunks = NULL) {
  chunk_label <- if (!is.null(chunk_idx) && !is.null(n_chunks)) {
    paste0(" chunk ", chunk_idx + 1L, "/", n_chunks)
  } else {
    ""
  }
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
    response_body <- tryCatch(httr2::resp_body_string(resp), error = function(e) NULL)
    if (!is.null(response_body) && nchar(response_body) > 500L) {
      response_body <- paste0(substr(response_body, 1L, 500L), "... [truncated]")
    }
    body_detail <- if (!is.null(response_body) && nzchar(response_body)) {
      paste0("; response body: ", response_body)
    } else {
      ""
    }
    stop(cytetype_api_error(
      message = paste0(
        "Presigned upload of ", file_kind, chunk_label,
        " rejected with HTTP ", status, body_detail
      ),
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
