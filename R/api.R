# API Response Helper for CyteType

#' @importFrom httr2 req_auth_bearer_token req_body_json req_headers req_method req_perform req_timeout request resp_body_json resp_body_string resp_status
.api_response_helper <- function(job_id, api_url, req_item, auth_token = NULL) {

  req_api_url <- file.path(api_url, req_item, job_id)

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
    stop(cytetype_api_error(error_type, paste("Error while checking job status:", e$message)))
  })
}

# Make Request for Job Results
.make_results_request <- function(job_id, api_url, auth_token = NULL) {

  # Helper for consistent responses
  make_response <- function(status, result = NULL, message, raw = NULL) {
    list(status = status, result = result, message = message, raw_response = raw)
  }

  tryCatch({
    # Get job status
    status_resp <- .api_response_helper(job_id, api_url, 'status', auth_token)

    # Handle 404 immediately
    if (status_resp$status_code == 404) {
      return(make_response("not_found", message = "Job not found"))
    }

    job_status <- status_resp$data$jobStatus
    status_data <- status_resp$data

    # Process based on job status
    if (job_status == "completed") {
      # Try to get results
      results_resp <- tryCatch({
        .api_response_helper(job_id, api_url, 'results', auth_token)
      }, error = function(e) {
        # If results fetch fails, treat as failed job
        return(make_response(
          "failed",
          message = paste("Job completed but results unavailable:", e$message),
          raw = status_data
        ))
      })

      # Check if we got an error response above
      if (is.list(results_resp) && !is.null(results_resp$status)) {
        return(results_resp)  # Return the error response
      }

      if (results_resp$status_code == 404) {
        return(make_response(
          "failed",
          message = "Job completed but results are unavailable",
          raw = status_data
        ))
      }

      return(make_response(
        "completed",
        result = results_resp$data,
        message = "Job completed successfully",
        raw = status_data
      ))
    }

    if (job_status == "failed") {
      return(make_response(
        "failed",
        message = "Job failed",
        raw = status_data
      ))
    }

    if (job_status %in% c("processing", "pending")) {
      return(make_response(
        job_status,
        message = paste("Job is", job_status),
        raw = status_data
      ))
    }

    # Unknown status
    return(make_response(
      "unknown",
      message = paste("Unknown job status:", job_status),
      raw = status_data
    ))

  }, error = function(e) {
    # Handle any unexpected errors
    return(make_response(
      "error",
      message = paste("Error checking job status:", e$message)
    ))
  })
}

