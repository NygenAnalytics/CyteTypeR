# Upload size limits (bytes): match Python API (vars_h5 10GB uses numeric to avoid integer overflow)
.MAX_UPLOAD_BYTES <- list(obs_duckdb = 100L * 1024L * 1024L, vars_h5 = 10 * 1024 * 1024 * 1024)

#' POST a file as binary to upload/{file_kind}. Returns list with upload_id, file_kind, etc.
#' @noRd
.upload_file_binary <- function(api_url, auth_token, file_kind, file_path, timeout_seconds = 3600L) {
  if (!file.exists(file_path)) stop("Upload file not found: ", file_path)
  size <- file.info(file_path)$size
  max_bytes <- .MAX_UPLOAD_BYTES[[file_kind]]
  if (is.null(max_bytes) || size > max_bytes) {
    stop(file_kind, " exceeds upload limit: ", size, " bytes (max ", max_bytes, ")")
  }
  url <- file.path(api_url, "upload", file_kind)
  req <- httr2::request(url) |>
    httr2::req_method("POST") |>
    httr2::req_body_file(file_path, type = "application/octet-stream") |>
    httr2::req_headers("Content-Type" = "application/octet-stream") |>
    httr2::req_timeout(c(30, timeout_seconds))
  if (!is.null(auth_token)) req <- req |> httr2::req_auth_bearer_token(auth_token)
  resp <- httr2::req_perform(req)
  if (httr2::resp_status(resp) != 200) stop("Upload failed: ", httr2::resp_body_string(resp))
  httr2::resp_body_json(resp)
}

.upload_obs_duckdb <- function(api_url, auth_token, file_path, timeout_seconds = 3600L) {
  .upload_file_binary(api_url, auth_token, "obs_duckdb", file_path, timeout_seconds)
}

.upload_vars_h5 <- function(api_url, auth_token, file_path, timeout_seconds = 3600L) {
  .upload_file_binary(api_url, auth_token, "vars_h5", file_path, timeout_seconds)
}

# Submit a new job
.submit_job <- function(payload, api_url, auth_token = NULL){

  submit_url <- file.path(api_url, "annotate")
  log_info("Submitting job to CyteType...")

  tryCatch({
    # Validate inputs
    if (is.null(payload)) {
      stop("Payload cannot be NULL")
    }

    # Build request
    req <- request(submit_url) |>
      req_method("POST") |>
      req_body_json(payload, na = "string") |>
      req_headers("Content-Type" = "application/json") |>
      req_timeout(60)

    # Add auth token if provided
    if (!is.null(auth_token)) {
      req <- req |> req_auth_bearer_token(auth_token)
    }

    # Execute request
    response <- req_perform(req)

    # Check status
    status <- resp_status(response)
    if (status != 200) {
      stop("HTTP ", status, ": ", resp_body_string(response))
    }

    # Parse response
    response_data <- resp_body_json(response)
    job_id <- response_data$job_id

    # Validate job_id
    if (is.null(job_id) || length(job_id) == 0 || nchar(job_id) == 0) {
      stop("API response did not contain a valid 'job_id'")
    }

    log_debug("Job submitted successfully. Job ID: {job_id}")
    return(as.character(job_id))

  }, error = function(e) {
    # Log the error
    cat("An error occurred:", conditionMessage(e), "\n")

    # Return NA for any error
    return(NA_character_)
  })
}
# Polling for results
.poll_for_results <- function(job_id, api_url, poll_interval = NULL,
                              timeout = NULL, auth_token = NULL,
                              show_progress = TRUE) {

  poll_interval <- poll_interval %||% .get_default_poll_interval()
  timeout <- timeout %||% .get_default_timeout()


  # Initialize
  log_info("CyteType job (id: {job_id}) submitted. Polling for results...")
  Sys.sleep(5)

  report_url <- file.path(api_url, "report", job_id)

  # Info for auth token uses
  if (!is.null(auth_token)) {
    log_info("Token secured report (updates automatically): \n{report_url}")
    cli::cli_text("{.url {report_url}}")
  } else {
    log_info("Report (updates automatically) available at: \n")
    cli::cli_text("{.url {report_url}}")
  }

  log_info("If disconnected, retrieve results with: GetResults()")

  # Initialize tracking

  start_time <- Sys.time()
  last_cluster_status <- list()
  spinner_frame = 0
  consecutive_not_found <- 0

  # Main polling loop
  repeat {
    elapsed <- as.numeric(Sys.time() - start_time)

    if (elapsed > timeout) {
      cat("\n")
      stop("timeout", "Timeout while fetching results")
    }

    tryCatch({
      status_response <- .make_results_request(job_id, api_url, auth_token)
      status <- status_response$status

      # Reset not found counter on valid response
      if (status != "not_found"){ consecutive_not_found <- 0 }

      # Extract cluster status
      current_cluster_status <- status_response$raw_response$clusterStatus %||% list()

      switch(status,
             "completed" = {
               if (show_progress &&
                   length(current_cluster_status) > 0) {

                 .display_cluster_status_cli(
                   current_cluster_status,
                   is_final = TRUE,
                   spinner_frame = spinner_frame
                 )
               }
               else if (length(current_cluster_status) > 0) {
                 cat("\n")
               }

               log_info("Job {job_id} completed successfully.")

               if (!is.list(status_response$result)) {
                 stop(cytetype_api_error(message = paste("Expected list result, got", class(status_response$result)), call = "api"))
               }

               return(status_response$result)
             },

             "failed" = {
               if (show_progress &&
                   length(current_cluster_status) > 0 &&
                   !identical(current_cluster_status, last_cluster_status)) {

                 .display_cluster_status_cli(
                   current_cluster_status,
                   is_final = TRUE,
                   spinner_frame = spinner_frame
                 )
                 last_cluster_status <- current_cluster_status
               }
               error_msg <- status_response$message %||% "Unknown server error"
               stop(paste("Server error:", error_msg))
             },
             "processing" = ,
             "pending" = {
               log_debug("Job {job_id} status: {status}. Waiting {poll_interval}s...")
               if (show_progress && length(current_cluster_status) > 0){

                 # Sleep with spinner updates
                 .sleep_with_spinner(poll_interval,current_cluster_status,show_progress)
               }
               last_cluster_status <- current_cluster_status
             },

             "not_found" = {
               consecutive_not_found <- consecutive_not_found + 1

               if (!is.null(auth_token) && consecutive_not_found >= 3) {
                 log_warn("\u26a0\ufe0f  Getting consecutive 404 responses with auth token. This might indicate authentication issues.")
                 log_warn("Please verify your auth_token is valid and has proper permissions")
                 log_warn("If you're using a shared server, contact your administrator.")
                 consecutive_not_found <- 0  # Reset to avoid spam
               }

               log_debug("Results endpoint not ready yet for job {job_id} (404). Waiting {poll_interval}s...")
               .sleep_with_spinner(poll_interval,current_cluster_status,show_progress)
               last_cluster_status <- current_cluster_status
             },

             {
               log_warn("Job {job_id} has unknown status: '{status}'. Continuing...")
               .sleep_with_spinner(poll_interval,current_cluster_status,show_progress)
             }
      )

    }, error = function(e) {
      if (inherits(e, "cytetype_api_error")) {
        error_msg <- e$message

        # Handle auth errors immediately
        if (grepl("Authentication failed|Authorization failed", error_msg)) {
          cat("\n")
          log_error("\u274c {error_msg}")
          log_error("Check auth_token validity and permissions")
          stop(e)
        }

        # Retry on network errors
        if (grepl("Network error", error_msg)) {
          log_debug("Network error for {job_id}: {error_msg}. Retrying...")
          retry_interval <- min(poll_interval, 5)
          .sleep_with_spinner(retry_interval,current_cluster_status,show_progress)
        } else {
          cat("\n")
          stop(e)
        }
      } else {
        cat("\n")
        stop(e)
      }
    })
  }
}

# Helper for sleeping with spinner animation
.sleep_with_spinner <- function(duration,cluster_status,show_progress = TRUE) {
  for (i in seq_len(duration * 2)) {
    Sys.sleep(0.5)
    if (show_progress){
      .display_cluster_status_cli(cluster_status, is_final=FALSE, spinner_frame=i)
    }

  }
}

