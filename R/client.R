# Upload size limits: 100MB and 50GB respectively (vars_h5 uses numeric to avoid integer overflow)
.MAX_UPLOAD_BYTES <- list(obs_duckdb = 100L * 1024L * 1024L, vars_h5 = 50 * 1024 * 1024 * 1024)

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

# Chunked upload flow (initiate -> PUT chunks -> complete). Uses scalar timeout; httr2 req_timeout takes seconds.

.upload_file_chunked <- function(api_url,
                                 auth_token,
                                 file_kind,
                                 file_path,
                                 timeout_seconds = 3600L,
                                 max_workers = 4L) {
  api_url <- paste0(api_url, "/upload")

  if (!file.exists(file_path)) stop("Upload file not found: ", file_path)

  size <- file.info(file_path)$size
  max_bytes <- .MAX_UPLOAD_BYTES[[file_kind]]

  if (is.null(max_bytes) || size > max_bytes) {
    stop(file_kind, " exceeds upload limit: ", size, " bytes (max ", max_bytes, ")")
  }

  connection_timeout <- 72L

  # Step 1 – Initiate (empty POST; explicit empty body for compatibility)
  init_resp <- tryCatch(
    .make_req(api_url, paste0(file_kind, "/initiate"), auth_token) |>
      req_method("POST") |>
      httr2::req_body_raw(raw(0), type = "application/json") |>
      req_timeout(connection_timeout) |>
      req_perform() |>
      resp_body_json(),
    error = function(e) { .stop_if_rate_limited(e); stop(e) }
  )

  upload_id <- init_resp$upload_id
  chunk_size <- as.integer(init_resp$chunk_size_bytes %||% (50L * 1024L * 1024L)) ## default 50MB
  server_max <- init_resp$max_size_bytes
  if (!is.null(server_max) && length(server_max) > 0) {
    server_max_n <- as.numeric(server_max)[1]
    if (!is.na(server_max_n) && size > server_max_n) {
      stop(file_kind, " exceeds server limit: ", size, " bytes > ", server_max_n)
    }
  }

  n_chunks <- if (size > 0) as.integer(ceiling(size / chunk_size)) else 0L

  # Step 2 – Upload chunks (future for cross-platform parallel, req_retry for resilience)
  if (n_chunks > 0) {
    effective_workers <- min(max_workers, n_chunks)
    chunk_idxs <- seq_len(n_chunks) - 1L

    .upload_one_chunk <- function(chunk_idx) {
      tryCatch({
        con <- file(file_path, "rb")
        on.exit(close(con), add = TRUE)
        offset <- chunk_idx * chunk_size
        read_size <- min(chunk_size, size - offset)
        seek(con, offset)
        chunk_data <- readBin(con, what = "raw", n = read_size)
        .make_req(api_url, paste0(upload_id, "/chunk/", chunk_idx), auth_token) |>
          req_method("PUT") |>
          httr2::req_body_raw(chunk_data, type = "application/octet-stream") |>
          req_timeout(timeout_seconds) |>
          httr2::req_retry(
            max_tries = length(.CHUNK_UPLOAD_BACKOFF_SECS) + 1L,
            retry_on_failure = TRUE,
            is_transient = function(resp) resp_status(resp) %in% .CHUNK_UPLOAD_TRANSIENT_STATUSES,
            backoff = function(tries) .CHUNK_UPLOAD_BACKOFF_SECS[min(tries, length(.CHUNK_UPLOAD_BACKOFF_SECS))]
          ) |>
          req_perform()
        list(ok = TRUE)
      }, error = function(e) {
        .stop_if_rate_limited(e)
        list(ok = FALSE, chunk_idx = chunk_idx, message = conditionMessage(e))
      })
    }

    if (effective_workers > 1L && requireNamespace("furrr", quietly = TRUE)) {
      chunk_results <- furrr::future_map(chunk_idxs, .upload_one_chunk)
    } else {
      chunk_results <- purrr::map(chunk_idxs, .upload_one_chunk)
    }
    failed <- which(!vapply(chunk_results, function(r) is.list(r) && isTRUE(r$ok), logical(1)))
    if (length(failed) > 0L) {
      r <- chunk_results[[failed[1L]]]
      stop("Upload chunk ", r$chunk_idx, " failed: ", r$message)
    }
  }
  # Step 3 – Complete (empty POST; explicit empty body for compatibility)
  complete_resp <- tryCatch(
    .make_req(api_url, paste0(upload_id, "/complete"), auth_token) |>
      req_method("POST") |>
      httr2::req_body_raw(raw(0), type = "application/json") |>
      req_timeout(connection_timeout) |>
      req_perform() |>
      resp_body_json(),
    error = function(e) { .stop_if_rate_limited(e); stop(e) }
  )

  complete_resp
}


.upload_obs_duckdb <- function(api_url, auth_token, file_path, timeout_seconds = 3600L, max_workers = 4L) {
  .upload_file_chunked(api_url, auth_token, "obs_duckdb", file_path, timeout_seconds, max_workers)
}

.upload_vars_h5 <- function(api_url, auth_token, file_path, timeout_seconds = 3600L, max_workers = 4L) {
  .upload_file_chunked(api_url, auth_token, "vars_h5", file_path, timeout_seconds, max_workers)
}

# Submit a new job
.submit_job <- function(payload, api_url, auth_token = NULL){

  submit_url <- .url_path(api_url, "annotate")
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
      req_timeout(180)

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
    .stop_if_rate_limited(e)
    cat("An error occurred:", conditionMessage(e), "\n")
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

  report_url <- .url_path(api_url, "report", job_id)

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

