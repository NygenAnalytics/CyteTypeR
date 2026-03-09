# Multi-step workflow orchestration: uploads, job submission, polling, and result retrieval.

# Chunked upload flow (initiate -> PUT chunks -> complete). Uses scalar timeout; httr2 req_timeout takes seconds.

.upload_file_chunked <- function(api_url,
                                 auth_token,
                                 file_kind,
                                 file_path,
                                 timeout_seconds = 3600L,
                                 max_workers = 6L,
                                 show_progress = TRUE) {

  if (!file.exists(file_path)) stop("Upload file not found: ", file_path)

  size <- file.info(file_path)$size
  max_bytes <- .MAX_UPLOAD_BYTES[[file_kind]]

  if (is.null(max_bytes) || size > max_bytes) {
    stop(file_kind, " exceeds upload limit: ", size, " bytes (max ", max_bytes, ")")
  }

  connection_timeout <- 72L
  api_url <- paste0(api_url, "/upload")

  # Step 1 – Initiate
  init_resp <- tryCatch(
    .make_req(api_url, paste0(file_kind, "/initiate"), auth_token) |>
      req_method("POST") |>
      httr2::req_body_json(list("file_size_bytes" = size), type = "application/json") |>
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

  presigned_urls <- init_resp$presigned_urls %||% init_resp$presignedUrls %||% list()
  r2_upload_id <- init_resp$r2_upload_id %||% init_resp$r2UploadId %||% NULL
  use_r2 <- length(presigned_urls) > 0L && !is.null(r2_upload_id)

  log_info("Upload {file_kind}: {n_chunks} chunks, use_r2={use_r2}, presigned_urls={length(presigned_urls)}")

  if (use_r2 && length(presigned_urls) < n_chunks) {
    stop("Server returned ", length(presigned_urls), " presigned URLs but need at least ", n_chunks, " (one per chunk).")
  }

  # Step 2 – Upload chunks (future for cross-platform parallel, req_retry for resilience)
  if (n_chunks > 0) {
    effective_workers <- min(max_workers, n_chunks)
    chunk_idxs <- seq_len(n_chunks) - 1L

    .upload_chunk_server <- function(chunk_idx) {
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

    .upload_chunk_r2 <- function(chunk_idx) {
      tryCatch({
        con <- file(file_path, "rb")
        on.exit(close(con), add = TRUE)
        offset <- chunk_idx * chunk_size
        read_size <- min(chunk_size, size - offset)
        seek(con, offset)
        chunk_data <- readBin(con, what = "raw", n = read_size)
        presigned_url <- presigned_urls[[chunk_idx + 1L]]
        etag <- .put_to_presigned_url(presigned_url, chunk_data, timeout_seconds)
        list(ok = TRUE, etag = etag, part_number = chunk_idx + 1L)
      }, error = function(e) {
        .stop_if_rate_limited(e)
        list(ok = FALSE, chunk_idx = chunk_idx, message = conditionMessage(e))
      })
    }

    upload_fn <- if (use_r2) .upload_chunk_r2 else .upload_chunk_server
    has_furrr <- requireNamespace("furrr", quietly = TRUE)
    has_progressr <- requireNamespace("progressr", quietly = TRUE)
    use_parallel <- effective_workers > 1L && has_furrr && (!show_progress || has_progressr)

    if (use_parallel) {
      oplan <- future::plan(future::multisession, workers = effective_workers)
      on.exit(future::plan(oplan), add = TRUE)

      if (show_progress) {
        pb_fmt <- paste0(
          "Uploading ", file_kind,
          " {cli::pb_bar} {cli::pb_current}/{cli::pb_total} chunks ({cli::pb_rate})"
        )
        progressr::with_progress({
          p <- progressr::progressor(steps = n_chunks)
          chunk_results <- furrr::future_map(chunk_idxs, function(idx) {
            result <- upload_fn(idx)
            p()
            result
          })
        }, handlers = progressr::handler_cli(format = pb_fmt))
      } else {
        chunk_results <- furrr::future_map(chunk_idxs, upload_fn)
      }
    } else {
      if (show_progress) {
        pb_fmt <- paste0(
          "Uploading ", file_kind,
          " {cli::pb_bar} {cli::pb_current}/{cli::pb_total} chunks ({cli::pb_rate})"
        )
        pb_id <- cli::cli_progress_bar(format = pb_fmt, total = n_chunks, clear = FALSE, .envir = environment())
      }
      chunk_results <- purrr::map(chunk_idxs, function(idx) {
        result <- upload_fn(idx)
        if (show_progress) cli::cli_progress_update(id = pb_id)
        result
      })
    }

    failed <- which(!vapply(chunk_results, function(r) is.list(r) && isTRUE(r$ok), logical(1)))
    if (length(failed) > 0L) {
      r <- chunk_results[[failed[1L]]]
      stop("Upload chunk ", r$chunk_idx, " failed: ", r$message)
    }
  }

  # Step 3 – Complete
  complete_body <- if (use_r2) {
    parts <- lapply(chunk_results, function(r) {
      list(ETag = r$etag, PartNumber = r$part_number)
    })
    list(parts = parts)
  } else {
    list()
  }

  complete_resp <- tryCatch(
    .make_req(api_url, paste0(upload_id, "/complete"), auth_token) |>
      req_method("POST") |>
      httr2::req_body_json(complete_body, type = "application/json") |>
      req_timeout(connection_timeout) |>
      req_perform() |>
      resp_body_json(),
    error = function(e) { .stop_if_rate_limited(e); stop(e) }
  )

  complete_resp
}


.upload_obs_duckdb <- function(api_url, auth_token, file_path, timeout_seconds = 3600L,
                               max_workers = 6L, show_progress = TRUE) {
  .upload_file_chunked(api_url, auth_token, "obs_duckdb", file_path, timeout_seconds,
                       max_workers, show_progress)
}

.upload_vars_h5 <- function(api_url, auth_token, file_path, timeout_seconds = 3600L,
                             max_workers = 6L, show_progress = TRUE) {
  .upload_file_chunked(api_url, auth_token, "vars_h5", file_path, timeout_seconds,
                       max_workers, show_progress)
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

# Multi-step job results request (status check + results fetch)
.make_results_request <- function(job_id, api_url, auth_token = NULL) {

  make_response <- function(status, result = NULL, message, raw = NULL) {
    list(status = status, result = result, message = message, raw_response = raw)
  }

  tryCatch({
    status_resp <- .api_response_helper(job_id, api_url, 'status', auth_token)

    if (status_resp$status_code == 404) {
      return(make_response("not_found", message = "Job not found"))
    }

    job_status <- status_resp$data$jobStatus
    status_data <- status_resp$data

    if (job_status == "completed") {
      results_resp <- tryCatch(
        .api_response_helper(job_id, api_url, 'results', auth_token),
        error = function(e) {
          make_response(
            "failed",
            message = paste("Job completed but results unavailable:", e$message),
            raw = status_data
          )
        }
      )

      if (!is.null(results_resp$status) && results_resp$status == "failed") {
        return(results_resp)
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

    return(make_response(
      "unknown",
      message = paste("Unknown job status:", job_status),
      raw = status_data
    ))

  }, error = function(e) {
    return(make_response(
      "error",
      message = paste("Error checking job status:", e$message)
    ))
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
  consecutive_errors <- 0
  max_consecutive_errors <- 5

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

      # Reset counters on valid server response
      if (status != "not_found") consecutive_not_found <- 0
      if (status != "error") consecutive_errors <- 0

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
                 .sleep_with_spinner(poll_interval, current_cluster_status, show_progress)
               } else {
                 Sys.sleep(poll_interval)
               }
               last_cluster_status <- current_cluster_status
             },

             "not_found" = {
               consecutive_not_found <- consecutive_not_found + 1
               log_debug("Job {job_id} not found (404, attempt {consecutive_not_found}). Waiting {poll_interval}s...")

               if (consecutive_not_found >= 5) {
                 cat("\n")
                 stop(paste0("Job '", job_id, "' not found after ", consecutive_not_found,
                             " attempts. Verify the job_id is correct."))
               }
               .sleep_with_spinner(poll_interval, current_cluster_status, show_progress)
               last_cluster_status <- current_cluster_status
             },

             "error" = {
               consecutive_errors <- consecutive_errors + 1
               error_msg <- status_response$message %||% "Unknown error"
               log_warn("Error checking job {job_id} ({consecutive_errors}/{max_consecutive_errors}): {error_msg}")
               if (consecutive_errors >= max_consecutive_errors) {
                 cat("\n")
                 stop(paste("Stopping after", max_consecutive_errors, "consecutive errors:", error_msg))
               }
               .sleep_with_spinner(poll_interval, current_cluster_status, show_progress)
             },

             {
               cat("\n")
               stop(paste("Job", job_id, "returned unexpected status:", status))
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
