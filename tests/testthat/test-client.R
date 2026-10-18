test_that(".upload_file_chunked errors when file does not exist", {
  expect_error(
    CyteTypeR:::.upload_file_chunked("https://api.example.com", NULL, "obs_duckdb", "/nonexistent/path.duckdb", 60L),
    "Upload file not found"
  )
})

test_that(".upload_file_chunked errors for unknown file_kind (no upload limit)", {
  f <- tempfile(fileext = ".bin")
  on.exit(unlink(f), add = TRUE)
  writeBin(raw(1), f)
  expect_error(
    CyteTypeR:::.upload_file_chunked("https://api.example.com", NULL, "unknown_kind", f, 60L),
    "exceeds upload limit"
  )
})

test_that(".upload_file_chunked rejects when presigned URL count < chunk count", {
  f <- tempfile(fileext = ".h5")
  on.exit(unlink(f), add = TRUE)
  writeBin(raw(1024), f)

  httr2::local_mocked_responses(function(req) {
    httr2::response(
      status_code = 200L,
      headers = list("Content-Type" = "application/json"),
      body = charToRaw(jsonlite::toJSON(list(
        upload_id = "u1",
        chunk_size_bytes = 512L,
        r2_upload_id = "r2-abc",
        presigned_urls = list("https://r2.example.com/chunk/0")
      ), auto_unbox = TRUE))
    )
  })

  expect_error(
    CyteTypeR:::.upload_file_chunked(
      "https://api.example.com", NULL, "vars_h5", f, 60L
    ),
    "presigned URLs but need at least"
  )
})

# --- Upload flow tests ---

.capture_body <- function(req) {
  tryCatch({
    d <- req$body$data
    if (is.null(d)) return(NULL)
    if (is.raw(d)) return(rawToChar(d))
    if (is.character(d)) return(d[[1L]])
    as.character(jsonlite::toJSON(d, auto_unbox = TRUE))
  }, error = function(e) NULL)
}

test_that("server-proxy upload: initiate -> chunk PUTs -> complete with empty body", {
  f <- tempfile(fileext = ".duckdb")
  on.exit(unlink(f), add = TRUE)
  writeBin(raw(1024), f)

  request_log <- list()

  httr2::local_mocked_responses(function(req) {
    url <- req$url
    request_log[[length(request_log) + 1L]] <<- list(url = url, method = req$method, body = .capture_body(req))

    if (grepl("/initiate$", url)) {
      httr2::response(
        status_code = 200L,
        headers = list("Content-Type" = "application/json"),
        body = charToRaw(jsonlite::toJSON(list(
          upload_id = "srv-u1",
          chunk_size_bytes = 512L
        ), auto_unbox = TRUE))
      )
    } else if (grepl("/chunk/", url)) {
      httr2::response(status_code = 200L, body = charToRaw(""))
    } else if (grepl("/complete$", url)) {
      httr2::response(
        status_code = 200L,
        headers = list("Content-Type" = "application/json"),
        body = charToRaw(jsonlite::toJSON(list(upload_id = "srv-u1"), auto_unbox = TRUE))
      )
    } else {
      httr2::response(status_code = 404L, body = charToRaw("Not found"))
    }
  })

  result <- CyteTypeR:::.upload_file_chunked(
    "https://api.example.com", "tok", "obs_duckdb", f, 60L,
    max_workers = 1L, show_progress = FALSE
  )

  expect_equal(result$upload_id, "srv-u1")

  urls <- vapply(request_log, `[[`, character(1), "url")
  expect_true(any(grepl("/obs_duckdb/initiate", urls)))
  expect_equal(sum(grepl("/chunk/", urls)), 2L)
  expect_true(any(grepl("/complete", urls)))

  init_body <- jsonlite::fromJSON(request_log[[which(grepl("/initiate", urls))]]$body, simplifyVector = FALSE)
  expect_equal(init_body$file_size_bytes, 1024)

  complete_body <- jsonlite::fromJSON(request_log[[which(grepl("/complete", urls))]]$body, simplifyVector = FALSE)
  expect_null(complete_body$r2_upload_id)
})

test_that("R2 upload: presigned PUTs -> complete with r2_upload_id and parts", {
  f <- tempfile(fileext = ".duckdb")
  on.exit(unlink(f), add = TRUE)
  writeBin(raw(1024), f)

  request_log <- list()

  httr2::local_mocked_responses(function(req) {
    url <- req$url
    request_log[[length(request_log) + 1L]] <<- list(url = url, method = req$method, body = .capture_body(req))

    if (grepl("/initiate$", url)) {
      httr2::response(
        status_code = 200L,
        headers = list("Content-Type" = "application/json"),
        body = charToRaw(jsonlite::toJSON(list(
          upload_id = "r2-u1",
          chunk_size_bytes = 512L,
          r2_upload_id = "r2-mp-xyz",
          presigned_urls = list(
            "https://r2.example.com/p/0?sig=a",
            "https://r2.example.com/p/1?sig=b"
          )
        ), auto_unbox = TRUE))
      )
    } else if (grepl("^https://r2\\.example\\.com/", url)) {
      etag <- if (grepl("p/0", url)) "\"etag-chunk0\"" else "\"etag-chunk1\""
      httr2::response(
        status_code = 200L,
        headers = list("ETag" = etag),
        body = charToRaw("")
      )
    } else if (grepl("/complete$", url)) {
      httr2::response(
        status_code = 200L,
        headers = list("Content-Type" = "application/json"),
        body = charToRaw(jsonlite::toJSON(list(upload_id = "r2-u1"), auto_unbox = TRUE))
      )
    } else {
      httr2::response(status_code = 404L, body = charToRaw("Not found"))
    }
  })

  result <- CyteTypeR:::.upload_file_chunked(
    "https://api.example.com", "tok", "obs_duckdb", f, 60L,
    max_workers = 1L, show_progress = FALSE
  )

  expect_equal(result$upload_id, "r2-u1")

  urls <- vapply(request_log, `[[`, character(1), "url")
  expect_equal(sum(grepl("r2\\.example\\.com", urls)), 2L)
  expect_equal(sum(grepl("/chunk/", urls)), 0L)

  complete_body <- jsonlite::fromJSON(request_log[[which(grepl("/complete", urls))]]$body, simplifyVector = FALSE)
  expect_null(complete_body$r2_upload_id)
  expect_length(complete_body$parts, 2L)
  expect_equal(complete_body$parts[[1]]$ETag, "\"etag-chunk0\"")
  expect_equal(complete_body$parts[[1]]$PartNumber, 1)
  expect_equal(complete_body$parts[[2]]$ETag, "\"etag-chunk1\"")
  expect_equal(complete_body$parts[[2]]$PartNumber, 2)
})

test_that("upload stops with error when a chunk fails", {
  f <- tempfile(fileext = ".duckdb")
  on.exit(unlink(f), add = TRUE)
  writeBin(raw(1024), f)

  httr2::local_mocked_responses(function(req) {
    url <- req$url
    if (grepl("/initiate$", url)) {
      httr2::response(
        status_code = 200L,
        headers = list("Content-Type" = "application/json"),
        body = charToRaw(jsonlite::toJSON(list(
          upload_id = "fail-u1",
          chunk_size_bytes = 512L
        ), auto_unbox = TRUE))
      )
    } else if (grepl("/chunk/0", url)) {
      httr2::response(status_code = 200L, body = charToRaw(""))
    } else if (grepl("/chunk/1", url)) {
      httr2::response(status_code = 400L, body = charToRaw("Bad Request"))
    } else {
      httr2::response(status_code = 200L, body = charToRaw(""))
    }
  })

  expect_error(
    CyteTypeR:::.upload_file_chunked(
      "https://api.example.com", NULL, "obs_duckdb", f, 60L,
      max_workers = 1L, show_progress = FALSE
    ),
    "Upload chunk.*failed"
  )
})

# --- .make_results_request tests ---

test_that(".make_results_request returns 'failed' when results endpoint errors", {
  call_count <- 0L
  testthat::local_mocked_bindings(
    .api_response_helper = function(job_id, api_url, req_item, auth_token = NULL) {
      call_count <<- call_count + 1L
      if (req_item == "status") {
        return(list(status_code = 200L, data = list(jobStatus = "completed")))
      }
      if (req_item == "results") {
        stop("HTTP 409 error: job still processing")
      }
    },
    .package = "CyteTypeR"
  )
  resp <- CyteTypeR:::.make_results_request("job1", "https://example.com")
  expect_identical(resp$status, "failed")
  expect_identical(resp$message, "Error while fetching job results: HTTP 409 error: job still processing")
  expect_s3_class(resp$condition, "cytetype_api_error")
})

test_that(".make_results_request returns 'not_found' on 404 status", {
  testthat::local_mocked_bindings(
    .api_response_helper = function(job_id, api_url, req_item, auth_token = NULL) {
      list(status_code = 404L, data = list())
    },
    .package = "CyteTypeR"
  )
  resp <- CyteTypeR:::.make_results_request("job1", "https://example.com")
  expect_identical(resp$status, "not_found")
})

test_that(".make_results_request returns 'error' on unexpected exception", {
  testthat::local_mocked_bindings(
    .api_response_helper = function(job_id, api_url, req_item, auth_token = NULL) {
      stop("connection refused")
    },
    .package = "CyteTypeR"
  )
  resp <- CyteTypeR:::.make_results_request("job1", "https://example.com")
  expect_identical(resp$status, "error")
  expect_true(grepl("connection refused", resp$message))
})

test_that(".make_results_request preserves authentication failures", {
  testthat::local_mocked_bindings(
    .api_response_helper = function(...) {
      CyteTypeR:::.stop_authentication(
        "Authentication failed: Invalid or expired auth token",
        "INVALID_TOKEN"
      )
    },
    .package = "CyteTypeR"
  )

  expect_error(
    CyteTypeR:::.make_results_request(
      "job1",
      "https://example.com",
      "expired-token"
    ),
    "Invalid or expired auth token",
    class = "cytetype_auth_error"
  )
})

# --- .poll_for_results tests ---

test_that("poll stops after consecutive 'error' statuses", {
  testthat::local_mocked_bindings(
    .make_results_request = function(...) {
      list(status = "error", result = NULL,
           message = "Error checking job status: connection refused",
           raw_response = NULL)
    },
    .sleep_with_spinner = function(...) invisible(NULL),
    .sleep = function(...) invisible(NULL),
    .package = "CyteTypeR"
  )
  expect_error(
    CyteTypeR:::.poll_for_results("job1", "https://example.com",
                                   poll_interval = 0, timeout = 600,
                                   show_progress = FALSE),
    "consecutive errors"
  )
})

test_that("poll stops after consecutive 'not_found' statuses", {
  testthat::local_mocked_bindings(
    .make_results_request = function(...) {
      list(status = "not_found", result = NULL,
           message = "Job not found",
           raw_response = NULL)
    },
    .sleep_with_spinner = function(...) invisible(NULL),
    .sleep = function(...) invisible(NULL),
    .package = "CyteTypeR"
  )
  expect_error(
    CyteTypeR:::.poll_for_results("job1", "https://example.com",
                                   poll_interval = 0, timeout = 600,
                                   show_progress = FALSE),
    "not found after"
  )
})

test_that("poll stops immediately on truly unknown status", {
  testthat::local_mocked_bindings(
    .make_results_request = function(...) {
      list(status = "something_new", result = NULL,
           message = "unknown",
           raw_response = NULL)
    },
    .sleep_with_spinner = function(...) invisible(NULL),
    .sleep = function(...) invisible(NULL),
    .package = "CyteTypeR"
  )
  expect_error(
    CyteTypeR:::.poll_for_results("job1", "https://example.com",
                                   poll_interval = 0, timeout = 600,
                                   show_progress = FALSE),
    "unexpected status"
  )
})

test_that("poll returns result on 'completed' status", {
  testthat::local_mocked_bindings(
    .make_results_request = function(...) {
      list(status = "completed",
           result = list(annotations = list()),
           message = "Job completed successfully",
           raw_response = list(clusterStatus = list("1" = "completed")))
    },
    .sleep_with_spinner = function(...) invisible(NULL),
    .sleep = function(...) invisible(NULL),
    .package = "CyteTypeR"
  )
  result <- CyteTypeR:::.poll_for_results("job1", "https://example.com",
                                           poll_interval = 0, timeout = 600,
                                           show_progress = FALSE)
  expect_true(is.list(result))
})

test_that("poll stops on 'failed' status with server message", {
  testthat::local_mocked_bindings(
    .make_results_request = function(...) {
      list(status = "failed", result = NULL,
           message = "Job failed",
           raw_response = list(clusterStatus = list()))
    },
    .sleep_with_spinner = function(...) invisible(NULL),
    .sleep = function(...) invisible(NULL),
    .package = "CyteTypeR"
  )
  expect_error(
    CyteTypeR:::.poll_for_results("job1", "https://example.com",
                                   poll_interval = 0, timeout = 600,
                                   show_progress = FALSE),
    "Server error.*Job failed"
  )
})

# --- timeout, retries and hints ---

# A clock that advances `step` seconds on every call.
fake_clock <- function(step) {
  now <- as.POSIXct("2026-01-01 00:00:00", tz = "UTC")
  function() {
    current <- now
    now <<- now + step
    current
  }
}

test_that("the polling timeout compares seconds, whatever the difftime units", {
  # Each check advances two minutes. With automatic difftime units, 0.5.1 read
  # this as 2, 4, 6, ... "minutes" and never reached a 600 second timeout.
  calls <- 0L
  testthat::local_mocked_bindings(
    .make_results_request = function(...) {
      calls <<- calls + 1L
      if (calls > 50L) stop("the timeout never fired")
      list(status = "processing", result = NULL, message = "Job is processing",
           raw_response = list(clusterStatus = list()))
    },
    .now = fake_clock(120),
    .sleep = function(...) invisible(NULL),
    .sleep_with_spinner = function(...) invisible(NULL),
    .package = "CyteTypeR"
  )

  error <- tryCatch(
    CyteTypeR:::.poll_for_results("job1", "https://example.com",
                                   poll_interval = 0, timeout = 600,
                                   show_progress = FALSE),
    error = identity
  )

  expect_s3_class(error, "cytetype_timeout_error")
  expect_identical(error$job_id, "job1")
  expect_match(conditionMessage(error), "Job job1 did not finish within 600 seconds")
  expect_match(conditionMessage(error), 'GetResults(job_id = "job1")', fixed = TRUE)
  expect_false(grepl("timeoutTimeout", conditionMessage(error)))
  expect_lt(calls, 10L)
})

test_that("network errors are retried until the timeout", {
  calls <- 0L
  testthat::local_mocked_bindings(
    .make_results_request = function(...) {
      calls <<- calls + 1L
      if (calls <= 8L) {
        return(list(status = "error", result = NULL,
                    message = "Error checking job status: Could not connect to server",
                    raw_response = NULL, error_type = "network"))
      }
      list(status = "completed", result = list(annotations = list()),
           message = "Job completed successfully", raw_response = list(clusterStatus = list()))
    },
    .sleep = function(...) invisible(NULL),
    .sleep_with_spinner = function(...) invisible(NULL),
    .package = "CyteTypeR"
  )

  result <- CyteTypeR:::.poll_for_results("job1", "https://example.com",
                                           poll_interval = 0, timeout = 600,
                                           show_progress = FALSE)

  expect_identical(result, list(annotations = list()))
  expect_identical(calls, 9L)
})

test_that("other errors stop after five in a row with a GetResults hint", {
  testthat::local_mocked_bindings(
    .make_results_request = function(...) {
      list(status = "error", result = NULL,
           message = "Error checking job status: HTTP 500 [INTERNAL_ERROR] boom",
           raw_response = NULL, error_type = "api")
    },
    .sleep = function(...) invisible(NULL),
    .sleep_with_spinner = function(...) invisible(NULL),
    .package = "CyteTypeR"
  )

  expect_error(
    CyteTypeR:::.poll_for_results("job1", "https://example.com",
                                   poll_interval = 0, timeout = 600,
                                   show_progress = FALSE),
    'Stopping after 5 consecutive errors.*GetResults\\(job_id = "job1"\\)'
  )
})

test_that(".make_results_request marks connection failures as network errors", {
  testthat::local_mocked_bindings(
    .api_response_helper = function(...) {
      stop(cytetype_api_error("Error while checking job status: Could not connect", call = "network"))
    },
    .package = "CyteTypeR"
  )

  response <- CyteTypeR:::.make_results_request("job1", "https://example.com")

  expect_identical(response$status, "error")
  expect_identical(response$error_type, "network")
})

test_that(".make_results_request reports HTTP 202 results as processing", {
  testthat::local_mocked_bindings(
    .api_response_helper = function(job_id, api_url, req_item, auth_token = NULL) {
      if (req_item == "status") {
        return(list(status_code = 200L, data = list(jobStatus = "completed")))
      }
      list(status_code = 202L, data = list(detail = list(error_code = "JOB_PROCESSING")))
    },
    .package = "CyteTypeR"
  )

  response <- CyteTypeR:::.make_results_request("job1", "https://example.com")

  expect_identical(response$status, "processing")
})

test_that("the hint printed while polling names the job ID", {
  testthat::local_mocked_bindings(
    .make_results_request = function(...) {
      list(status = "completed", result = list(annotations = list()),
           message = "Job completed successfully", raw_response = list(clusterStatus = list()))
    },
    .sleep = function(...) invisible(NULL),
    .package = "CyteTypeR"
  )

  output <- capture.output(
    result <- CyteTypeR:::.poll_for_results("job-42", "https://example.com",
                                   poll_interval = 0, timeout = 600,
                                   show_progress = FALSE),
    type = "message"
  )

  expect_true(any(grepl('GetResults(job_id = "job-42")', output, fixed = TRUE)))
})

test_that("repeated status errors stop the wait with a cytetype_api_error", {
  testthat::local_mocked_bindings(
    .make_results_request = function(...) {
      condition <- cytetype_api_error(
        "Error while checking job status: HTTP 503 [HTTP_ERROR] Service unavailable",
        call = "api",
        error_code = "HTTP_ERROR"
      )
      list(status = "error", result = NULL, message = conditionMessage(condition),
           raw_response = NULL, error_type = "api", condition = condition)
    },
    .sleep = function(...) invisible(NULL),
    .sleep_with_spinner = function(...) invisible(NULL),
    .package = "CyteTypeR"
  )

  error <- tryCatch(
    CyteTypeR:::.poll_for_results("job1", "https://example.com",
                                   poll_interval = 0, timeout = 600,
                                   show_progress = FALSE),
    error = identity
  )

  expect_s3_class(error, "cytetype_api_error")
  expect_identical(error$error_code, "HTTP_ERROR")
  expect_match(conditionMessage(error), "^Stopping after 5 consecutive errors: Error while checking job status: HTTP 503")
  # The context appears once.
  expect_identical(lengths(regmatches(conditionMessage(error), gregexpr("Error while checking job status", conditionMessage(error)))), 1L)
})

test_that("a results fetch failure stops the wait with the cytetype_api_error", {
  testthat::local_mocked_bindings(
    .api_response_helper = function(job_id, api_url, req_item, auth_token = NULL) {
      if (req_item == "status") {
        return(list(status_code = 200L, data = list(jobStatus = "completed", clusterStatus = list())))
      }
      stop(cytetype_api_error(
        "Error while fetching job results: HTTP 410 [REPORT_DELETED] This report has been deleted by the owner",
        call = "api",
        error_code = "REPORT_DELETED"
      ))
    },
    .sleep = function(...) invisible(NULL),
    .sleep_with_spinner = function(...) invisible(NULL),
    .package = "CyteTypeR"
  )

  error <- tryCatch(
    CyteTypeR:::.poll_for_results("job1", "https://example.com",
                                   poll_interval = 0, timeout = 600,
                                   show_progress = FALSE),
    error = identity
  )

  expect_s3_class(error, "cytetype_api_error")
  expect_identical(error$error_code, "REPORT_DELETED")
  expect_identical(
    conditionMessage(error),
    "Error while fetching job results: HTTP 410 [REPORT_DELETED] This report has been deleted by the owner"
  )
})

# --- Results of a completed job: transient errors are retried ---

http_json <- function(status_code, body) {
  httr2::response(
    status_code = status_code,
    headers = list("Content-Type" = "application/json"),
    body = charToRaw(as.character(jsonlite::toJSON(body, auto_unbox = TRUE, null = "null")))
  )
}

server_error <- function(status_code, error_code, message) {
  http_json(status_code, list(detail = list(error_code = error_code, message = message)))
}

no_connection <- function() {
  stop(structure(
    list(message = "Could not connect to server", call = NULL),
    class = c("httr2_failure", "httr2_error", "error", "condition")
  ))
}

# The status endpoint reports a completed job. The results endpoint answers
# with the elements of `results` in turn, repeating the last one; a function
# is called instead, to fail without a response.
mock_completed_job <- function(results, env = parent.frame()) {
  calls <- new.env()
  calls$results <- 0L
  httr2::local_mocked_responses(function(req) {
    if (grepl("/status/", req$url)) {
      return(http_json(200L, list(jobStatus = "completed", clusterStatus = list())))
    }
    calls$results <- calls$results + 1L
    answer <- results[[min(calls$results, length(results))]]
    if (is.function(answer)) answer() else answer
  }, env = env)
  calls
}

test_that("only transient results fetch errors are retried while waiting", {
  cases <- list(
    list(answer = server_error(500L, "INTERNAL_ERROR", "boom"), status = "error"),
    list(answer = http_json(502L, list(detail = "Bad gateway")), status = "error"),
    list(answer = server_error(503L, "HTTP_ERROR", "Service unavailable"), status = "error"),
    list(answer = http_json(504L, list(detail = "Gateway timeout")), status = "error"),
    list(answer = server_error(429L, "RATE_LIMIT_EXCEEDED", "Hourly limit reached (5/5)."), status = "error"),
    list(answer = no_connection, status = "error"),
    list(answer = server_error(410L, "REPORT_DELETED", "This report has been deleted by the owner"), status = "failed"),
    list(answer = http_json(400L, list(detail = "Bad request")), status = "failed"),
    list(answer = httr2::response(status_code = 404L), status = "failed")
  )
  for (case in cases) {
    mock_completed_job(list(case$answer))

    response <- CyteTypeR:::.make_results_request("job1", "https://example.com")

    label <- if (is.function(case$answer)) "no response" else as.character(httr2::resp_status(case$answer))
    expect_identical(response$status, case$status, info = label)
  }
})

test_that("a transient results error followed by success returns the results", {
  calls <- mock_completed_job(list(
    server_error(503L, "HTTP_ERROR", "Service unavailable"),
    http_json(200L, list(annotations = list(list(clusterId = "1", annotation = "T cell"))))
  ))
  testthat::local_mocked_bindings(
    .sleep = function(...) invisible(NULL),
    .sleep_with_spinner = function(...) invisible(NULL),
    .package = "CyteTypeR"
  )

  result <- CyteTypeR:::.poll_for_results("job1", "https://example.com",
                                           poll_interval = 0, timeout = 600,
                                           show_progress = FALSE)

  expect_identical(result$annotations[[1]]$annotation, "T cell")
  expect_identical(calls$results, 2L)
})

test_that("a deleted report stops the wait at the first results request", {
  calls <- mock_completed_job(list(
    server_error(410L, "REPORT_DELETED", "This report has been deleted by the owner")
  ))
  testthat::local_mocked_bindings(
    .sleep = function(...) invisible(NULL),
    .sleep_with_spinner = function(...) invisible(NULL),
    .package = "CyteTypeR"
  )

  error <- tryCatch(
    CyteTypeR:::.poll_for_results("job1", "https://example.com",
                                   poll_interval = 0, timeout = 600,
                                   show_progress = FALSE),
    error = identity
  )

  expect_s3_class(error, "cytetype_api_error")
  expect_identical(error$error_code, "REPORT_DELETED")
  expect_identical(error$status_code, 410L)
  expect_identical(
    conditionMessage(error),
    "Error while fetching job results: HTTP 410 [REPORT_DELETED] This report has been deleted by the owner"
  )
  expect_identical(calls$results, 1L)
})

test_that("five transient results errors in a row stop the wait", {
  calls <- mock_completed_job(list(
    server_error(503L, "HTTP_ERROR", "Service unavailable")
  ))
  testthat::local_mocked_bindings(
    .sleep = function(...) invisible(NULL),
    .sleep_with_spinner = function(...) invisible(NULL),
    .package = "CyteTypeR"
  )

  error <- tryCatch(
    CyteTypeR:::.poll_for_results("job1", "https://example.com",
                                   poll_interval = 0, timeout = 600,
                                   show_progress = FALSE),
    error = identity
  )

  expect_s3_class(error, "cytetype_api_error")
  expect_identical(
    conditionMessage(error),
    paste0(
      "Stopping after 5 consecutive errors: Error while fetching job results: ",
      "HTTP 503 [HTTP_ERROR] Service unavailable\n",
      "The job may still be running; retrieve its results later with ",
      'GetResults(job_id = "job1").'
    )
  )
  expect_identical(error$error_code, "HTTP_ERROR")
  expect_identical(error$status_code, 503L)
  expect_identical(calls$results, 5L)
})

# --- The deadline ---

# A clock that moves only when the poll waits or a mocked request takes time.
# It records every wait.
fake_timeline <- function() {
  start <- as.POSIXct("2026-01-01 00:00:00", tz = "UTC")
  state <- new.env()
  state$now <- start
  state$waits <- numeric()
  list(
    now = function() state$now,
    advance = function(seconds) state$now <- state$now + seconds,
    wait = function(seconds, ...) {
      state$waits <- c(state$waits, seconds)
      state$now <- state$now + seconds
      invisible(NULL)
    },
    waits = function() state$waits,
    elapsed = function() as.numeric(difftime(state$now, start, units = "secs"))
  )
}

# Poll with the clock, answering every request with `respond()`.
poll_with_clock <- function(clock, respond, poll_interval, timeout) {
  testthat::with_mocked_bindings(
    tryCatch(
      CyteTypeR:::.poll_for_results("job1", "https://example.com",
                                     poll_interval = poll_interval,
                                     timeout = timeout,
                                     show_progress = FALSE),
      error = identity
    ),
    .make_results_request = function(...) respond(),
    .now = clock$now,
    .sleep = clock$wait,
    .sleep_with_spinner = clock$wait,
    .package = "CyteTypeR"
  )
}

test_that("no wait runs past the deadline", {
  answer <- function(status, error_type = NULL) {
    list(status = status, result = NULL, message = paste("Job is", status),
         raw_response = list(clusterStatus = list()), error_type = error_type)
  }
  # timeout 12 s, poll interval 10 s: the 5 s initial wait, then waits cut to
  # the 7 s left. Network errors are retried every 5 s.
  cases <- list(
    list(answer = answer("processing"), waits = c(5, 7)),
    list(answer = answer("pending"), waits = c(5, 7)),
    list(answer = answer("not_found"), waits = c(5, 7)),
    list(answer = answer("error", "api"), waits = c(5, 7)),
    list(answer = answer("error", "network"), waits = c(5, 5, 2))
  )
  for (case in cases) {
    clock <- fake_timeline()
    requests <- 0L

    error <- poll_with_clock(clock, function() {
      requests <<- requests + 1L
      if (requests > 20L) stop("the timeout never fired")
      case$answer
    }, poll_interval = 10, timeout = 12)

    label <- paste(case$answer$status, case$answer$error_type)
    expect_s3_class(error, "cytetype_timeout_error")
    expect_identical(clock$waits(), case$waits, info = label)
    # The deadline is reached exactly, not overshot.
    expect_identical(clock$elapsed(), 12, info = label)
    expect_identical(requests, length(case$waits) - 1L, info = label)
  }
})

test_that("a timeout shorter than the initial wait ends it", {
  clock <- fake_timeline()
  requests <- 0L

  error <- poll_with_clock(clock, function() {
    requests <<- requests + 1L
    stop("no request is made after the deadline")
  }, poll_interval = 10, timeout = 3)

  expect_s3_class(error, "cytetype_timeout_error")
  expect_identical(clock$waits(), 3)
  expect_identical(requests, 0L)
})

test_that("results found by a request that ends after the deadline are returned", {
  clock <- fake_timeline()
  requests <- 0L

  result <- poll_with_clock(clock, function() {
    requests <<- requests + 1L
    if (requests == 1L) {
      return(list(status = "processing", result = NULL, message = "Job is processing",
                  raw_response = list(clusterStatus = list())))
    }
    # Starts at 15 s, before the 20 s deadline, and ends at 23 s.
    clock$advance(8)
    list(status = "completed", result = list(annotations = list()),
         message = "Job completed successfully", raw_response = list(clusterStatus = list()))
  }, poll_interval = 10, timeout = 20)

  expect_identical(result, list(annotations = list()))
  expect_identical(clock$waits(), c(5, 10))
  expect_identical(clock$elapsed(), 23)
})

test_that("the spinner sleeps the whole wait, including a last partial step", {
  waits <- numeric()
  testthat::local_mocked_bindings(
    .sleep = function(seconds) waits <<- c(waits, seconds),
    .package = "CyteTypeR"
  )

  CyteTypeR:::.sleep_with_spinner(1.2, list(), show_progress = FALSE)
  expect_equal(waits, c(0.5, 0.5, 0.2))

  waits <- numeric()
  CyteTypeR:::.sleep_with_spinner(0, list(), show_progress = FALSE)
  expect_length(waits, 0L)
})
