test_that(".MAX_UPLOAD_BYTES has expected limits", {
  lim <- CyteTypeR:::.MAX_UPLOAD_BYTES
  expect_identical(lim$obs_duckdb, 100L * 1024L * 1024L)
  expect_equal(lim$vars_h5, 50 * 1024 * 1024 * 1024)
  expect_true(is.numeric(lim$vars_h5), info = "vars_h5 must be numeric to avoid integer overflow")
})

test_that("chunked upload retry constants are as expected", {
  expect_identical(CyteTypeR:::.CHUNK_UPLOAD_BACKOFF_SECS, c(1L, 5L, 20L))
  expect_identical(CyteTypeR:::.CHUNK_UPLOAD_TRANSIENT_STATUSES, c(500L, 502L, 503L, 504L))
})

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
  expect_true(grepl("results unavailable", resp$message))
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

# --- .poll_for_results tests ---

test_that("poll stops after consecutive 'error' statuses", {
  testthat::local_mocked_bindings(
    .make_results_request = function(...) {
      list(status = "error", result = NULL,
           message = "Error checking job status: connection refused",
           raw_response = NULL)
    },
    .sleep_with_spinner = function(...) invisible(NULL),
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
    .package = "CyteTypeR"
  )
  expect_error(
    CyteTypeR:::.poll_for_results("job1", "https://example.com",
                                   poll_interval = 0, timeout = 600,
                                   show_progress = FALSE),
    "Server error.*Job failed"
  )
})
