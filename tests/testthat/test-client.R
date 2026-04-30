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
    "Upload of obs_duckdb failed: chunk 2/2 \\(1 of 2 chunks failed\\)"
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
