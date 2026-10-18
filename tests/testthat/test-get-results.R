# GetResults() by job ID: status first, NULL while running, optional JSON file.

mock_results_api <- function(status = "completed", results_status = 200L, results_body = results_response()) {
  requests <- character()
  httr2::local_mocked_responses(function(req) {
    requests <<- c(requests, req$url)
    json <- function(code, body) {
      httr2::response(
        status_code = code,
        headers = list("Content-Type" = "application/json"),
        body = charToRaw(as.character(jsonlite::toJSON(body, auto_unbox = TRUE, null = "null")))
      )
    }
    if (grepl("/status/", req$url)) {
      return(json(200L, list(jobStatus = status, clusterStatus = list(`1` = status))))
    }
    if (grepl("/results/", req$url)) {
      if (results_status == 202L) {
        return(json(202L, list(detail = list(
          error_code = "JOB_PROCESSING",
          message = "Job job-1 is still processing. Please check status endpoint for progress"
        ))))
      }
      return(json(results_status, results_body))
    }
    httr2::response(status_code = 404L)
  }, env = parent.frame())
  environment()
}

test_that("GetResults(job_id) on a running job returns NULL with a message", {
  local_temp_workdir()
  api <- mock_results_api(status = "processing")

  expect_message(
    result <- GetResults(job_id = "job-1", api_url = "https://api.example.com", auth_token = "tok"),
    "still processing"
  )

  expect_null(result)
  expect_false(file.exists("cytetypeR_results_job-1.json"))
  # Only the status was requested.
  expect_identical(api$requests, "https://api.example.com/status/job-1")
})

test_that("GetResults(job_id) treats HTTP 202 from the results endpoint as still running", {
  local_temp_workdir()
  mock_results_api(status = "completed", results_status = 202L)

  expect_message(
    result <- GetResults(job_id = "job-1", api_url = "https://api.example.com", auth_token = "tok"),
    "GetResults\\(job_id = \"job-1\"\\)"
  )
  expect_null(result)
  expect_false(file.exists("cytetypeR_results_job-1.json"))
})

test_that("GetResults(job_id) returns the normalized table and can skip the JSON file", {
  local_temp_workdir()
  mock_results_api(status = "completed")

  table <- GetResults(
    job_id = "job-1",
    api_url = "https://api.example.com",
    auth_token = "tok",
    save_json = FALSE
  )

  expect_s3_class(table, "data.frame")
  expect_identical(names(table), c(EXISTING_TABLE_COLUMNS, SDK_KEYS))
  # Without the object there is no label map, so clusterId is the CyteType ID.
  expect_identical(table$clusterId, c("1", "2", "3"))
  expect_identical(table$supportingMarkers[3], "CD8A; CD8B; GZMK")
  expect_identical(table$reviewerConfidence, c("High", "Moderate", "Low"))
  expect_identical(attr(table, "job_id"), "job-1")
  expect_identical(names(attr(table, "result")$raw_annotations), as.character(1:5))
  expect_length(list.files(), 0L)
})

test_that("GetResults(job_id) saves the server's response by default", {
  local_temp_workdir()
  mock_results_api(status = "completed")

  GetResults(job_id = "job-1", api_url = "https://api.example.com", auth_token = "tok")

  expect_true(file.exists("cytetypeR_results_job-1.json"))
  saved <- jsonlite::fromJSON("cytetypeR_results_job-1.json", simplifyVector = FALSE)
  expect_identical(names(saved$annotations), as.character(1:5))
})

test_that("GetResults(job_id) stops when the job failed", {
  local_temp_workdir()
  mock_results_api(status = "failed")

  expect_error(
    GetResults(job_id = "job-1", api_url = "https://api.example.com", auth_token = "tok"),
    "Job failed"
  )
})

test_that("GetResults(job_id) returns NULL with a message when the job is unknown", {
  local_temp_workdir()
  httr2::local_mocked_responses(function(req) {
    httr2::response(status_code = 404L)
  })

  expect_message(
    result <- GetResults(job_id = "missing", api_url = "https://api.example.com", auth_token = "tok"),
    "was not found"
  )
  expect_null(result)
})

test_that("GetResults(obj) messages while the stored job is still running", {
  obj <- SeuratObject::CreateSeuratObject(
    Matrix::sparseMatrix(i = c(1L, 2L), j = c(1L, 2L), x = c(1, 1), dims = c(2L, 2L))
  )
  obj@misc$cytetype_jobDetails <- list(job_id = "job-1", api_url = "https://api.example.com")
  testthat::local_mocked_bindings(
    .make_results_request = function(...) list(status = "processing", result = NULL),
    .package = "CyteTypeR"
  )

  expect_message(result <- GetResults(obj, auth_token = "tok", api_url = "https://api.example.com"), "still processing")
  expect_null(result)
})

test_that("GetResults(obj) normalizes results fetched for the stored job", {
  obj <- SeuratObject::CreateSeuratObject(
    Matrix::sparseMatrix(i = c(1L, 2L), j = c(1L, 2L), x = c(1, 1), dims = c(2L, 2L))
  )
  obj@misc$cytetype_jobDetails <- list(job_id = "job-1", api_url = "https://api.example.com")
  testthat::local_mocked_bindings(
    .make_results_request = function(...) {
      list(status = "completed", result = as_api_json(results_response()))
    },
    .package = "CyteTypeR"
  )

  result <- GetResults(obj, auth_token = "tok", api_url = "https://api.example.com")

  expect_type(result, "list")
  annotations <- by_cluster(result)
  expect_identical(legacy_values(annotations[["3"]]), EXPECTED_FILL)
  expect_identical(annotations[["3"]]$reviewerConfidence, "Low")
  expect_identical(result$semanticOrder, list("1", "2", "3"))
})

# --- Errors while fetching the status or the results ---

json_response <- function(code, body) {
  httr2::response(
    status_code = code,
    headers = list("Content-Type" = "application/json"),
    body = charToRaw(as.character(jsonlite::toJSON(body, auto_unbox = TRUE, null = "null")))
  )
}

report_deleted <- function(req) {
  json_response(410L, list(detail = list(
    error_code = "REPORT_DELETED",
    message = "This report has been deleted by the owner"
  )))
}

test_that("GetResults(job_id) signals one clean cytetype_api_error for a deleted report", {
  local_temp_workdir()
  httr2::local_mocked_responses(report_deleted)

  error <- tryCatch(
    GetResults(job_id = "job-1", api_url = "https://api.example.com", auth_token = "tok"),
    error = identity
  )

  expect_s3_class(error, "cytetype_api_error")
  expect_false(inherits(error, "cytetype_auth_error"))
  expect_identical(
    conditionMessage(error),
    "Error while checking job status: HTTP 410 [REPORT_DELETED] This report has been deleted by the owner"
  )
  expect_identical(error$error_code, "REPORT_DELETED")
  expect_identical(error$call, "api")
  # Printed as "Error: <message>", not 'Error in "api" : <message>'.
  expect_null(conditionCall(error))
})

test_that("GetResults(obj) signals the same condition for a stored job", {
  obj <- SeuratObject::CreateSeuratObject(
    Matrix::sparseMatrix(i = c(1L, 2L), j = c(1L, 2L), x = c(1, 1), dims = c(2L, 2L))
  )
  obj@misc$cytetype_jobDetails <- list(job_id = "job-1", api_url = "https://api.example.com")
  httr2::local_mocked_responses(report_deleted)

  expect_error(
    GetResults(obj, auth_token = "tok", api_url = "https://api.example.com"),
    "^Error while checking job status: HTTP 410 \\[REPORT_DELETED\\] This report has been deleted by the owner$",
    class = "cytetype_api_error"
  )
})

test_that("a failed results request is a cytetype_api_error with the server's code", {
  local_temp_workdir()
  httr2::local_mocked_responses(function(req) {
    if (grepl("/status/", req$url)) {
      return(json_response(200L, list(jobStatus = "completed")))
    }
    json_response(500L, list(detail = list(
      error_code = "INTERNAL_ERROR",
      message = "An unexpected error occurred: disk full"
    )))
  })

  error <- tryCatch(
    GetResults(job_id = "job-1", api_url = "https://api.example.com", auth_token = "tok"),
    error = identity
  )

  expect_s3_class(error, "cytetype_api_error")
  expect_identical(
    conditionMessage(error),
    "Error while fetching job results: HTTP 500 [INTERNAL_ERROR] An unexpected error occurred: disk full"
  )
  expect_identical(error$error_code, "INTERNAL_ERROR")
  expect_false(file.exists("cytetypeR_results_job-1.json"))
})

test_that("a connection failure is a network cytetype_api_error with one prefix", {
  httr2::local_mocked_responses(function(req) {
    stop(structure(
      list(message = "Could not connect to server", call = NULL),
      class = c("httr2_failure", "httr2_error", "error", "condition")
    ))
  })

  error <- tryCatch(
    GetResults(job_id = "job-1", api_url = "https://api.example.com", auth_token = "tok"),
    error = identity
  )

  expect_s3_class(error, "cytetype_api_error")
  expect_identical(conditionMessage(error), "Error while checking job status: Could not connect to server")
  expect_identical(error$call, "network")
  expect_null(error$error_code)
})

test_that("a rate-limited status check carries the code and the advice", {
  httr2::local_mocked_responses(function(req) {
    json_response(429L, list(detail = list(
      error_code = "RATE_LIMIT_EXCEEDED",
      message = "Daily limit reached (10/10). Resets in about 5 hours."
    )))
  })

  error <- tryCatch(
    GetResults(job_id = "job-1", api_url = "https://api.example.com", auth_token = "tok"),
    error = identity
  )

  expect_s3_class(error, "cytetype_api_error")
  expect_identical(error$error_code, "RATE_LIMIT_EXCEEDED")
  expect_match(conditionMessage(error), "^Daily limit reached \\(10/10\\)")
  expect_match(conditionMessage(error), "including jobs that use your own LLM", fixed = TRUE)
})

test_that("authentication failures are still cytetype_auth_error", {
  for (code in c(401L, 403L)) {
    httr2::local_mocked_responses(function(req) {
      json_response(code, list(detail = "Invalid API token"))
    })
    expect_error(
      GetResults(job_id = "job-1", api_url = "https://api.example.com", auth_token = "tok"),
      class = "cytetype_auth_error"
    )
  }
})
