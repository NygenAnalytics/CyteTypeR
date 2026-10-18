# --- Constants ---

test_that(".MAX_UPLOAD_BYTES has expected limits", {
  lim <- CyteTypeR:::.MAX_UPLOAD_BYTES
  expect_equal(lim$obs_duckdb, 2 * 1024 * 1024 * 1024)
  expect_equal(lim$vars_h5, 50 * 1024 * 1024 * 1024)
  expect_true(is.numeric(lim$obs_duckdb))
  expect_true(is.numeric(lim$vars_h5))
})

test_that("chunked upload retry constants are as expected", {
  expect_identical(CyteTypeR:::.CHUNK_UPLOAD_BACKOFF_SECS, c(1L, 5L, 20L))
  expect_identical(CyteTypeR:::.CHUNK_UPLOAD_TRANSIENT_STATUSES, c(500L, 502L, 503L, 504L))
})

# --- .url_path ---

test_that(".url_path joins segments and strips slashes", {
  expect_identical(
    CyteTypeR:::.url_path("https://api.example.com", "upload", "123"),
    "https://api.example.com/upload/123"
  )
})

test_that(".url_path strips leading and trailing slashes from each segment", {
  expect_identical(
    CyteTypeR:::.url_path("https://api.example.com/", "/upload/", "/123/"),
    "https://api.example.com/upload/123"
  )
})

test_that(".url_path drops empty segments", {
  expect_identical(
    CyteTypeR:::.url_path("https://api.example.com", "", "upload"),
    "https://api.example.com/upload"
  )
})

test_that(".url_path handles single segment", {
  expect_identical(
    CyteTypeR:::.url_path("https://api.example.com"),
    "https://api.example.com"
  )
})

# --- .make_req ---

test_that(".make_req builds URL from base and path", {
  req <- CyteTypeR:::.make_req("https://api.example.com", "v1/jobs", NULL)
  expect_identical(req$url, "https://api.example.com/v1/jobs")
})

test_that(".make_req adds Authorization header when token provided", {
  req <- CyteTypeR:::.make_req("https://api.example.com", "v1/jobs", "my-token")
  expect_true("Authorization" %in% names(req$headers))
})

test_that(".make_req omits Authorization header when token is NULL", {
  req <- CyteTypeR:::.make_req("https://api.example.com", "v1/jobs", NULL)
  expect_false("Authorization" %in% names(req$headers))
})

test_that(".api_response_helper preserves authentication failures", {
  httr2::local_mocked_responses(function(req) {
    httr2::response(
      status_code = 401L,
      headers = list("Content-Type" = "application/json"),
      body = charToRaw('{"detail":"Invalid API token"}')
    )
  })

  expect_error(
    CyteTypeR:::.api_response_helper(
      "job-123",
      "https://api.example.com",
      "results",
      "expired-token"
    ),
    "Invalid or expired auth token",
    class = "cytetype_auth_error"
  )
})

test_that(".api_response_helper preserves rate-limit guidance", {
  httr2::local_mocked_responses(function(req) {
    httr2::response(
      status_code = 429L,
      headers = list("Content-Type" = "application/json"),
      body = charToRaw(
        paste0(
          '{"detail":{"error_code":"RATE_LIMIT_EXCEEDED",',
          '"message":"Free-tier limit reached"}}'
        )
      )
    )
  })

  rate_limit_error <- tryCatch(
    CyteTypeR:::.api_response_helper(
      "job-123",
      "https://api.example.com",
      "results"
    ),
    error = identity
  )

  expect_s3_class(rate_limit_error, "error")
  expect_identical(
    conditionMessage(rate_limit_error),
    paste0(
      "Free-tier limit reached\n",
      "The limits apply to every job, including jobs that use your own LLM ",
      "through llm_configs. Wait for the limit to reset before retrying."
    )
  )
})

test_that("the rate-limit message never says your own LLM bypasses the limits", {
  httr2::local_mocked_responses(function(req) {
    httr2::response(
      status_code = 429L,
      headers = list("Content-Type" = "application/json"),
      body = charToRaw(paste0(
        '{"detail":{"error_code":"RATE_LIMIT_EXCEEDED",',
        '"message":"Daily limit reached (10/10). Resets in about 5 hours."}}'
      ))
    )
  })

  rate_limit_error <- tryCatch(
    CyteTypeR:::.submit_job(list(input_data = list()), "https://api.example.com", "tok"),
    error = identity
  )

  message <- conditionMessage(rate_limit_error)
  expect_match(message, "Daily limit reached (10/10)", fixed = TRUE)
  expect_match(message, "including jobs that use your own LLM", fixed = TRUE)
  expect_false(grepl("bypass", message, ignore.case = TRUE))
})

test_that(".api_response_helper classifies connection failures as network errors", {
  httr2::local_mocked_responses(function(req) {
    stop(structure(
      list(message = "Could not connect to server", call = NULL),
      class = c("httr2_failure", "httr2_error", "error", "condition")
    ))
  })

  error <- tryCatch(
    CyteTypeR:::.api_response_helper("job-123", "https://api.example.com", "status"),
    error = identity
  )

  expect_s3_class(error, "cytetype_api_error")
  expect_identical(error$call, "network")
})

# --- .put_to_presigned_url ---

test_that(".put_to_presigned_url returns ETag on successful PUT", {
  httr2::local_mocked_responses(function(req) {
    httr2::response(
      status_code = 200L,
      headers = list("ETag" = "\"abc123\""),
      body = charToRaw("")
    )
  })
  etag <- CyteTypeR:::.put_to_presigned_url(
    "https://r2.example.com/bucket/chunk/0?sig=xyz",
    charToRaw("chunk-data"),
    60L
  )
  expect_identical(etag, "\"abc123\"")
})

test_that(".put_to_presigned_url raises api error on HTTP 4xx", {
  httr2::local_mocked_responses(function(req) {
    httr2::response(status_code = 403L, body = charToRaw("Forbidden"))
  })
  expect_error(
    CyteTypeR:::.put_to_presigned_url(
      "https://r2.example.com/bucket/chunk/0?sig=xyz",
      charToRaw("chunk-data"),
      60L
    ),
    "Presigned upload rejected with HTTP 403",
    class = "cytetype_api_error"
  )
})

test_that(".put_to_presigned_url raises network error when ETag header is missing", {
  httr2::local_mocked_responses(function(req) {
    httr2::response(status_code = 200L, body = charToRaw(""))
  })
  expect_error(
    CyteTypeR:::.put_to_presigned_url(
      "https://r2.example.com/bucket/chunk/0?sig=xyz",
      charToRaw("chunk-data"),
      60L
    ),
    "missing ETag header",
    class = "cytetype_api_error"
  )
})

test_that(".put_to_presigned_url raises api error on HTTP 400", {
  httr2::local_mocked_responses(function(req) {
    httr2::response(status_code = 400L, body = charToRaw("Bad Request"))
  })
  expect_error(
    CyteTypeR:::.put_to_presigned_url(
      "https://r2.example.com/bucket/chunk/0?sig=xyz",
      charToRaw("chunk-data"),
      60L
    ),
    "Presigned upload rejected with HTTP 400",
    class = "cytetype_api_error"
  )
})

# --- Server error bodies, as FastAPI sends them for the CyteType server ---

# 400 from a route wrapped by the server's capture_core_errors: a plain
# HTTPException detail is turned into {"error_code": "HTTP_ERROR", ...}.
ORG_VISIBILITY_WRAPPED <- paste0(
  '{"detail": {"error_code": "HTTP_ERROR", ',
  '"message": "Org visibility requires an organization license"}}'
)
# An HTTPException outside a wrapped route keeps its plain text detail.
ORG_VISIBILITY_PLAIN <- '{"detail": "Org visibility requires an organization license"}'
# FastAPI's request validation error (HTTP 422).
VISIBILITY_422 <- paste0(
  '{"detail": [{"type": "literal_error", "loc": ["body", "visibility"], ',
  '"msg": "Input should be \'private\', \'org\' or \'public\'", "input": "shared", ',
  '"ctx": {"expected": "\'private\', \'org\' or \'public\'"}}]}'
)

body_response <- function(status_code, body) {
  httr2::response(
    status_code = status_code,
    headers = list("Content-Type" = "application/json"),
    body = charToRaw(body)
  )
}

test_that(".parse_server_error reads every error body the server sends", {
  parse <- function(status_code, body) {
    CyteTypeR:::.parse_server_error(body_response(status_code, body))
  }

  expect_identical(
    parse(400L, ORG_VISIBILITY_WRAPPED),
    list(error_code = "HTTP_ERROR", message = "Org visibility requires an organization license")
  )
  expect_identical(
    parse(400L, ORG_VISIBILITY_PLAIN),
    list(error_code = NULL, message = "Org visibility requires an organization license")
  )
  expect_identical(
    parse(422L, VISIBILITY_422),
    list(error_code = NULL, message = "body.visibility: Input should be 'private', 'org' or 'public'")
  )
  expect_null(parse(502L, '{"detail": []}'))
  expect_null(CyteTypeR:::.parse_server_error(httr2::response(
    status_code = 502L,
    headers = list("Content-Type" = "text/html"),
    body = charToRaw("<html>Bad gateway</html>")
  )))
})

test_that("job submission errors show the server's explanation", {
  cases <- list(
    list(status = 400L, body = ORG_VISIBILITY_WRAPPED,
         expected = "Job submission failed: [HTTP_ERROR] Org visibility requires an organization license"),
    list(status = 400L, body = ORG_VISIBILITY_PLAIN,
         expected = "Job submission failed: Org visibility requires an organization license"),
    list(status = 422L, body = VISIBILITY_422,
         expected = "Job submission failed: body.visibility: Input should be 'private', 'org' or 'public'")
  )
  for (case in cases) {
    httr2::local_mocked_responses(function(req) body_response(case$status, case$body))

    error <- tryCatch(
      CyteTypeR:::.submit_job(list(input_data = list()), "https://api.example.com", "tok"),
      error = identity
    )

    expect_identical(conditionMessage(error), case$expected)
  }
})

test_that("upload errors show a plain text explanation", {
  f <- tempfile(fileext = ".duckdb")
  on.exit(unlink(f), add = TRUE)
  writeBin(raw(16), f)
  httr2::local_mocked_responses(function(req) {
    body_response(403L, '{"detail": "User account is inactive"}')
  })

  expect_error(
    CyteTypeR:::.upload_file_chunked("https://api.example.com", "tok", "obs_duckdb", f, 60L,
                                     max_workers = 1L, show_progress = FALSE),
    "^Upload initiate failed: User account is inactive$"
  )
})

test_that("status errors with a plain text or 422 body keep the explanation", {
  httr2::local_mocked_responses(function(req) body_response(400L, ORG_VISIBILITY_PLAIN))
  error <- tryCatch(
    CyteTypeR:::.api_response_helper("job-1", "https://api.example.com", "status"),
    error = identity
  )
  expect_s3_class(error, "cytetype_api_error")
  expect_identical(
    conditionMessage(error),
    "Error while checking job status: HTTP 400 Org visibility requires an organization license"
  )
  expect_null(error$error_code)

  httr2::local_mocked_responses(function(req) body_response(422L, VISIBILITY_422))
  error <- tryCatch(
    CyteTypeR:::.api_response_helper("job-1", "https://api.example.com", "results"),
    error = identity
  )
  expect_identical(
    conditionMessage(error),
    "Error while fetching job results: HTTP 422 body.visibility: Input should be 'private', 'org' or 'public'"
  )
})

test_that("cytetype_api_error carries an optional error code", {
  error <- cytetype_api_error("boom", call = "api", error_code = "REPORT_DELETED")
  expect_identical(error$error_code, "REPORT_DELETED")
  expect_null(cytetype_api_error("boom")$error_code)
  expect_null(conditionCall(error))
  expect_identical(error$call, "api")
})
