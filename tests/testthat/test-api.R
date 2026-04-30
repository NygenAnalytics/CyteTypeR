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

test_that(".put_to_presigned_url raises api error on HTTP 4xx with response body", {
  httr2::local_mocked_responses(function(req) {
    httr2::response(status_code = 403L, body = charToRaw("Forbidden"))
  })
  expect_error(
    CyteTypeR:::.put_to_presigned_url(
      "https://r2.example.com/bucket/chunk/0?sig=xyz",
      charToRaw("chunk-data"),
      60L
    ),
    "Presigned upload of vars_h5 rejected with HTTP 403; response body: Forbidden",
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

test_that(".put_to_presigned_url raises api error on HTTP 400 with response body", {
  httr2::local_mocked_responses(function(req) {
    httr2::response(status_code = 400L, body = charToRaw("Bad Request"))
  })
  expect_error(
    CyteTypeR:::.put_to_presigned_url(
      "https://r2.example.com/bucket/chunk/0?sig=xyz",
      charToRaw("chunk-data"),
      60L
    ),
    "Presigned upload of vars_h5 rejected with HTTP 400; response body: Bad Request",
    class = "cytetype_api_error"
  )
})

test_that(".put_to_presigned_url includes file_kind and chunk position in error", {
  httr2::local_mocked_responses(function(req) {
    httr2::response(status_code = 400L, body = charToRaw("EntityTooSmall"))
  })
  expect_error(
    CyteTypeR:::.put_to_presigned_url(
      presigned_url = "https://r2.example.com/bucket/chunk/9?sig=xyz",
      chunk_data = charToRaw("chunk-data"),
      timeout_seconds = 60L,
      file_kind = "vars_h5",
      chunk_idx = 9L,
      n_chunks = 10L
    ),
    "Presigned upload of vars_h5 chunk 10/10 rejected with HTTP 400; response body: EntityTooSmall",
    class = "cytetype_api_error"
  )
})

test_that(".put_to_presigned_url truncates large response bodies", {
  long_body <- paste(rep("A", 1200L), collapse = "")
  httr2::local_mocked_responses(function(req) {
    httr2::response(status_code = 400L, body = charToRaw(long_body))
  })
  err <- tryCatch(
    CyteTypeR:::.put_to_presigned_url(
      "https://r2.example.com/bucket/chunk/0?sig=xyz",
      charToRaw("chunk-data"),
      60L
    ),
    cytetype_api_error = function(e) e
  )
  expect_s3_class(err, "cytetype_api_error")
  expect_true(endsWith(err$message, "... [truncated]"))
  truncated_body <- sub("^.*response body: ", "", err$message)
  truncated_body <- sub("\\.\\.\\. \\[truncated\\]$", "", truncated_body)
  expect_identical(truncated_body, paste(rep("A", 500L), collapse = ""))
})
