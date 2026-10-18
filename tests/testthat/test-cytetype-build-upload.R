# Helper: minimal Seurat and prepped_data for build/upload path
.local_seurat_and_prepped <- function(build_succeeded = FALSE) {
  obj <- Seurat::CreateSeuratObject(
    Matrix::sparseMatrix(
      i = c(1L, 2L),
      j = c(1L, 2L),
      x = c(1, 1),
      dims = c(2L, 2L)
    )
  )
  obj$cluster <- "1"
  prepped_data <- list(
    studyInfo = "",
    infoTags = list(),
    clusterLabels = list("1" = "A"),
    clusterMetadata = list(),
    markerGenes = list(),
    visualizationData = NULL,
    expressionData = list(),
    nParallelClusters = 2L,
    group_key = "cluster",
    build_succeeded = build_succeeded,
    vars_h5_path = "vars.h5",
    obs_duckdb_path = "obs.duckdb"
  )
  list(obj = obj, prepped_data = prepped_data)
}

test_that("build_succeeded FALSE with require_artifacts TRUE stops", {
  x <- .local_seurat_and_prepped(build_succeeded = FALSE)
  expect_error(
    CyteTypeR::CyteTypeR(
      x$obj, x$prepped_data,
      api_url = "https://example.com",
      auth_token = "test-token",
      save_query = FALSE,
      require_artifacts = TRUE
    ),
    "Artifact build did not succeed"
  )
})

test_that("build_succeeded FALSE with require_artifacts FALSE skips uploads and continues", {
  testthat::local_mocked_bindings(
    .submit_job = function(...) "job1",
    .poll_for_results = function(...) NULL
  )
  x <- .local_seurat_and_prepped(build_succeeded = FALSE)
  out <- CyteTypeR::CyteTypeR(
    x$obj, x$prepped_data,
    api_url = "https://example.com",
    auth_token = "test-token",
    save_query = FALSE,
    require_artifacts = FALSE
  )
  expect_s4_class(out, "Seurat")
})

test_that("upload failure with require_artifacts TRUE stops with upload error", {
  testthat::local_mocked_bindings(
    .upload_obs_duckdb = function(...) stop("upload error")
  )
  x <- .local_seurat_and_prepped(build_succeeded = TRUE)
  expect_error(
    CyteTypeR::CyteTypeR(
      x$obj, x$prepped_data,
      api_url = "https://example.com",
      auth_token = "test-token",
      save_query = FALSE,
      require_artifacts = TRUE
    ),
    "upload error"
  )
})

test_that("upload failure with require_artifacts FALSE continues and completes", {
  testthat::local_mocked_bindings(
    .upload_obs_duckdb = function(...) stop("upload error"),
    .submit_job = function(...) "job1",
    .poll_for_results = function(...) NULL
  )
  x <- .local_seurat_and_prepped(build_succeeded = TRUE)
  out <- CyteTypeR::CyteTypeR(
    x$obj, x$prepped_data,
    api_url = "https://example.com",
    auth_token = "test-token",
    save_query = FALSE,
    require_artifacts = FALSE
  )
  expect_s4_class(out, "Seurat")
})

test_that("missing credentials stop before uploads or submission", {
  upload_called <- FALSE
  submit_called <- FALSE
  testthat::local_mocked_bindings(
    .load_credentials = function(api_url = NULL) NULL,
    .upload_obs_duckdb = function(...) {
      upload_called <<- TRUE
      stop("unexpected upload")
    },
    .submit_job = function(...) {
      submit_called <<- TRUE
      stop("unexpected submission")
    },
    .package = "CyteTypeR"
  )
  x <- .local_seurat_and_prepped(build_succeeded = TRUE)

  expect_error(
    CyteTypeR::CyteTypeR(
      x$obj,
      x$prepped_data,
      api_url = "https://example.com",
      save_query = FALSE
    ),
    "SetupCyteTypeR",
    class = "cytetype_auth_error"
  )
  expect_false(upload_called)
  expect_false(submit_called)
})

test_that("stored credentials are used for submission", {
  captured <- list()
  credentials <- list(
    apiUrl = "https://example.com",
    dashboardUrl = "https://example.com/dashboard",
    apiToken = "stored-token",
    tokenId = "token-id",
    userId = "user-id",
    email = "researcher@example.com"
  )
  testthat::local_mocked_bindings(
    .load_credentials = function(api_url = NULL) {
      expect_identical(api_url, "https://example.com")
      credentials
    },
    .submit_job = function(payload, api_url, auth_token = NULL) {
      captured <<- list(api_url = api_url, auth_token = auth_token)
      "job1"
    },
    .poll_for_results = function(...) NULL,
    .package = "CyteTypeR"
  )
  x <- .local_seurat_and_prepped(build_succeeded = FALSE)

  out <- CyteTypeR::CyteTypeR(
    x$obj,
    x$prepped_data,
    api_url = "https://example.com",
    save_query = FALSE,
    require_artifacts = FALSE
  )

  expect_s4_class(out, "Seurat")
  expect_identical(captured$api_url, "https://example.com")
  expect_identical(captured$auth_token, "stored-token")
  expect_false(
    "auth_token" %in%
      names(out@misc$cytetype_jobDetails)
  )
})

test_that("explicit token overrides stored credentials", {
  captured_token <- NULL
  testthat::local_mocked_bindings(
    .load_credentials = function(...) {
      fail("Explicit tokens must not read stored credentials")
    },
    .submit_job = function(payload, api_url, auth_token = NULL) {
      captured_token <<- auth_token
      "job1"
    },
    .poll_for_results = function(...) NULL,
    .package = "CyteTypeR"
  )
  x <- .local_seurat_and_prepped(build_succeeded = FALSE)

  CyteTypeR::CyteTypeR(
    x$obj,
    x$prepped_data,
    api_url = "https://example.com",
    auth_token = "explicit-token",
    save_query = FALSE,
    require_artifacts = FALSE
  )

  expect_identical(captured_token, "explicit-token")
})

test_that("completed workflow stores results that GetResults can return", {
  result <- list(
    annotations = list(list(
      clusterId = "1",
      annotation = "T cell",
      ontologyTerm = "T cell",
      ontologyTermID = "CL:0000084",
      cellState = "",
      granularAnnotation = ""
    ))
  )
  testthat::local_mocked_bindings(
    .submit_job = function(...) "job1",
    .poll_for_results = function(...) result,
    .package = "CyteTypeR"
  )
  x <- .local_seurat_and_prepped(build_succeeded = FALSE)

  out <- CyteTypeR::CyteTypeR(
    x$obj,
    x$prepped_data,
    api_url = "https://example.com",
    auth_token = "explicit-token",
    save_query = FALSE,
    require_artifacts = FALSE
  )

  expect_s3_class(out@misc$cytetype_results, "data.frame")
  expect_identical(
    GetResults(out),
    out@misc$cytetype_results
  )
})

test_that("visibility is validated and sent as a top-level body key", {
  captured <- NULL
  testthat::local_mocked_bindings(
    .submit_job = function(payload, api_url, auth_token = NULL) {
      captured <<- payload
      "job1"
    },
    .poll_for_results = function(...) NULL,
    .package = "CyteTypeR"
  )
  x <- .local_seurat_and_prepped(build_succeeded = FALSE)

  out <- CyteTypeR::CyteTypeR(
    x$obj, x$prepped_data,
    api_url = "https://example.com",
    auth_token = "test-token",
    save_query = FALSE,
    require_artifacts = FALSE,
    visibility = "org"
  )

  expect_identical(captured$visibility, "org")
  expect_false("visibility" %in% names(captured$input_data))
  expect_identical(out@misc$cytetype_jobDetails$visibility, "org")
  body <- jsonlite::fromJSON(
    as.character(jsonlite::toJSON(captured, auto_unbox = TRUE, null = "null")),
    simplifyVector = FALSE
  )
  expect_identical(body$visibility, "org")
})

test_that("visibility is omitted from the request by default", {
  captured <- NULL
  testthat::local_mocked_bindings(
    .submit_job = function(payload, api_url, auth_token = NULL) {
      captured <<- payload
      "job1"
    },
    .poll_for_results = function(...) NULL,
    .package = "CyteTypeR"
  )
  x <- .local_seurat_and_prepped(build_succeeded = FALSE)

  out <- CyteTypeR::CyteTypeR(
    x$obj, x$prepped_data,
    api_url = "https://example.com",
    auth_token = "test-token",
    save_query = FALSE,
    require_artifacts = FALSE
  )

  expect_false("visibility" %in% names(captured))
  expect_false("visibility" %in% names(out@misc$cytetype_jobDetails))
})

test_that("an invalid visibility stops before uploads or submission", {
  testthat::local_mocked_bindings(
    .upload_obs_duckdb = function(...) stop("unexpected upload"),
    .submit_job = function(...) stop("unexpected submission"),
    .package = "CyteTypeR"
  )
  x <- .local_seurat_and_prepped(build_succeeded = TRUE)

  for (visibility in list("shared", c("private", "org"), NA_character_, 1)) {
    expect_error(
      CyteTypeR::CyteTypeR(
        x$obj, x$prepped_data,
        api_url = "https://example.com",
        auth_token = "test-token",
        save_query = FALSE,
        visibility = visibility
      ),
      "visibility must be"
    )
  }
})

test_that("visibility is the last argument of CyteTypeR()", {
  formals_names <- names(formals(CyteTypeR::CyteTypeR))
  expect_identical(formals_names[length(formals_names)], "visibility")
  expect_null(formals(CyteTypeR::CyteTypeR)$visibility)
})

test_that("an interrupted run points to GetResults(job_id = ...)", {
  testthat::local_mocked_bindings(
    .submit_job = function(...) "job-77",
    .poll_for_results = function(...) {
      signalCondition(structure(
        list(message = "", call = NULL),
        class = c("interrupt", "condition")
      ))
      stop("not reached")
    },
    .package = "CyteTypeR"
  )
  x <- .local_seurat_and_prepped(build_succeeded = FALSE)

  # The exiting handler stands in for R's top level, which ends the run.
  expect_message(
    outcome <- tryCatch(
      CyteTypeR::CyteTypeR(
        x$obj, x$prepped_data,
        api_url = "https://example.com",
        auth_token = "test-token",
        save_query = FALSE,
        require_artifacts = FALSE
      ),
      interrupt = function(cnd) "interrupted"
    ),
    'GetResults\\(job_id = "job-77"\\)'
  )
  expect_identical(outcome, "interrupted")
})
