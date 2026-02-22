# Helper: minimal Seurat and prepped_data for build/upload path
.local_seurat_and_prepped <- function() {
  obj <- Seurat::CreateSeuratObject(Matrix::Matrix(1, 2, 2))
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
    group_key = "cluster"
  )
  list(obj = obj, prepped_data = prepped_data)
}

test_that("build failure with require_artifacts TRUE stops with build error", {
  testthat::local_mocked_bindings(
    .save_vars_h5 = function(...) stop("build error")
  )
  x <- .local_seurat_and_prepped()
  expect_error(
    CyteTypeR::CyteTypeR(
      x$obj, x$prepped_data,
      api_url = "https://example.com",
      save_query = FALSE,
      require_artifacts = TRUE
    ),
    "build error"
  )
})

test_that("build failure with require_artifacts FALSE continues and completes", {
  testthat::local_mocked_bindings(
    .save_vars_h5 = function(...) stop("build error"),
    .submit_job = function(...) "job1",
    .poll_for_results = function(...) NULL
  )
  x <- .local_seurat_and_prepped()
  out <- CyteTypeR::CyteTypeR(
    x$obj, x$prepped_data,
    api_url = "https://example.com",
    save_query = FALSE,
    require_artifacts = FALSE
  )
  expect_null(out)
})

test_that("upload failure with require_artifacts TRUE stops with upload error", {
  testthat::local_mocked_bindings(
    .save_vars_h5 = function(...) invisible(NULL),
    .save_obs_duckdb = function(...) invisible(NULL),
    .upload_obs_duckdb = function(...) stop("upload error")
  )
  x <- .local_seurat_and_prepped()
  expect_error(
    CyteTypeR::CyteTypeR(
      x$obj, x$prepped_data,
      api_url = "https://example.com",
      save_query = FALSE,
      require_artifacts = TRUE
    ),
    "upload error"
  )
})

test_that("upload failure with require_artifacts FALSE continues and completes", {
  testthat::local_mocked_bindings(
    .save_vars_h5 = function(...) invisible(NULL),
    .save_obs_duckdb = function(...) invisible(NULL),
    .upload_obs_duckdb = function(...) stop("upload error"),
    .submit_job = function(...) "job1",
    .poll_for_results = function(...) NULL
  )
  x <- .local_seurat_and_prepped()
  out <- CyteTypeR::CyteTypeR(
    x$obj, x$prepped_data,
    api_url = "https://example.com",
    save_query = FALSE,
    require_artifacts = FALSE
  )
  expect_null(out)
})
