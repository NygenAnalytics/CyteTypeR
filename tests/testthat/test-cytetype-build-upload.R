# Helper: minimal Seurat and prepped_data for build/upload path
.local_seurat_and_prepped <- function(build_succeeded = FALSE) {
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
    save_query = FALSE,
    require_artifacts = FALSE
  )
  expect_s4_class(out, "Seurat")
})
