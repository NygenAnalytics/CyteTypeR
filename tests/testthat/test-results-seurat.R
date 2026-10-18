# How results end up in the per-cluster table and the Seurat object.

small_seurat <- function(clusters = c("0", "0", "1", "1", "2", "3", "4", "2")) {
  n <- length(clusters)
  counts <- Matrix::sparseMatrix(
    i = rep(1:2, length.out = n),
    j = seq_len(n),
    x = 1,
    dims = c(2L, n),
    dimnames = list(c("g1", "g2"), paste0("cell", seq_len(n)))
  )
  obj <- suppressWarnings(SeuratObject::CreateSeuratObject(counts = counts))
  obj$leiden <- clusters
  obj
}

# CyteType IDs "1".."5" for the labels "0".."4", as PrepareCyteTypeR() makes them.
LEIDEN_LABELS <- list(`1` = "0", `2` = "1", `3` = "2", `4` = "3", `5` = "4")

test_that("the results table keeps the 0.5.x columns first and appends the new ones", {
  table <- CyteTypeR:::.transform_results_seurat(results_response(), cluster_map = LEIDEN_LABELS)

  expect_s3_class(table, "data.frame")
  expect_identical(names(table)[1:11], EXISTING_TABLE_COLUMNS)
  expect_identical(names(table), c(EXISTING_TABLE_COLUMNS, SDK_KEYS))
  # Clusters 4 and 5 have no result and get no row.
  expect_identical(table$clusterId, c("0", "1", "2"))
  expect_identical(rownames(table), c("0", "1", "2"))
  expect_identical(table$annotation, c("Astrocyte", "CD8+ T Cell", "CD8+ T Cell"))
  expect_identical(table$justification, c(LEGACY_EVIDENCE$justification, CONCLUSION, CONCLUSION))
  expect_identical(table$supportingMarkers, c("GFAP; S100B; AQP4", "CD8A; CD8B; GZMK", "CD8A; CD8B; GZMK"))
  expect_identical(table$conflictingMarkers, c("", "MKI67", "MKI67"))
  expect_identical(table$missingExpression[2], "`CCR7` and `SELL` mark naive cells.")
  expect_identical(table$unexpectedExpression[3], "`MKI67` suggests a proliferating subset.")
  expect_identical(table$reviewerConfidence, c("High", "Moderate", "Low"))
  expect_identical(table$isHeterogeneous, c(NA, FALSE, TRUE))
  expect_identical(table$heterogeneityType, c(NA, NA, "distinct_populations"))
  for (column in EXISTING_TABLE_COLUMNS) {
    expect_type(table[[column]], "character")
  }
})

test_that("failed clusters with an empty latest result do not break the table", {
  # 0.5.1 mapped every failed cluster to the ID "NULL" and stopped on the
  # duplicate row names at the end of CyteTypeR().
  response <- list(annotations = list(
    `1` = list(latest = new_result("1", v2_cell_type()), history = list()),
    `2` = list(latest = json_object(), history = list()),
    `3` = list(latest = json_object(), history = list()),
    `4` = list(latest = json_object(), history = list())
  ))

  table <- CyteTypeR:::.transform_results_seurat(
    as_api_json(response),
    cluster_map = list(`1` = "T", `2` = "B", `3` = "NK", `4` = "Mono")
  )

  expect_identical(table$clusterId, "T")
  expect_false(any(table$clusterId == "NULL"))
})

test_that("duplicate or missing cluster IDs keep default row names", {
  stored <- list(annotations = list(
    list(clusterId = "X", annotation = "T cell"),
    list(clusterId = "X", annotation = "B cell"),
    list(annotation = "NK cell")
  ))

  table <- CyteTypeR:::.transform_results_seurat(stored)

  expect_identical(table$clusterId, c("X", "X", ""))
  expect_identical(rownames(table), c("1", "2", "3"))
})

test_that("an empty result gives an empty table with all columns", {
  table <- CyteTypeR:::.transform_results_seurat(list(annotations = json_object()))

  expect_identical(nrow(table), 0L)
  expect_identical(names(table), c(EXISTING_TABLE_COLUMNS, SDK_KEYS))
})

test_that("annotation columns include the reviewer confidence factor", {
  obj <- small_seurat()
  result <- CyteTypeR:::.normalize_results(results_response())

  obj <- CyteTypeR:::.store_annotations_seurat(obj, result, "job-1", "cytetype", "leiden", LEIDEN_LABELS)

  confidence <- obj$cytetype_reviewerConfidence_leiden
  expect_s3_class(confidence, "factor")
  expect_identical(levels(confidence), c("High", "Moderate", "Low", "Unknown"))
  expect_identical(
    as.character(confidence),
    c("High", "High", "Moderate", "Moderate", "Low", "Unknown", "Unknown", "Low")
  )
  # The four existing columns keep their names and values.
  expect_identical(
    as.character(obj$cytetype_annotation_leiden),
    c("Astrocyte", "Astrocyte", "CD8+ T Cell", "CD8+ T Cell", "CD8+ T Cell", "Unknown", "Unknown", "CD8+ T Cell")
  )
  expect_identical(
    as.character(obj$cytetype_cellOntologyTerm_leiden)[1:3],
    c("astrocyte", "astrocyte", "CD8-positive, alpha-beta T cell")
  )
  expect_identical(
    as.character(obj$cytetype_cellOntologyTermID_leiden)[6:8],
    c("Unknown", "Unknown", "CL:0000625")
  )
  expect_identical(as.character(obj$cytetype_cellState_leiden)[5:6], c("Cytotoxic", ""))
  new_columns <- grep("^cytetype_", names(obj@meta.data), value = TRUE)
  expect_setequal(
    new_columns,
    paste0("cytetype_", c("annotation", "cellOntologyTerm", "cellOntologyTermID", "cellState", "reviewerConfidence"), "_leiden")
  )
})

test_that("the reviewer confidence column keeps only the levels that occur", {
  obj <- small_seurat(c("1", "2", "3"))
  result <- list(annotations = list(
    list(clusterId = "1", annotation = "T cell", reviewerConfidence = "Low"),
    list(clusterId = "2", annotation = "B cell", reviewerConfidence = "bogus"),
    list(clusterId = "3", annotation = "NK cell")
  ))

  obj <- CyteTypeR:::.store_annotations_seurat(
    obj, result, "job-1", "cytetype", "leiden", list(`1` = "1", `2` = "2", `3` = "3")
  )

  confidence <- obj$cytetype_reviewerConfidence_leiden
  expect_identical(levels(confidence), c("Low", "Unknown"))
  expect_identical(as.character(confidence), c("Low", "Unknown", "Unknown"))
})

test_that("the default prefix stores the table with the job and full results attached", {
  obj <- small_seurat()
  result <- CyteTypeR:::.normalize_results(results_response())

  obj <- CyteTypeR:::.store_annotations_seurat(obj, result, "job-1", "cytetype", "leiden", LEIDEN_LABELS)

  stored <- obj@misc$cytetype_results
  expect_s3_class(stored, "data.frame")
  expect_identical(names(stored)[1:11], EXISTING_TABLE_COLUMNS)
  expect_identical(attr(stored, "job_id"), "job-1")
  expect_identical(attr(stored, "result"), result)
  expect_identical(GetResults(obj), stored)
})

test_that("another prefix does not overwrite cytetype_results", {
  obj <- small_seurat()
  first <- CyteTypeR:::.normalize_results(results_response())
  obj <- CyteTypeR:::.store_annotations_seurat(obj, first, "job-1", "cytetype", "leiden", LEIDEN_LABELS)
  default_table <- obj@misc$cytetype_results

  second <- list(annotations = list(list(clusterId = "1", annotation = "Other label")))
  obj <- CyteTypeR:::.store_annotations_seurat(obj, second, "job-2", "run2", "leiden", LEIDEN_LABELS)

  expect_identical(obj@misc$cytetype_results, default_table)
  expect_identical(obj@misc$run2_results$job_id, "job-2")
  expect_identical(obj@misc$run2_results$result, second)
  expect_identical(GetResults(obj), default_table)
  run2 <- GetResults(obj, results_prefix = "run2")
  expect_identical(run2$annotations[[1]]$annotation, "Other label")
  expect_true(all(SDK_KEYS %in% names(run2$annotations[[1]])))
})

test_that("CyteTypeR() completes when some clusters have no result", {
  testthat::local_mocked_bindings(
    .submit_job = function(...) "job1",
    .poll_for_results = function(...) as_api_json(results_response()),
    .package = "CyteTypeR"
  )
  obj <- small_seurat()
  prepped_data <- list(
    clusterLabels = LEIDEN_LABELS,
    clusterMetadata = list(),
    markerGenes = list(),
    visualizationData = NULL,
    expressionData = list(),
    group_key = "leiden",
    build_succeeded = FALSE
  )

  out <- CyteTypeR::CyteTypeR(
    obj,
    prepped_data,
    api_url = "https://example.com",
    auth_token = "test-token",
    save_query = FALSE,
    require_artifacts = FALSE,
    show_progress = FALSE
  )

  expect_s4_class(out, "Seurat")
  table <- out@misc$cytetype_results
  expect_identical(table$clusterId, c("0", "1", "2"))
  expect_identical(table$supportingMarkers[2], "CD8A; CD8B; GZMK")
  expect_identical(attr(table, "job_id"), "job1")
  expect_identical(names(attr(table, "result")$raw_annotations), c("1", "2", "3", "4", "5"))
  expect_identical(
    as.character(out$cytetype_reviewerConfidence_leiden),
    c("High", "High", "Moderate", "Moderate", "Low", "Unknown", "Unknown", "Low")
  )
  # The three legacy columns keep their names; cells without a result are NA.
  expect_identical(
    as.character(out$cytetype_leiden),
    c("Astrocyte", "Astrocyte", "CD8+ T Cell", "CD8+ T Cell", "CD8+ T Cell", NA, NA, "CD8+ T Cell")
  )
  expect_identical(as.character(out$cytetype_ontologyTerm_leiden)[1], "astrocyte")
  expect_identical(as.character(out$cytetype_ontologyID_leiden)[8], "CL:0000625")
  expect_identical(GetResults(out), table)
})

test_that("CyteTypeR() with another prefix leaves cytetype_results alone", {
  testthat::local_mocked_bindings(
    .submit_job = function(...) "job2",
    .poll_for_results = function(...) as_api_json(results_response()),
    .package = "CyteTypeR"
  )
  obj <- small_seurat()
  obj@misc$cytetype_results <- data.frame(clusterId = "0", annotation = "earlier")
  prepped_data <- list(
    clusterLabels = LEIDEN_LABELS,
    clusterMetadata = list(),
    markerGenes = list(),
    visualizationData = NULL,
    expressionData = list(),
    group_key = "leiden",
    build_succeeded = FALSE
  )

  out <- CyteTypeR::CyteTypeR(
    obj,
    prepped_data,
    results_prefix = "run2",
    api_url = "https://example.com",
    auth_token = "test-token",
    save_query = FALSE,
    require_artifacts = FALSE,
    show_progress = FALSE
  )

  expect_identical(out@misc$cytetype_results, data.frame(clusterId = "0", annotation = "earlier"))
  expect_identical(out@misc$run2_results$job_id, "job2")
  expect_true("run2_reviewerConfidence_leiden" %in% names(out@meta.data))
  expect_true("run2_leiden" %in% names(out@meta.data))
})
