test_that(".transform_results_seurat returns data frame with expected columns", {
  raw <- list(
    annotations = list(
      list(latest = list(annotation = list(
        clusterId = "1",
        annotation = "T cell",
        cellOntologyTermName = "T cell",
        cellOntologyTerm = "CL:0000084",
        granularAnnotation = "",
        cellState = "naive",
        justification = "Markers",
        supportingMarkers = "CD3D",
        conflictingMarkers = "",
        missingExpression = "",
        unexpectedExpression = ""
      ))),
      list(latest = list(annotation = list(
        clusterId = "2",
        annotation = "B cell",
        cellOntologyTermName = "B cell",
        cellOntologyTerm = "CL:0000236",
        granularAnnotation = "",
        cellState = "",
        justification = "",
        supportingMarkers = "CD79A",
        conflictingMarkers = "",
        missingExpression = "",
        unexpectedExpression = ""
      )))
    ),
    clusterCategories = list()
  )
  out <- CyteTypeR:::.transform_results_seurat(raw, cluster_map = c("1" = "1", "2" = "2"))
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 2L)
  expect_true(all(c("clusterId", "annotation", "ontologyTerm", "ontologyTermID") %in% names(out)))
  expect_identical(out$annotation, c("T cell", "B cell"))
  expect_identical(rownames(out), c("1", "2"))
})

test_that(".transform_results_seurat uses cluster_map when provided", {
  raw <- list(
    annotations = list(
      list(latest = list(annotation = list(
        clusterId = "1", annotation = "T cell", cellOntologyTermName = "T", cellOntologyTerm = "CL:1",
        granularAnnotation = "", cellState = "", justification = "", supportingMarkers = "",
        conflictingMarkers = "", missingExpression = "", unexpectedExpression = ""
      )))
    ),
    clusterCategories = list()
  )
  out <- CyteTypeR:::.transform_results_seurat(raw, cluster_map = c("1" = "c1"))
  expect_identical(out$clusterId, "c1")
  expect_identical(rownames(out), "c1")
})

test_that(".transform_results_seurat coalesces missing annotation fields", {
  raw <- list(
    annotations = list(
      list(latest = list(annotation = list(
        clusterId = "1",
        annotation = NULL,
        cellOntologyTermName = NULL,
        cellOntologyTerm = NULL,
        granularAnnotation = NULL,
        cellState = NULL,
        justification = NULL,
        supportingMarkers = NULL,
        conflictingMarkers = NULL,
        missingExpression = NULL,
        unexpectedExpression = NULL
      )))
    ),
    clusterCategories = list()
  )
  out <- CyteTypeR:::.transform_results_seurat(raw, cluster_map = NULL)
  expect_identical(out$annotation, "Unknown")
  expect_identical(out$ontologyTerm, "Unknown")
  expect_identical(out$ontologyTermID, "Unknown")
})
