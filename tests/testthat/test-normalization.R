# The results normalization rules shared with the CyteType server and the
# Python SDK. These tests mirror tests/test_results_normalization.py.

test_that("every stored result generation is normalized", {
  response <- results_response()
  before <- response

  result <- CyteTypeR:::.normalize_results(response)
  annotations <- by_cluster(result)

  # Clusters without an annotation object are skipped.
  expect_identical(names(annotations), c("1", "2", "3"))
  expect_identical(legacy_values(annotations[["1"]]), LEGACY_EVIDENCE)
  expect_identical(legacy_values(annotations[["2"]]), EXPECTED_FILL)
  expect_identical(legacy_values(annotations[["3"]]), EXPECTED_FILL)
  expect_identical(
    sdk_values(annotations[["1"]]),
    list(reviewerConfidence = "High", isHeterogeneous = NULL, heterogeneityType = NULL)
  )
  expect_identical(
    sdk_values(annotations[["2"]]),
    list(reviewerConfidence = "Moderate", isHeterogeneous = FALSE, heterogeneityType = NULL)
  )
  expect_identical(
    sdk_values(annotations[["3"]]),
    list(reviewerConfidence = "Low", isHeterogeneous = TRUE, heterogeneityType = "distinct_populations")
  )

  # Existing keys keep their names, order and values; the new keys come last.
  expect_identical(names(annotations[["3"]]), c(EXISTING_KEYS, SDK_KEYS))
  expect_identical(
    annotations[["3"]][EXISTING_KEYS[1:6]],
    list(
      clusterId = "3",
      annotation = "CD8+ T Cell",
      ontologyTerm = "CD8-positive, alpha-beta T cell",
      ontologyTermID = "CL:0000625",
      granularAnnotation = "GZMK+ CD8+ T cell",
      cellState = "Cytotoxic"
    )
  )

  # The raw API data is kept unchanged and the other keys are kept.
  expect_identical(result$raw_annotations, before$annotations)
  expect_identical(response, before)
  expect_identical(result$summary, before$summary)
  expect_identical(result$clusterCategories, before$clusterCategories)
  expect_identical(result$studyContext, before$studyContext)
  expect_identical(result$semanticOrder, before$semanticOrder)
  expect_identical(result$displayLevel, before$displayLevel)
})

test_that("results parsed from JSON are normalized the same way", {
  result <- CyteTypeR:::.normalize_results(as_api_json(results_response()))
  annotations <- by_cluster(result)

  expect_identical(names(annotations), c("1", "2", "3"))
  expect_identical(legacy_values(annotations[["2"]]), EXPECTED_FILL)
  expect_identical(legacy_values(annotations[["1"]]), LEGACY_EVIDENCE)
  expect_identical(annotations[["3"]]$heterogeneityType, "distinct_populations")
})

test_that("non-blank fields are never overwritten", {
  curated <- list(
    justification = "Curated justification",
    supportingMarkers = list("CD3E"),
    conflictingMarkers = list("CD19"),
    missingExpression = "Curated missing note",
    unexpectedExpression = "Curated unexpected note"
  )

  entry <- normalize_latest(do.call(new_result, c(list("1", v2_cell_type()), curated)))

  expect_identical(legacy_values(entry), curated)
})

test_that("only blank fields are filled", {
  entry <- normalize_latest(new_result(
    "1",
    v2_cell_type(),
    justification = "Curated justification",
    supportingMarkers = NULL,
    conflictingMarkers = list("CD19"),
    missingExpression = "   ",
    unexpectedExpression = "\n"
  ))

  expect_identical(
    legacy_values(entry),
    list(
      justification = "Curated justification",
      supportingMarkers = list("CD8A", "CD8B", "GZMK"),
      conflictingMarkers = list("CD19"),
      missingExpression = "`CCR7` and `SELL` mark naive cells.",
      unexpectedExpression = "`MKI67` suggests a proliferating subset."
    )
  )
})

test_that("missing legacy fields are filled when a source exists", {
  latest <- new_result("1", gen2_cell_type())
  for (field in LEGACY_FIELDS) {
    latest$annotation[[field]] <- NULL
  }

  expect_identical(legacy_values(normalize_latest(latest)), EXPECTED_FILL)
})

test_that("text is copied as stored", {
  reasoning <- "  `CCR7` is *absent*.\n"

  entry <- normalize_latest(new_result("1", v2_cell_type(missingGenesReasoning = reasoning)))

  expect_identical(entry$justification, CONCLUSION)
  expect_identical(entry$missingExpression, reasoning)
})

test_that("blank reasoning falls back to the joined gene list", {
  for (reasoning in list("", "  \n ", NULL, 7, list("CCR7"))) {
    entry <- normalize_latest(new_result(
      "1",
      gen2_cell_type(missingGenesReasoning = reasoning, unexpectedGenesReasoning = reasoning)
    ))

    expect_identical(entry$missingExpression, "CCR7, SELL")
    expect_identical(entry$unexpectedExpression, "MKI67")
  }
})

test_that("gene lists keep only non-blank strings, as stored", {
  noisy <- list(" CD8A", NULL, 5, "", "   ", list(gene = "CD8B"), list("GZMK"), TRUE, "GZMK")

  entry <- normalize_latest(new_result(
    "1",
    v2_cell_type(
      keySupportingGenes = noisy,
      unexpectedGenes = noisy,
      unexpectedGenesReasoning = "",
      missingGenes = noisy,
      missingGenesReasoning = ""
    )
  ))

  expect_identical(entry$supportingMarkers, list(" CD8A", "GZMK"))
  expect_identical(entry$conflictingMarkers, list(" CD8A", "GZMK"))
  expect_identical(entry$missingExpression, " CD8A, GZMK")
  expect_identical(entry$unexpectedExpression, " CD8A, GZMK")
})

test_that("empty sources fill typed blank values and never NULL", {
  entry <- normalize_latest(new_result(
    "1",
    v2_cell_type(
      conclusion = "",
      keySupportingGenes = list(NULL, ""),
      unexpectedGenes = list(),
      unexpectedGenesReasoning = "",
      missingGenes = list(),
      missingGenesReasoning = ""
    ),
    justification = NULL,
    supportingMarkers = NULL,
    conflictingMarkers = NULL,
    missingExpression = NULL,
    unexpectedExpression = NULL
  ))

  expect_identical(legacy_values(entry), EMPTY_LEGACY_FIELDS)
})

test_that("missing or wrongly typed sources leave the fields unchanged", {
  cell_type <- list(
    conclusion = list("not", "a", "string"),
    keySupportingGenes = "CD8A, CD8B",
    unexpectedGenes = list(MKI67 = 1),
    missingGenesReasoning = "   ",
    unexpectedGenesReasoning = NULL
  )

  entry <- normalize_latest(new_result("1", cell_type, justification = NULL))

  expected <- EMPTY_LEGACY_FIELDS
  expected["justification"] <- list(NULL)
  expect_identical(legacy_values(entry), expected)
})

test_that("results without a structured cellType keep their fields", {
  full_outputs <- list(
    NULL,
    "not an object",
    list(),
    json_object(),
    list(cellType = "T cell"),
    list(cellType = list("T cell"))
  )
  for (full_output in full_outputs) {
    latest <- legacy_result("1", justification = "")
    latest$annotation["fullOutput"] <- list(full_output)

    entry <- normalize_latest(latest)

    expected <- LEGACY_EVIDENCE
    expected$justification <- ""
    expect_identical(legacy_values(entry), expected)
  }
})

test_that("reviewer fields are type-checked", {
  cases <- list(
    list(review = list(confidence = "high", isHeterogeneous = "true"), expected = list(NULL, NULL, NULL)),
    list(review = list(confidence = list("High"), isHeterogeneous = 1), expected = list(NULL, NULL, NULL)),
    list(review = list(confidence = 0.9, isHeterogeneous = NULL), expected = list(NULL, NULL, NULL)),
    list(review = review_fixture("Moderate", TRUE, "  "), expected = list("Moderate", TRUE, NULL)),
    list(review = review_fixture("Moderate", TRUE, 5), expected = list("Moderate", TRUE, NULL)),
    list(review = review_fixture("High", FALSE, "mixed_lineage"), expected = list("High", FALSE, "mixed_lineage")),
    list(review = list(confidence = "Low", fullOutput = "n/a"), expected = list("Low", NULL, NULL)),
    list(review = "not a review", expected = list(NULL, NULL, NULL))
  )
  for (case in cases) {
    latest <- new_result("1", v2_cell_type())
    latest["review"] <- list(case$review)

    expected <- stats::setNames(case$expected, SDK_KEYS)
    expect_identical(sdk_values(normalize_latest(latest)), expected)
  }
})

test_that("a missing review gives NULL values", {
  latest <- new_result("1", v2_cell_type())
  latest$review <- NULL

  entry <- normalize_latest(latest)

  expect_true(all(SDK_KEYS %in% names(entry)))
  expect_identical(sdk_values(entry), stats::setNames(list(NULL, NULL, NULL), SDK_KEYS))
})

test_that("clusters without an annotation are skipped", {
  cluster_data <- list(
    json_object(),
    list(latest = json_object()),
    list(latest = list(developmentalSimilarity = list())),
    list(latest = list(annotation = "T cell", confidence = 0.95)),
    list(latest = list(annotation = NULL)),
    list(latest = NULL),
    "not a cluster"
  )
  for (data in cluster_data) {
    expect_null(CyteTypeR:::.normalize_cluster_annotation("1", data))
  }
})

test_that("normalization is idempotent and does not modify its inputs", {
  latests <- list(
    legacy_result("1"),
    new_result("1", gen2_cell_type()),
    new_result("1", v2_cell_type(), review_fixture("Low", TRUE, "doublet_artifact")),
    new_result("1", v2_cell_type(missingGenesReasoning = "", conclusion = ""))
  )
  for (latest in latests) {
    cluster_data <- list(latest = latest, history = list())
    before <- cluster_data

    entry <- CyteTypeR:::.normalize_cluster_annotation("1", cluster_data)
    again <- CyteTypeR:::.fill_cluster_annotation(entry, cluster_data)

    expect_identical(again, entry)
    expect_identical(cluster_data, before)
  }

  result <- CyteTypeR:::.normalize_results(results_response())
  expect_identical(CyteTypeR:::.normalize_results(result), result)
})

test_that("results stored by older versions are filled from raw_annotations", {
  stored <- old_sdk_result(results_response())
  before <- stored

  result <- CyteTypeR:::.normalize_results(stored)
  annotations <- by_cluster(result)

  expect_identical(stored, before)
  expect_identical(legacy_values(annotations[["1"]]), LEGACY_EVIDENCE)
  expect_identical(legacy_values(annotations[["2"]]), EXPECTED_FILL)
  expect_identical(legacy_values(annotations[["3"]]), EXPECTED_FILL)
  expect_identical(
    sdk_values(annotations[["3"]]),
    list(reviewerConfidence = "Low", isHeterogeneous = TRUE, heterogeneityType = "distinct_populations")
  )
  expect_identical(result$raw_annotations, before$raw_annotations)
  expect_identical(CyteTypeR:::.normalize_results(result), result)
})

test_that("stored values are kept and unmatched entries are handled", {
  stored <- old_sdk_result(results_response())
  stored$annotations[[2]]$justification <- "Kept as stored"
  stored$annotations[[2]]$reviewerConfidence <- "High"
  stored$annotations[[4]] <- list(clusterId = "99", annotation = "Orphan")
  stored$annotations[[5]] <- "not an entry"

  result <- CyteTypeR:::.normalize_results(stored)

  expect_identical(result$annotations[[2]]$justification, "Kept as stored")
  expect_identical(result$annotations[[2]]$reviewerConfidence, "High")
  expect_identical(result$annotations[[2]]$supportingMarkers, list("CD8A", "CD8B", "GZMK"))
  expect_identical(
    result$annotations[[4]],
    c(list(clusterId = "99", annotation = "Orphan"), stats::setNames(list(NULL, NULL, NULL), SDK_KEYS))
  )
  expect_identical(result$annotations[[5]], "not an entry")
})

test_that("results without raw_annotations gain empty review fields", {
  stored <- list(annotations = list(
    list(clusterId = "1", annotation = "T cell"),
    list(clusterId = "2", annotation = "B cell", reviewerConfidence = "High"),
    "not an entry"
  ))
  before <- stored

  result <- CyteTypeR:::.normalize_results(stored)

  expect_identical(
    result$annotations,
    list(
      c(list(clusterId = "1", annotation = "T cell"), stats::setNames(list(NULL, NULL, NULL), SDK_KEYS)),
      list(
        clusterId = "2",
        annotation = "B cell",
        reviewerConfidence = "High",
        isHeterogeneous = NULL,
        heterogeneityType = NULL
      ),
      "not an entry"
    )
  )
  expect_identical(stored, before)
  expect_null(result$raw_annotations)
})

test_that("entries with duplicated cluster IDs pair with their own raw result", {
  response <- list(
    studyContext = "",
    annotations = list(
      a = list(
        latest = new_result("X", v2_cell_type(keySupportingGenes = list("CD8A")), review_fixture("High", FALSE)),
        history = list()
      ),
      `no-result` = list(latest = json_object(), history = list()),
      b = list(
        latest = new_result("X", v2_cell_type(keySupportingGenes = list("MS4A1")), review_fixture("Low", TRUE, "mixed_lineage")),
        history = list()
      )
    ),
    summary = json_object(),
    clusterCategories = list()
  )
  stored <- old_sdk_result(response)
  expect_identical(vapply(stored$annotations, function(entry) entry$clusterId, ""), c("X", "X"))

  result <- CyteTypeR:::.normalize_results(stored)

  expect_identical(
    lapply(result$annotations, function(entry) entry$supportingMarkers),
    list(list("CD8A"), list("MS4A1"))
  )
  expect_identical(
    vapply(result$annotations, function(entry) entry$reviewerConfidence, ""),
    c("High", "Low")
  )
  # The same entries as normalizing the response directly.
  expect_identical(result$annotations, CyteTypeR:::.normalize_results(response)$annotations)
})

test_that("entries fall back to the raw cluster key", {
  stored <- list(
    annotations = list(list(clusterId = "x", annotation = "T cell")),
    raw_annotations = list(
      x = list(latest = list(annotation = "T cell", review = list(confidence = "Low")))
    )
  )

  result <- CyteTypeR:::.normalize_results(stored)

  expect_identical(result$annotations[[1]]$reviewerConfidence, "Low")
})

test_that("raw and flat elements are detected per element", {
  # Cluster IDs 1..N are what CyteTypeR submits; a raw result without a
  # clusterId falls back to its key, and a flat entry is kept as an entry.
  annotation <- new_result("ignored", v2_cell_type())$annotation
  annotation$clusterId <- NULL
  response <- list(annotations = list(
    `1` = list(latest = list(annotation = annotation), history = list()),
    `2` = list(clusterId = "2", annotation = "B cell", ontologyTerm = "B cell"),
    `3` = list(latest = json_object(), history = list())
  ))

  result <- CyteTypeR:::.normalize_results(response)

  expect_length(result$annotations, 2L)
  expect_identical(result$annotations[[1]]$clusterId, "1")
  expect_identical(result$annotations[[1]]$supportingMarkers, list("CD8A", "CD8B", "GZMK"))
  expect_identical(result$annotations[[2]]$clusterId, "2")
  expect_identical(result$annotations[[2]]$ontologyTerm, "B cell")
})

test_that("blank detection matches the shared rules", {
  is_blank <- CyteTypeR:::.is_blank
  expect_true(is_blank(NULL))
  expect_true(is_blank(""))
  expect_true(is_blank(" \t\n"))
  expect_true(is_blank("\u00a0"))
  expect_true(is_blank(list()))
  expect_true(is_blank(NA))
  expect_false(is_blank("x"))
  expect_false(is_blank(list("")))
  expect_false(is_blank(json_object()))
  expect_false(is_blank(0))
  expect_false(is_blank(FALSE))
})

test_that("a missing term name falls back to the term ID, as in 0.5.x", {
  latest <- new_result("1", v2_cell_type())
  latest$annotation$cellOntologyTermName <- NULL
  latest$annotation["clusterId"] <- list(NULL)

  entry <- CyteTypeR:::.normalize_cluster_annotation("7", list(latest = latest, history = list()))

  expect_identical(entry$ontologyTerm, "CL:0000625")
  expect_identical(entry$ontologyTermID, "CL:0000625")
  expect_identical(entry$clusterId, "7")
})
