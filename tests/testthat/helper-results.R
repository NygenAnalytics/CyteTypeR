# Result fixtures for the normalization tests. They mirror the shapes in the
# Python SDK's tests/test_results_normalization.py, in the form httr2 parses
# JSON (simplifyVector = FALSE): objects are named lists, arrays unnamed lists.

LEGACY_FIELDS <- c(
  "justification",
  "supportingMarkers",
  "conflictingMarkers",
  "missingExpression",
  "unexpectedExpression"
)
EXISTING_KEYS <- c(
  "clusterId",
  "annotation",
  "ontologyTerm",
  "ontologyTermID",
  "granularAnnotation",
  "cellState",
  LEGACY_FIELDS
)
SDK_KEYS <- c("reviewerConfidence", "isHeterogeneous", "heterogeneityType")
EXISTING_TABLE_COLUMNS <- c(
  "clusterId",
  "annotation",
  "ontologyTerm",
  "ontologyTermID",
  "granularAnnotation",
  "cellState",
  "justification",
  "supportingMarkers",
  "conflictingMarkers",
  "missingExpression",
  "unexpectedExpression"
)

json_object <- function() stats::setNames(list(), character(0))

EMPTY_LEGACY_FIELDS <- list(
  justification = "",
  supportingMarkers = list(),
  conflictingMarkers = list(),
  missingExpression = "",
  unexpectedExpression = ""
)
CONCLUSION <- paste0(
  "**CD8+ T cell**: `CD8A` and `CD8B` with cytotoxic `GZMK` support a\n",
  "cytotoxic T cell identity. "
)
EXPECTED_FILL <- list(
  justification = CONCLUSION,
  supportingMarkers = list("CD8A", "CD8B", "GZMK"),
  conflictingMarkers = list("MKI67"),
  missingExpression = "`CCR7` and `SELL` mark naive cells.",
  unexpectedExpression = "`MKI67` suggests a proliferating subset."
)
LEGACY_EVIDENCE <- list(
  justification = "Strong GFAP and S100B expression.",
  supportingMarkers = list("GFAP", "S100B", "AQP4"),
  conflictingMarkers = list(),
  missingExpression = "SOX9 lower than expected",
  unexpectedExpression = ""
)

# Set or add fields, keeping NULL values (a JSON null) as entries.
with_fields <- function(x, fields) {
  for (name in names(fields)) {
    x[name] <- list(fields[[name]])
  }
  x
}

# fullOutput.cellType as stored between January and August 2026.
gen2_cell_type <- function(...) {
  with_fields(
    list(
      label = "Cd8+ T Cell",
      keySupportingGenes = list("CD8A", "CD8B", "GZMK"),
      keySupportingGenesReasoning = "`CD8A` and `CD8B` are lineage markers.",
      missingGenes = list("CCR7", "SELL"),
      missingGenesReasoning = "`CCR7` and `SELL` mark naive cells.",
      unexpectedGenes = list("MKI67"),
      unexpectedGenesReasoning = "`MKI67` suggests a proliferating subset.",
      pathwaysSupportingEvidence = list("T cell receptor signaling"),
      pathwaysSupportingEvidenceReasoning = "TCR signaling is enriched.",
      candidateComparison = list("NK cell: lacks `NCAM1`."),
      tissueContextFit = "Expected in blood.",
      clusterContextFit = "Present in all samples.",
      conclusion = CONCLUSION,
      citationExplanations = json_object()
    ),
    list(...)
  )
}

# fullOutput.cellType as stored since August 2026.
v2_cell_type <- function(...) {
  cell_type <- gen2_cell_type(
    catalogReferenceLabel = "CD8-positive, alpha-beta T cell",
    classification = "catalogue",
    whyNotDeeper = "",
    annotationLevels = list(
      L1 = list(label = "Immune cell"),
      L2 = list(label = "T cell"),
      L3 = list(label = "CD8+ T cell")
    ),
    candidateComparison = list(`NK cell` = list(verdict = "rejected"))
  )
  cell_type$citationExplanations <- NULL
  with_fields(cell_type, list(...))
}

review_fixture <- function(confidence, is_heterogeneous, heterogeneity_type = NULL) {
  list(
    reviewComments = "",
    confidence = confidence,
    isHeterogeneous = is_heterogeneous,
    feedback = "",
    labelComparisonScore = NULL,
    fullOutput = list(
      confidenceAssessment = list(label = confidence, reasoning = ""),
      heterogeneityAssessment = list(
        label = if (isTRUE(is_heterogeneous)) "Heterogeneous" else "Homogeneous",
        reasoning = "",
        heterogeneityType = heterogeneity_type,
        heterogeneityTypeReasoning = ""
      )
    )
  )
}

# A latest entry written since January 2026: legacy fields stored empty.
new_result <- function(cluster_id, cell_type, review = NULL, label = "CD8+ T Cell", ...) {
  annotation <- with_fields(
    c(
      list(
        clusterId = cluster_id,
        annotation = label,
        granularAnnotation = "GZMK+ CD8+ T cell",
        cellState = "Cytotoxic",
        cellOntologyTerm = "CL:0000625",
        cellOntologyTermName = "CD8-positive, alpha-beta T cell"
      ),
      EMPTY_LEGACY_FIELDS
    ),
    list(...)
  )
  annotation$fullOutput <- list(
    clusterId = cluster_id,
    cellType = cell_type,
    cellStateWithReasoning = list(label = "Cytotoxic", reasoning = "")
  )
  annotation$clusterContext <- json_object()
  list(
    context = "Cluster context",
    annotation = annotation,
    review = if (is.null(review)) review_fixture("High", FALSE) else review,
    developmentalSimilarity = list(),
    functionalSimilarity = list()
  )
}

# A latest entry stored before January 2026: evidence in the legacy fields.
legacy_result <- function(cluster_id, ...) {
  annotation <- c(
    list(
      clusterId = cluster_id,
      annotation = "Astrocyte",
      granularAnnotation = "GFAP+ astrocyte",
      cellState = "mature",
      cellOntologyTerm = "CL:0000127",
      cellOntologyTermName = "astrocyte"
    ),
    LEGACY_EVIDENCE,
    list(fullOutput = NULL, clusterContext = NULL)
  )
  list(
    context = "Cluster context",
    annotation = with_fields(annotation, list(...)),
    review = list(confidence = "High"),
    developmentalSimilarity = list(),
    functionalSimilarity = list()
  )
}

# A GET /results response mixing every stored result generation, plus a
# cluster without a result and an older test-fixture shape.
results_response <- function() {
  list(
    studyContext = "Human PBMC",
    annotations = list(
      `1` = list(latest = legacy_result("1"), history = list()),
      `2` = list(
        latest = new_result("2", gen2_cell_type(), review_fixture("Moderate", FALSE)),
        history = list()
      ),
      `3` = list(
        latest = new_result("3", v2_cell_type(), review_fixture("Low", TRUE, "distinct_populations")),
        history = list(legacy_result("3"))
      ),
      `4` = list(
        latest = list(developmentalSimilarity = list(), functionalSimilarity = list()),
        history = list()
      ),
      `5` = list(latest = list(annotation = "B cell"), history = list())
    ),
    summary = list(tissue = "blood"),
    semanticOrder = list("1", "2", "3"),
    clusterCategories = list(list(categoryName = "Lymphoid", clusterIds = list("2", "3"))),
    displayLevel = ""
  )
}

# What CyteTypeR before 0.6.0 and the Python SDK before the normalization
# stored for `response`: six fields per cluster, the evidence left empty.
old_sdk_result <- function(response) {
  annotations <- list()
  for (cluster_id in names(response$annotations)) {
    annotation <- response$annotations[[cluster_id]]$latest$annotation
    if (!is.list(annotation) || is.null(names(annotation))) {
      next
    }
    get <- function(key, default) if (key %in% names(annotation)) annotation[[key]] else default
    entry <- list(
      clusterId = get("clusterId", cluster_id),
      annotation = get("annotation", "Unknown"),
      ontologyTerm = get("cellOntologyTermName", "Unknown"),
      ontologyTermID = get("cellOntologyTerm", "Unknown"),
      granularAnnotation = get("granularAnnotation", ""),
      cellState = get("cellState", "")
    )
    for (field in names(EMPTY_LEGACY_FIELDS)) {
      entry[field] <- list(get(field, EMPTY_LEGACY_FIELDS[[field]]))
    }
    annotations[[length(annotations) + 1L]] <- entry
  }
  list(
    annotations = annotations,
    summary = response$summary,
    clusterCategories = response$clusterCategories,
    studyContext = response$studyContext,
    raw_annotations = response$annotations
  )
}

# Round-trip through JSON as the API client does.
as_api_json <- function(x) {
  jsonlite::fromJSON(
    jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", digits = NA),
    simplifyVector = FALSE
  )
}

legacy_values <- function(entry) entry[LEGACY_FIELDS]

sdk_values <- function(entry) {
  stats::setNames(lapply(SDK_KEYS, function(key) entry[[key]]), SDK_KEYS)
}

by_cluster <- function(result) {
  entries <- result$annotations
  stats::setNames(entries, vapply(entries, function(entry) entry$clusterId, character(1)))
}

normalize_latest <- function(latest) {
  entry <- CyteTypeR:::.normalize_cluster_annotation("1", list(latest = latest, history = list()))
  testthat::expect_false(is.null(entry))
  entry
}

# A small, real Seurat object: 60 genes x 90 cells in three clusters, with
# integer counts, log-normalized data, a batch column and a 2D "umap".
make_seurat_fixture <- function(n_genes = 60L, n_cells = 90L, seed = 7L) {
  counts <- withr::with_seed(seed, {
    Matrix::rsparsematrix(
      n_genes, n_cells, density = 0.4,
      rand.x = function(n) as.numeric(stats::rpois(n, 4) + 1)
    )
  })
  dimnames(counts) <- list(
    sprintf("GENE%02d", seq_len(n_genes)),
    sprintf("cell%03d", seq_len(n_cells))
  )
  obj <- suppressWarnings(SeuratObject::CreateSeuratObject(counts = counts))
  obj <- Seurat::NormalizeData(obj, verbose = FALSE)
  clusters <- rep(c("0", "1", "2"), length.out = n_cells)
  obj$seurat_clusters <- factor(clusters, levels = c("0", "1", "2"))
  obj$batch <- rep(c("a", "b"), each = n_cells / 2)
  obj$donor <- rep(c("d1", "d2", "d3"), times = n_cells / 3)
  embedding <- withr::with_seed(seed, matrix(stats::rnorm(n_cells * 2), ncol = 2))
  dimnames(embedding) <- list(colnames(obj), c("umap_1", "umap_2"))
  obj[["umap"]] <- SeuratObject::CreateDimReducObject(
    embeddings = embedding, key = "umap_", assay = "RNA"
  )
  obj
}

make_marker_table <- function(n_genes = 60L) {
  genes <- sprintf("GENE%02d", seq_len(n_genes))
  data.frame(
    p_val = 0.001,
    avg_log2FC = rep(seq(3, 0.1, length.out = 20), 3),
    pct.1 = 0.9,
    pct.2 = 0.1,
    p_val_adj = 0.01,
    cluster = factor(rep(c("0", "1", "2"), each = 20), levels = c("0", "1", "2")),
    gene = genes[c(1:20, 21:40, 41:60)],
    stringsAsFactors = FALSE
  )
}

# Run code in a temporary working directory.
local_temp_workdir <- function(env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  withr::local_dir(dir, .local_envir = env)
  dir
}
