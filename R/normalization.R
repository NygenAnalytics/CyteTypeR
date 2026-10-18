# Normalization of per-cluster annotation results.
#
# The CyteType server (read-time fill in GET /results), the Python SDK
# (cytetype/api/normalization.py) and this package apply the same rules, so
# change all three together:
#
# - Jobs since 2026-01-25 store the legacy evidence fields (justification,
#   supportingMarkers, ...) empty and keep the evidence in
#   latest.annotation.fullOutput.cellType. A blank legacy field is filled from
#   there; a non-blank value is never overwritten, so the fill is idempotent.
# - A result is filled only when fullOutput.cellType is a JSON object (gen-2 and
#   V2 results), never because its legacy fields look empty.
# - reviewerConfidence, isHeterogeneous and heterogeneityType are added from
#   latest.review. The server does not add these keys; both SDKs do.
#
# Results are parsed with simplifyVector = FALSE, so JSON objects are named
# lists and JSON arrays are unnamed lists. Stored results may be partial or
# malformed, so every access is type-checked.

.REVIEWER_CONFIDENCE_LEVELS <- c("High", "Moderate", "Low")

.LEGACY_EVIDENCE_FIELDS <- c(
  "justification",
  "supportingMarkers",
  "conflictingMarkers",
  "missingExpression",
  "unexpectedExpression"
)

.SDK_RESULT_KEYS <- c("reviewerConfidence", "isHeterogeneous", "heterogeneityType")

# Columns of obj@misc$cytetype_results. The first eleven are the 0.5.x table;
# new columns are only ever appended.
.RESULTS_TABLE_COLUMNS <- c(
  "clusterId",
  "annotation",
  "ontologyTerm",
  "ontologyTermID",
  "granularAnnotation",
  "cellState",
  .LEGACY_EVIDENCE_FIELDS,
  .SDK_RESULT_KEYS
)

# Python's str.strip() removes Unicode whitespace; [\h\v] is the PCRE match.
.WHITESPACE_PATTERN <- "[\\h\\v]"

.is_json_object <- function(value) {
  is.list(value) && !is.data.frame(value) && !is.null(names(value))
}

.is_json_array <- function(value) {
  is.list(value) && !is.data.frame(value) && is.null(names(value))
}

.as_json_object <- function(value) {
  if (.is_json_object(value)) value else stats::setNames(list(), character(0))
}

# value[[key]] for a JSON object, NULL when the key is absent (dict.get).
.json_get <- function(value, key) {
  if (.is_json_object(value) && key %in% names(value)) value[[key]] else NULL
}

# dict.get(key, default): the default only when the key is absent, so a key
# stored as null stays NULL.
.json_get_or <- function(value, key, default) {
  if (.is_json_object(value) && key %in% names(value)) value[[key]] else default
}

.is_string <- function(value) {
  is.character(value) && length(value) == 1L && !is.na(value)
}

.is_nonblank_string <- function(value) {
  .is_string(value) &&
    nzchar(trimws(value, whitespace = .WHITESPACE_PATTERN))
}

# Blank: NULL (None), an empty or whitespace-only string, or an empty array.
.is_blank <- function(value) {
  if (is.null(value)) {
    return(TRUE)
  }
  if (is.character(value) && length(value) == 1L) {
    return(is.na(value) || !nzchar(trimws(value, whitespace = .WHITESPACE_PATTERN)))
  }
  if (.is_json_array(value) || (is.atomic(value) && length(value) == 0L)) {
    return(length(value) == 0L)
  }
  is.atomic(value) && length(value) == 1L && is.na(value)
}

# The non-blank string items of a JSON array, kept as stored; NULL when the
# value is not an array.
.gene_list <- function(value) {
  if (!.is_json_array(value)) {
    return(NULL)
  }
  Filter(.is_nonblank_string, value)
}

.gene_text <- function(reasoning, genes) {
  if (.is_nonblank_string(reasoning)) {
    return(reasoning)
  }
  gene_list <- .gene_list(genes)
  if (is.null(gene_list)) {
    return(NULL)
  }
  paste(unlist(gene_list, use.names = FALSE), collapse = ", ")
}

# Each legacy field's value from fullOutput.cellType; NULL means no source.
.legacy_field_sources <- function(cell_type) {
  conclusion <- .json_get(cell_type, "conclusion")
  list(
    justification = if (.is_string(conclusion)) conclusion else NULL,
    supportingMarkers = .gene_list(.json_get(cell_type, "keySupportingGenes")),
    conflictingMarkers = .gene_list(.json_get(cell_type, "unexpectedGenes")),
    missingExpression = .gene_text(
      .json_get(cell_type, "missingGenesReasoning"),
      .json_get(cell_type, "missingGenes")
    ),
    unexpectedExpression = .gene_text(
      .json_get(cell_type, "unexpectedGenesReasoning"),
      .json_get(cell_type, "unexpectedGenes")
    )
  )
}

.review_fields <- function(latest) {
  review <- .as_json_object(.json_get(latest, "review"))
  confidence <- .json_get(review, "confidence")
  is_heterogeneous <- .json_get(review, "isHeterogeneous")
  assessment <- .as_json_object(
    .json_get(.as_json_object(.json_get(review, "fullOutput")), "heterogeneityAssessment")
  )
  heterogeneity_type <- .json_get(assessment, "heterogeneityType")
  list(
    reviewerConfidence = if (
      .is_string(confidence) && confidence %in% .REVIEWER_CONFIDENCE_LEVELS
    ) {
      confidence
    } else {
      NULL
    },
    isHeterogeneous = if (
      is.logical(is_heterogeneous) && length(is_heterogeneous) == 1L &&
        !is.na(is_heterogeneous)
    ) {
      is_heterogeneous
    } else {
      NULL
    },
    heterogeneityType = if (.is_nonblank_string(heterogeneity_type)) {
      heterogeneity_type
    } else {
      NULL
    }
  )
}

# Fill-only normalization of one annotations entry (fill_cluster_annotation in
# the Python SDK). `cluster_data` is the cluster's raw result
# (list(latest = ..., history = ...)) or NULL.
.fill_cluster_annotation <- function(entry, cluster_data) {
  normalized <- if (is.list(entry)) entry else list()
  latest <- .as_json_object(.json_get(.as_json_object(cluster_data), "latest"))
  annotation <- .as_json_object(.json_get(latest, "annotation"))
  cell_type <- .json_get(.as_json_object(.json_get(annotation, "fullOutput")), "cellType")
  # A structured cellType marks gen-2 and V2 results; legacy results have none.
  if (.is_json_object(cell_type)) {
    sources <- .legacy_field_sources(cell_type)
    for (field in names(sources)) {
      value <- sources[[field]]
      if (!is.null(value) && .is_blank(.json_get(normalized, field))) {
        normalized[field] <- list(value)
      }
    }
  }
  review <- .review_fields(latest)
  for (key in names(review)) {
    if (is.null(.json_get(normalized, key))) {
      normalized[key] <- list(review[[key]])
    }
  }
  normalized
}

# The normalized annotations entry for one raw cluster result, or NULL when the
# cluster has no annotation (normalize_cluster_annotation in the Python SDK).
.normalize_cluster_annotation <- function(cluster_id, cluster_data) {
  latest <- .as_json_object(.json_get(.as_json_object(cluster_data), "latest"))
  annotation <- .json_get(latest, "annotation")
  if (!.is_json_object(annotation)) {
    return(NULL)
  }
  get <- function(key, default) .json_get_or(annotation, key, default)
  entry <- list(
    # As in CyteTypeR 0.5.x, a missing cluster ID or term name falls back to
    # the cluster's key and the term ID, so the existing columns keep their
    # values. The Python SDK uses the key or "Unknown" only when the key is
    # absent; stored results always have both fields.
    clusterId = .json_get(annotation, "clusterId") %||% cluster_id,
    annotation = get("annotation", "Unknown"),
    ontologyTerm = .json_get(annotation, "cellOntologyTermName") %||%
      .json_get(annotation, "cellOntologyTerm") %||% "Unknown",
    ontologyTermID = get("cellOntologyTerm", "Unknown"),
    granularAnnotation = get("granularAnnotation", ""),
    cellState = get("cellState", ""),
    justification = get("justification", ""),
    supportingMarkers = get("supportingMarkers", list()),
    conflictingMarkers = get("conflictingMarkers", list()),
    missingExpression = get("missingExpression", ""),
    unexpectedExpression = get("unexpectedExpression", "")
  )
  .fill_cluster_annotation(entry, cluster_data)
}

# How an element of `annotations` is normalized: "raw" is a cluster result from
# the API (list(latest = ..., history = ...)), "flat" an entry that was already
# normalized, and "other" anything else, which is kept as is.
.annotation_element_kind <- function(element, in_object) {
  if (.is_json_object(element)) {
    keys <- names(element)
    if (any(c("latest", "history") %in% keys)) {
      return("raw")
    }
    if (any(c("clusterId", "annotation") %in% keys)) {
      return("flat")
    }
    return(if (in_object) "raw" else "flat")
  }
  if (in_object) "raw" else "other"
}

# Raw cluster results with an annotation, queued by the entry's cluster ID in
# raw order, so entries with a duplicated clusterId pair with their own result.
.queue_raw_results <- function(raw_annotations) {
  queue <- list()
  raw_ids <- names(raw_annotations)
  for (index in seq_along(raw_annotations)) {
    cluster_data <- raw_annotations[[index]]
    latest <- .as_json_object(.json_get(.as_json_object(cluster_data), "latest"))
    annotation <- .json_get(latest, "annotation")
    if (!.is_json_object(annotation)) {
      next
    }
    entry_id <- .json_get_or(annotation, "clusterId", raw_ids[[index]])
    if (.is_string(entry_id)) {
      key <- paste0("id:", entry_id)
      queue[[key]] <- c(queue[[key]], list(cluster_data))
    }
  }
  queue
}

#' Normalize CyteType results
#'
#' Turns the `annotations` of a results response into a list of normalized
#' entries. Raw cluster results (the API's
#' `annotations = {cluster_id: {latest, history}}`) become one entry per cluster
#' that has an annotation, and the raw results are kept unchanged as
#' `raw_annotations`. Entries that are already normalized (results stored by
#' earlier versions) are filled from `raw_annotations` when available. Every
#' entry gets the `reviewerConfidence`, `isHeterogeneous` and
#' `heterogeneityType` keys.
#' @noRd
.normalize_results <- function(result) {
  if (!.is_json_object(result)) {
    return(result)
  }
  annotations <- .json_get(result, "annotations")
  if (!is.list(annotations) || is.data.frame(annotations)) {
    return(result)
  }
  in_object <- !is.null(names(annotations))
  kinds <- vapply(
    annotations,
    .annotation_element_kind,
    character(1),
    in_object = in_object,
    USE.NAMES = FALSE
  )
  raw_annotations <- .json_get(result, "raw_annotations")
  if (!.is_json_object(raw_annotations)) {
    raw_annotations <- if (any(kinds == "raw") && in_object) annotations else NULL
  }
  queue <- .queue_raw_results(raw_annotations)
  ids <- names(annotations)

  normalized <- list()
  for (index in seq_along(annotations)) {
    element <- annotations[[index]]
    key <- if (in_object && !is.na(ids[[index]])) ids[[index]] else ""
    entry <- switch(
      kinds[[index]],
      raw = .normalize_cluster_annotation(key, element),
      flat = {
        cluster_id <- .json_get(element, "clusterId")
        cluster_data <- NULL
        if (.is_string(cluster_id)) {
          queue_key <- paste0("id:", cluster_id)
          matches <- queue[[queue_key]]
          if (length(matches) > 0L) {
            cluster_data <- matches[[1L]]
            queue[[queue_key]] <- matches[-1L]
          } else {
            cluster_data <- .json_get(raw_annotations, cluster_id)
          }
        }
        filled <- .fill_cluster_annotation(element, cluster_data)
        if (is.null(.json_get(filled, "clusterId")) && nzchar(key)) {
          filled <- c(list(clusterId = key), filled)
        }
        filled
      },
      element
    )
    if (!is.null(entry)) {
      normalized[[length(normalized) + 1L]] <- entry
    }
  }

  result["annotations"] <- list(normalized)
  if (!is.null(raw_annotations)) {
    result["raw_annotations"] <- list(raw_annotations)
  }
  result
}

# A scalar text value for a table cell or meta.data column.
.scalar_text <- function(value, default) {
  if (is.null(value)) {
    return(default)
  }
  if (is.list(value)) {
    value <- unlist(
      Filter(function(item) is.atomic(item) && length(item) == 1L, value),
      use.names = FALSE
    )
  }
  if (!is.atomic(value)) {
    return(default)
  }
  value <- value[!is.na(value)]
  if (length(value) == 0L) {
    return(default)
  }
  paste(as.character(value), collapse = ", ")
}

# A gene list (JSON array, or a string stored by older jobs) as "A; B; C".
.marker_text <- function(value) {
  if (is.null(value)) {
    return("")
  }
  items <- if (is.list(value)) value else as.list(value)
  items <- Filter(
    function(item) is.atomic(item) && length(item) == 1L && !is.na(item),
    items
  )
  paste(vapply(items, as.character, character(1)), collapse = "; ")
}

.reviewer_confidence_label <- function(entry) {
  confidence <- .json_get(entry, "reviewerConfidence")
  if (.is_string(confidence) && confidence %in% .REVIEWER_CONFIDENCE_LEVELS) {
    confidence
  } else {
    "Unknown"
  }
}

# Map CyteType cluster IDs to the user's labels; unmapped IDs are kept.
.map_cluster_labels <- function(cluster_ids, cluster_map) {
  map_names <- names(cluster_map)
  if (is.null(cluster_map) || is.null(map_names) || length(cluster_ids) == 0L) {
    return(cluster_ids)
  }
  vapply(
    cluster_ids,
    function(cluster_id) {
      if (cluster_id %in% map_names) {
        .scalar_text(cluster_map[[cluster_id]], cluster_id)
      } else {
        cluster_id
      }
    },
    character(1),
    USE.NAMES = FALSE
  )
}

.normalized_entries <- function(result) {
  annotations <- .json_get(result, "annotations")
  if (!is.list(annotations) || is.data.frame(annotations)) {
    return(list())
  }
  Filter(.is_json_object, annotations)
}

# Transform CyteType results into the per-cluster table stored in
# obj@misc$cytetype_results: one row per annotated cluster. `cluster_map` maps
# CyteType cluster IDs to the user's cluster labels.
.transform_results_seurat <- function(raw_results = NULL, cluster_map = NULL) {
  entries <- .normalized_entries(.normalize_results(raw_results))
  text <- function(key, default) {
    vapply(
      entries,
      function(entry) .scalar_text(.json_get(entry, key), default),
      character(1),
      USE.NAMES = FALSE
    )
  }
  markers <- function(key) {
    vapply(
      entries,
      function(entry) .marker_text(.json_get(entry, key)),
      character(1),
      USE.NAMES = FALSE
    )
  }
  optional <- function(key, check, missing) {
    vapply(
      entries,
      function(entry) {
        value <- .json_get(entry, key)
        if (check(value)) value else missing
      },
      missing,
      USE.NAMES = FALSE
    )
  }

  table <- data.frame(
    clusterId = .map_cluster_labels(text("clusterId", ""), cluster_map),
    annotation = text("annotation", "Unknown"),
    ontologyTerm = text("ontologyTerm", "Unknown"),
    ontologyTermID = text("ontologyTermID", "Unknown"),
    granularAnnotation = text("granularAnnotation", ""),
    cellState = text("cellState", ""),
    justification = text("justification", ""),
    supportingMarkers = markers("supportingMarkers"),
    conflictingMarkers = markers("conflictingMarkers"),
    missingExpression = text("missingExpression", ""),
    unexpectedExpression = text("unexpectedExpression", ""),
    reviewerConfidence = optional(
      "reviewerConfidence",
      function(value) .is_string(value) && value %in% .REVIEWER_CONFIDENCE_LEVELS,
      NA_character_
    ),
    isHeterogeneous = optional(
      "isHeterogeneous",
      function(value) is.logical(value) && length(value) == 1L,
      NA
    ),
    heterogeneityType = optional(
      "heterogeneityType",
      .is_nonblank_string,
      NA_character_
    ),
    stringsAsFactors = FALSE
  )

  ids <- table$clusterId
  if (length(ids) > 0L && all(nzchar(ids)) && !anyDuplicated(ids)) {
    rownames(table) <- ids
  }
  table
}
