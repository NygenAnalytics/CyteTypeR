#' @importFrom Matrix rowSums
NULL

# Resolve/validate Seurat RNA assay for RNA-seq workflows (strict).
.resolve_seurat_assay_rna <- function(seurat_obj) {
  assays <- tryCatch(names(seurat_obj@assays), error = function(e) character(0))
  if (length(assays) == 0) stop("No assays found in Seurat object.")
  if (!("RNA" %in% assays)) {
    stop(
      "This workflow requires an assay named 'RNA'. Available assays: ",
      paste0(assays, collapse = ", "),
      "."
    )
  }

  default_assay <- tryCatch(Seurat::DefaultAssay(seurat_obj), error = function(e) NULL)
  if (!is.null(default_assay) && nzchar(as.character(default_assay)) && default_assay != "RNA") {
    warning(
      "DefaultAssay(seurat_obj) is '", as.character(default_assay),
      "'. Using assay='RNA' instead."
    )
  }

  "RNA"
}

# Layers of a Seurat v5 assay whose names start with `layer`, such as
# "data" or the split layers "data.batch1" and "data.batch2".
.assay5_layers <- function(assay_obj, layer) {
  found <- suppressWarnings(SeuratObject::Layers(assay_obj, search = layer))
  if (is.null(found)) character(0) else found
}

# One layer ("data" or "counts") of an assay as a features x cells matrix in
# the object's cell order, or NULL when the assay has no such layer. Split
# Seurat v5 layers are joined on a local copy; the object itself is unchanged.
.get_rna_layer <- function(obj, layer = c("data", "counts"), assay = "RNA") {
  layer <- match.arg(layer)
  assay_obj <- obj[[assay]]
  if (methods::is(assay_obj, "Assay")) {
    # Seurat v3/v4 assays (also in SeuratObject 5) store each layer in a slot.
    mat <- methods::slot(assay_obj, layer)
  } else if (methods::is(assay_obj, "StdAssay")) {
    found <- .assay5_layers(assay_obj, layer)
    if (length(found) == 0L) {
      return(NULL)
    }
    if (length(found) > 1L || !identical(found, layer)) {
      assay_obj <- SeuratObject::JoinLayers(assay_obj, layers = layer)
    }
    mat <- SeuratObject::LayerData(assay_obj, layer = layer)
  } else {
    stop(
      "Unsupported assay class for '", assay, "': ",
      paste(class(assay_obj), collapse = "/"),
      call. = FALSE
    )
  }
  if (is.null(mat) || nrow(mat) == 0L || ncol(mat) == 0L) {
    return(NULL)
  }
  cells <- colnames(obj)
  if (!identical(colnames(mat), cells)) {
    if (!is.null(colnames(mat)) && setequal(colnames(mat), cells) &&
        length(cells) == ncol(mat)) {
      mat <- mat[, cells, drop = FALSE]
    } else {
      stop(
        "The '", layer, "' layer of the ", assay,
        " assay does not contain the same cells as the object.",
        call. = FALSE
      )
    }
  }
  mat
}

# Feature metadata of an assay, with feature names as row names.
.assay_feature_metadata <- function(obj, assay = "RNA") {
  assay_obj <- obj[[assay]]
  feature_df <- tryCatch(
    as.data.frame(assay_obj[[]]),
    error = function(e) tryCatch(
      as.data.frame(methods::slot(assay_obj, "meta.features")),
      error = function(e2) tryCatch(
        as.data.frame(methods::slot(assay_obj, "meta.data")),
        error = function(e3) NULL
      )
    )
  )
  if (is.null(feature_df)) {
    return(NULL)
  }
  features <- rownames(assay_obj)
  if (.row_names_info(feature_df) < 0L && nrow(feature_df) == length(features)) {
    rownames(feature_df) <- features
  }
  feature_df
}

# TRUE when every value of a count matrix is a finite whole number. Checked in
# chunks so a large matrix is not copied at once.
.is_integer_valued <- function(mat, chunk_size = 1e7) {
  if (is.null(mat)) {
    return(FALSE)
  }
  if (methods::is(mat, "Matrix")) {
    if (!methods::.hasSlot(mat, "x")) {
      return(TRUE)
    }
    values <- mat@x
  } else if (is.matrix(mat)) {
    values <- mat
  } else {
    return(FALSE)
  }
  if (is.integer(values) || is.logical(values)) {
    return(!anyNA(values))
  }
  if (!is.numeric(values)) {
    return(FALSE)
  }
  n <- length(values)
  start <- 1
  while (start <= n) {
    end <- min(n, start + chunk_size - 1)
    chunk <- values[start:end]
    if (!all(is.finite(chunk)) || any(chunk != floor(chunk))) {
      return(FALSE)
    }
    start <- end + 1
  }
  TRUE
}

# Calculate Expression Percentages by Cluster
.calculate_pcent <- function(
    seurat_obj,
    group_key,
    cluster_map,
    batch_size = 2000,
    expr = NULL
){
  if (is.null(expr)) {
    expr <- .get_rna_layer(seurat_obj, "data")
  }
  if (is.null(expr)) {
    stop("The RNA assay has no normalized 'data' layer. Run NormalizeData() first.", call. = FALSE)
  }
  gene_names <- rownames(expr)
  clusters <- as.character(seurat_obj[[group_key, drop = TRUE]])
  unique_clusters <- unique(clusters)
  cluster_ids <- vapply(
    unique_clusters,
    function(cluster) as.character(cluster_map[[cluster]]),
    character(1),
    USE.NAMES = FALSE
  )
  cells_by_cluster <- lapply(unique_clusters, function(cluster) which(clusters == cluster))

  n_genes <- nrow(expr)
  batch_size <- max(1L, as.integer(batch_size))
  n_batches <- ceiling(n_genes / batch_size)
  pcent <- matrix(0, nrow = n_genes, ncol = length(unique_clusters))

  for (batch_idx in seq_len(n_batches)) {
    start_idx <- (batch_idx - 1) * batch_size + 1
    end_idx <- min(batch_idx * batch_size, n_genes)
    batch_expr <- expr[start_idx:end_idx, , drop = FALSE]
    for (k in seq_along(unique_clusters)) {
      cells <- cells_by_cluster[[k]]
      expressing <- Matrix::rowSums(batch_expr[, cells, drop = FALSE] > 0)
      # Percentage of the cluster's cells expressing each gene (> 0).
      pcent[start_idx:end_idx, k] <- expressing / length(cells) * 100
    }
    logger::log_info("Processed batch {batch_idx}/{n_batches} ({end_idx - start_idx + 1} genes)")
  }
  logger::log_info("Processed {n_batches} batches and a total of {n_genes} genes")

  pcent <- round(pcent, 2)
  results_list <- lapply(seq_len(n_genes), function(i) {
    stats::setNames(as.list(pcent[i, ]), cluster_ids)
  })
  names(results_list) <- gene_names
  results_list
}

# Aggregate Metadata by Cluster
.aggregate_metadata <- function(
    seurat_obj,
    group_key,
    min_percentage = 10,
    max_metadata_categories = 128L)
{
  metadata <- droplevels(seurat_obj@meta.data)
  # Get unique groups and initialize result structure
  unique_groups <- unique(metadata[[group_key]])
  unique_groups <- unique_groups[!is.na(unique_groups)]
  unique_groups <- as.character(sort(unique_groups))

  # Initialize result list
  result <- setNames(
    lapply(unique_groups, function(x) list()),
    unique_groups
  )

  for (column in colnames(metadata)){
    if (column == group_key){
      next
    }
    if (inherits(metadata[[column]], "factor") || is.character(metadata[[column]])){
      n_unique <- length(unique(metadata[[column]]))
      if (n_unique > max_metadata_categories) next

      crosstab <- table(metadata[[column]], metadata[[group_key]], useNA = "no")

      # Percentages in each column(metadata group with prop.table margin=2)
      percentage_matrix <- round(100 * prop.table(crosstab, margin = 2), 0)

      # Convert matrix to data frame for easier manipulation
      percentage_df <- as.data.frame.table(percentage_matrix)
      colnames(percentage_df) <- c("value", "group", "percentage")

      # Filter for significant values (> min_percentage)
      significant_df <- percentage_df[which(percentage_df$percentage > min_percentage), ]

      if (nrow(significant_df) > 0) {
        # Organize results by group
        for (i in seq_len(nrow(significant_df))) {
          group_name <- as.character(significant_df$group[i])
          value_name <- as.character(significant_df$value[i])
          percentage_val <- as.integer(significant_df$percentage[i])

          # Initialize column list if it doesn't exist
          if (is.null(result[[group_name]][[column]])) {
            result[[group_name]][[column]] <- list()
          }

          # Store the percentage
          result[[group_name]][[column]][[value_name]] <- percentage_val
        }
      }
    }
  }

  return(result)
}

# The reduction used for the report's visualization: `coordinates_key`, then
# "umap", "tsne" and "pca" (case-insensitive), the first with one row per cell
# and at least two dimensions. NULL when none qualifies.
.resolve_coordinates_key <- function(obj, coordinates_key = "umap") {
  available <- names(obj@reductions)
  candidates <- unique(c(coordinates_key, "umap", "tsne", "pca"))
  candidates <- candidates[!is.na(candidates) & nzchar(candidates)]
  n_cells <- ncol(obj)
  for (candidate in candidates) {
    matched <- available[tolower(available) == tolower(candidate)]
    if (length(matched) == 0L) {
      next
    }
    key <- matched[[1L]]
    coords <- tryCatch(SeuratObject::Embeddings(obj, reduction = key), error = function(e) NULL)
    if (is.null(coords)) {
      next
    }
    if (nrow(coords) != n_cells) {
      log_warn("Coordinates in '{key}' have {nrow(coords)} rows, expected {n_cells}.")
      next
    }
    if (ncol(coords) < 2L) {
      log_warn("Coordinates in '{key}' have {ncol(coords)} dimension, need at least 2.")
      next
    }
    if (!identical(key, coordinates_key)) {
      log_info("Using coordinates from '{key}' for visualization (coordinates_key was '{coordinates_key}').")
    } else {
      log_info("Using coordinates from '{key}' for visualization.")
    }
    return(key)
  }
  log_warn(paste0(
    "No suitable 2D coordinates found in the object's reductions. Looked for: ",
    paste(candidates, collapse = ", "),
    " (case-insensitive). Available reductions: ",
    if (length(available)) paste(available, collapse = ", ") else "none",
    ". Visualization will be disabled."
  ))
  NULL
}

# Embedding of a reduction (first two dimensions) in the object's cell order.
.reduction_coordinates <- function(obj, coordinates_key) {
  coords <- SeuratObject::Embeddings(obj, reduction = coordinates_key)
  cells <- colnames(obj)
  if (!is.null(rownames(coords)) && !identical(rownames(coords), cells) &&
      all(cells %in% rownames(coords))) {
    coords <- coords[cells, , drop = FALSE]
  }
  coords[, seq_len(min(2L, ncol(coords))), drop = FALSE]
}

# Sample visualization coordinates per group (mirrors Python max_cells_per_group).
# The fixed seed keeps the sample reproducible without changing the caller's
# random number stream.
.sample_visualization_data <- function(obj, group_key, coordinates_key, cluster_map, max_cells_per_group = 1000) {
  coords <- .reduction_coordinates(obj, coordinates_key)
  groups <- as.character(obj[[group_key, drop = TRUE]])
  idx_by_group <- split(seq_len(nrow(coords)), groups)
  sampled_idx <- withr::with_seed(42, {
    unlist(lapply(idx_by_group, function(ii) {
      n <- min(max_cells_per_group, length(ii))
      # Index with sample.int: sample(ii, n) samples 1:ii when ii is one number.
      ii[sample.int(length(ii), n)]
    }), use.names = FALSE)
  })
  coords_sampled <- coords[sampled_idx, , drop = FALSE]
  clusters_sampled <- groups[sampled_idx]
  cluster_ids <- as.character(cluster_map[clusters_sampled])
  coordinates_list <- lapply(seq_len(nrow(coords_sampled)), function(i) as.numeric(coords_sampled[i, ]))
  list(coordinates = coordinates_list, clusters = cluster_ids)
}

# TRUE for values that look like gene identifiers rather than gene symbols
# (mirrors _is_gene_id_like in the Python SDK): Ensembl gene IDs with an
# optional version suffix, RefSeq accessions, numeric (Entrez) IDs, and long
# dotted or underscored IDs.
.is_gene_id_like <- function(values) {
  values <- trimws(as.character(values), whitespace = .WHITESPACE_PATTERN)
  valid <- !is.na(values) & nzchar(values)
  values[!valid] <- ""
  ensembl <- grepl("^ENS[A-Z]*G[0-9]{11}(\\.[0-9]+)?$", values, ignore.case = TRUE, perl = TRUE)
  refseq <- grepl("^[NX][MR]_[0-9]+$", values, perl = TRUE)
  numeric_id <- grepl("^[0-9]+(\\.0)?$", values, perl = TRUE)
  long_id <- grepl("^[A-Z0-9]+[._][A-Z0-9._]+$", values, perl = TRUE) & nchar(values) > 10L
  valid & (ensembl | refseq | numeric_id | long_id)
}

# Percentage of values that look like gene IDs (100 for no values, as in the
# Python SDK). Unlike the Python SDK, every value is checked, not a sample.
.id_like_percentage <- function(values) {
  values <- as.character(values)
  if (length(values) == 0L) {
    return(100)
  }
  100 * mean(.is_gene_id_like(values))
}

.id_like_examples <- function(values, n = 3L) {
  values <- as.character(values)
  values <- values[!is.na(values)]
  examples <- utils::head(values[.is_gene_id_like(values)], n)
  paste0("'", examples, "'", collapse = ", ")
}

# Validate Gene Symbols in Seurat Object (warns only).
.validate_gene_symbols <- function(seurat_obj, gene_symbols, assay = "RNA"){
  gene_values <- tryCatch({
    meta_features <- .assay_feature_metadata(seurat_obj, assay)
    if (is.null(meta_features)) stop("No feature metadata found on assay")

    symbols <- meta_features[[gene_symbols]]

    if (is.null(symbols) || length(symbols) == 0 || all(is.na(symbols))) {
      stop("No valid gene symbols found in meta.features")
    }

    symbols

  }, error = function(e) {
    # Fallback to the RNA assay's feature names
    message("Gene symbols not found in meta.features, using rownames")
    rownames(seurat_obj[[assay]])
  })

  gene_values <- as.character(gene_values)
  gene_values <- gene_values[!is.na(gene_values)]
  if (length(gene_values) == 0L) {
    log_warn("Gene values not found or are NA values")
    return(invisible(NULL))
  }
  pct <- .id_like_percentage(gene_values)

  if (pct > 50) {
    log_warn(paste0(
      "More than half of the gene values (", format(round(pct, 1), nsmall = 1),
      "%) appear to be gene IDs (e.g., ", .id_like_examples(gene_values), "). ",
      "The annotation might not be accurate. Consider using a column that contains ",
      "human-readable gene symbols (e.g., 'TSPAN6', 'DPM1', 'SCYL3') instead of database identifiers."
    ))
  } else if (pct > 20) {
    log_warn(paste0(
      format(round(pct, 1), nsmall = 1), "% of the gene values look like gene IDs. ",
      "Please verify that they are gene symbols rather than gene identifiers."
    ))
  }
  invisible(NULL)
}

# Stop early when cells have no cluster label.
.check_group_labels <- function(groups, group_key) {
  n_na <- sum(is.na(groups))
  if (n_na == 0L) {
    return(invisible(TRUE))
  }
  n_cells <- length(groups)
  if (n_na == n_cells) {
    stop(
      "All ", n_cells, " cells have NA values in '", group_key,
      "'. Cannot prepare the data for annotation.",
      call. = FALSE
    )
  }
  stop(
    n_na, " cells (", format(round(100 * n_na / n_cells, 1), nsmall = 1),
    "%) have NA values in '", group_key, "'. Assign these cells to a cluster, ",
    "or remove them (for example with subset()), before calling PrepareCyteTypeR().",
    call. = FALSE
  )
}

# Validate Seurat Object Structure
.validate_seurat <- function(seurat_obj, group_key, gene_symbols, coordinates_key = NULL){
  if (!inherits(seurat_obj, "Seurat")){
    stop("Please provide a Seurat Object")
  }

  .resolve_seurat_assay_rna(seurat_obj)

  if (!(group_key %in% names(seurat_obj@meta.data))){
    stop("Please provide a valid group key that exists in the Seurat object's meta.data.")
  }
  .check_group_labels(seurat_obj@meta.data[[group_key]], group_key)

  .validate_gene_symbols(seurat_obj, gene_symbols)
}


.validate_marker_table <- function(marker_table, sorted_clusters){
  log_info("Checking markers table...")
  df_like_classes <- c("data.frame", "tbl_df", "tbl", "data.table")
  if (inherits(marker_table, df_like_classes)){
    log_info(paste("Provided markers data is a dataframe", cli::symbol$tick))
  }

  if (any(is.na(marker_table))){
    log_info("NA value(s) found in the table")
  }
  else{ log_info(paste("No NA values", cli::symbol$tick))}

  table_cols <- names(marker_table)
  if ("cluster" %in% table_cols){
    log_info(paste("'cluster' column found in marker table", cli::symbol$tick))
  }
  else{log_info("cluster column not found in marker table, rename your column to 'cluster' if its name is different" )}

  # Gene symbols rather than gene IDs? Warn only.
  if ("gene" %in% table_cols){
    genes <- as.character(marker_table$gene)
    genes <- genes[!is.na(genes)]
    pct <- if (length(genes) > 0L) .id_like_percentage(genes) else 0
    if (pct > 50) {
      log_warn(paste0(
        format(round(pct, 1), nsmall = 1), "% of the genes in the marker table look like gene IDs (e.g., ",
        .id_like_examples(genes), "). CyteType expects gene symbols such as 'TSPAN6' or 'DPM1', ",
        "so the annotation might not be accurate."
      ))
    } else {
      log_info(paste("Found gene symbols", cli::symbol$tick))
    }
  }
  # cluster labels == seurat obj labels?
  if (setequal(as.character(sorted_clusters), as.character(marker_table$cluster))){
    log_info(paste("Correct cluster labels", cli::symbol$tick))
  }
  else{
    stop("Please check if cluster labels are consistent between marker table and seurat obj!")
  }
  log_info(paste("Markers check: done", cli::symbol$tick))
}

# Store job details in Seurat object misc (no auth_token in stored list).
.store_job_details_seurat <- function(obj, job_id, api_url, results_prefix, group_key = NULL,
                                      cluster_labels = NULL, visibility = NULL) {
  api_url <- .validate_api_url(api_url)
  if (is.null(obj@misc)) obj@misc <- list()
  job_details <- list(
    job_id = job_id,
    report_url = .url_path(api_url, "report", job_id),
    api_url = api_url,
    group_key = group_key,
    cluster_labels = cluster_labels
  )
  if (!is.null(visibility)) {
    job_details$visibility <- visibility
  }
  obj@misc[[paste0(results_prefix, "_jobDetails")]] <- job_details
  obj
}

# Normalize API result annotations (kept for callers of the 0.5.x helper).
.normalize_result_annotations <- function(result) {
  .normalize_results(result)
}

# The per-cluster results table with the job ID and the normalized results
# attached as attributes.
.results_table_with_attributes <- function(result, job_id, cluster_map = NULL) {
  table <- .transform_results_seurat(result, cluster_map = cluster_map)
  attr(table, "job_id") <- job_id
  attr(table, "result") <- result
  table
}

# Add the annotation columns to obj@meta.data and store the results in obj@misc.
# `result` holds normalized annotations (see .normalize_results()).
#
# With the default prefix, obj@misc$cytetype_results is the per-cluster table
# (a data.frame) with the job ID and the full results as the attributes
# "job_id" and "result". Other prefixes store list(job_id, result) in
# obj@misc[[paste0(prefix, "_results")]]; obj@misc$cytetype_results is then
# left as it is.
.store_annotations_seurat <- function(obj, result, job_id, results_prefix, group_key, cluster_labels,
                                      results_table = NULL) {
  entries <- .normalized_entries(result)
  label_ids <- names(cluster_labels)
  label_values <- vapply(
    cluster_labels,
    function(label) .scalar_text(label, NA_character_),
    character(1),
    USE.NAMES = FALSE
  )
  orig_to_id <- stats::setNames(label_ids, label_values)
  cells_group <- as.character(obj[[group_key, drop = TRUE]])
  cell_ids <- unname(orig_to_id[cells_group])

  entry_ids <- vapply(
    entries,
    function(entry) .scalar_text(.json_get(entry, "clusterId"), ""),
    character(1),
    USE.NAMES = FALSE
  )
  # As in the Python SDK, the last entry for a cluster ID wins.
  keep <- !duplicated(entry_ids, fromLast = TRUE)
  per_cell <- function(values, default) {
    lookup <- stats::setNames(values[keep], entry_ids[keep])
    out <- unname(lookup[cell_ids])
    out[is.na(out)] <- default
    out
  }
  text <- function(key, default) {
    vapply(
      entries,
      function(entry) .scalar_text(.json_get(entry, key), default),
      character(1),
      USE.NAMES = FALSE
    )
  }

  prefix <- results_prefix
  gk <- group_key
  obj@meta.data[[paste0(prefix, "_annotation_", gk)]] <-
    factor(per_cell(text("annotation", "Unknown"), "Unknown"))
  obj@meta.data[[paste0(prefix, "_cellOntologyTerm_", gk)]] <-
    factor(per_cell(text("ontologyTerm", "Unknown"), "Unknown"))
  obj@meta.data[[paste0(prefix, "_cellOntologyTermID_", gk)]] <-
    factor(per_cell(text("ontologyTermID", "Unknown"), "Unknown"))
  obj@meta.data[[paste0(prefix, "_cellState_", gk)]] <-
    factor(per_cell(text("cellState", ""), ""))
  confidence <- per_cell(
    vapply(entries, .reviewer_confidence_label, character(1), USE.NAMES = FALSE),
    "Unknown"
  )
  # Ordered High, Moderate, Low, Unknown; only levels that occur are kept.
  obj@meta.data[[paste0(prefix, "_reviewerConfidence_", gk)]] <- factor(
    confidence,
    levels = intersect(c(.REVIEWER_CONFIDENCE_LEVELS, "Unknown"), confidence)
  )

  if (is.null(obj@misc)) obj@misc <- list()
  if (identical(results_prefix, "cytetype")) {
    if (is.null(results_table)) {
      results_table <- .transform_results_seurat(result, cluster_map = cluster_labels)
    }
    attr(results_table, "job_id") <- job_id
    attr(results_table, "result") <- result
    obj@misc[["cytetype_results"]] <- results_table
  } else {
    obj@misc[[paste0(results_prefix, "_results")]] <- list(job_id = job_id, result = result)
  }
  obj
}

# The three columns kept under the names used before 0.4.0. They hold the
# label, the Cell Ontology term and its ID of each cell's cluster, and NA for
# cells in clusters without a result.
.add_legacy_annotation_columns <- function(obj, results_table, results_prefix, group_key) {
  ann_colname <- paste(results_prefix, group_key, sep = "_")
  onto_colname <- paste(results_prefix, "ontologyTerm", group_key, sep = "_")
  onto_id_colname <- paste(results_prefix, "ontologyID", group_key, sep = "_")
  cluster_ann_map <- stats::setNames(results_table$annotation, results_table$clusterId)
  cluster_onto_map <- stats::setNames(results_table$ontologyTerm, results_table$clusterId)
  cluster_onto_id_map <- stats::setNames(results_table$ontologyTermID, results_table$clusterId)
  cells_group <- as.character(as.vector(obj@meta.data[[group_key]]))
  obj@meta.data[[ann_colname]] <- factor(unname(cluster_ann_map[cells_group]))
  obj@meta.data[[onto_colname]] <- factor(unname(cluster_onto_map[cells_group]))
  obj@meta.data[[onto_id_colname]] <- factor(unname(cluster_onto_id_map[cells_group]))
  obj
}
