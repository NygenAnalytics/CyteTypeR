
#'
#'
#'
#' @importFrom purrr list_rbind map_dfr
#' @importFrom tibble tibble
#' @importFrom Matrix rowSums
#'
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

# Calculate Expression Percentages by Cluster
.calculate_pcent <- function(
    seurat_obj,
    group_key,
    cluster_map,
    batch_size = 2000
){
  gene_names <- rownames(seurat_obj)
  assay <- .resolve_seurat_assay_rna(seurat_obj)
  clusters <- seurat_obj[[group_key]][[group_key]]
  unique_clusters <- unique(clusters)

  pcent <- list()
  n_genes <- dim(seurat_obj)[1]
  n_batches <- ceiling(n_genes / batch_size)

  # Initialize results list
  results_list <- list()


  for (batch_idx in 1:n_batches) {
    start_idx <- (batch_idx - 1) * batch_size + 1
    end_idx <- min(batch_idx * batch_size, n_genes)


    batch_genes <- gene_names[start_idx:end_idx]
    batch_expr_full <- tryCatch(
      Seurat::GetAssayData(seurat_obj, assay = assay, layer = "data"),
      error = function(e) Seurat::GetAssayData(seurat_obj, assay = assay, slot = "data")
    )
    batch_expr_matrix <- batch_expr_full[batch_genes, , drop = FALSE]


    for (cluster in unique_clusters) {
      cluster_cells <- which(clusters == cluster)
      cluster_matrix <- batch_expr_matrix[, cluster_cells, drop = FALSE]

      # Calculate percentage of cluster cells expressing a gene (> 0)
      batch_percentages <- rowSums(cluster_matrix > 0) / ncol(cluster_matrix) * 100
      cluster_idx <- cluster_map[[cluster]]

      for (gene in batch_genes){
        if (is.null(results_list[[gene]])) {
          results_list[[gene]] <- list()
        }
        results_list[[gene]][cluster_idx] <- round(batch_percentages, 2)[[gene]]
      }
    }
    # Optional progress reporting
    if (requireNamespace("logger", quietly = TRUE)) {
      logger::log_info("Processed batch {batch_idx}/{n_batches} ({length(batch_genes)} genes)")
    }
  }
  if (requireNamespace("logger", quietly = TRUE)) {
    logger::log_info("Processed {n_batches} batches and a total of {end_idx} genes")
  }

  return(results_list)
}

# Aggregate Metadata by Cluster
.aggregate_metadata <- function(
    seurat_obj,
    group_key,
    min_percentage = 10)
{
  metadata <- seurat_obj@meta.data
  # Get unique groups and initialize result structure
  unique_groups <- unique(metadata[[group_key]])
  unique_groups <- unique_groups[!is.na(unique_groups)]
  unique_groups <- as.character(sort(unique_groups))

  # Initialize result list
  result <- setNames(
    lapply(unique_groups, function(x) setNames(list(), character(0))),
    unique_groups
  )

  for (column in colnames(metadata)){
    if (column == group_key){
      next # skip group_key group in metadata
    }
    if (inherits(metadata[[column]], "factor") || is.character(metadata[[column]])){

      # Create "crosstab": rows = group values in a metadata cat, columns = groups in group_key
      crosstab <- table(metadata[[column]], metadata[[group_key]], useNA = "no")

      # Percentages in each column(metadata group with prop.table margin=2)
      percentage_matrix <- round(100 * prop.table(crosstab, margin = 2), 0)

      # Convert matrix to data frame for easier manipulation
      percentage_df <- as.data.frame.table(percentage_matrix)
      colnames(percentage_df) <- c("value", "group", "percentage")

      # Filter for significant values (> min_percentage)
      significant_df <- percentage_df[percentage_df$percentage > min_percentage, ]

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

# Sample visualization coordinates per group (mirrors Python max_cells_per_group).
.sample_visualization_data <- function(obj, group_key, coordinates_key, cluster_map, max_cells_per_group = 1000) {
  coords <- Embeddings(obj, reduction = coordinates_key)
  if (ncol(coords) > 2) coords <- coords[, 1:2]
  groups <- obj[[group_key]][[group_key]]
  idx_by_group <- split(seq_len(nrow(coords)), as.character(groups))
  set.seed(42)
  sampled_idx <- unlist(lapply(idx_by_group, function(ii) {
    n <- min(max_cells_per_group, length(ii))
    sample(ii, n)
  }), use.names = FALSE)
  coords_sampled <- coords[sampled_idx, , drop = FALSE]
  clusters_sampled <- as.character(groups[sampled_idx])
  cluster_ids <- as.character(cluster_map[clusters_sampled])
  coordinates_list <- lapply(seq_len(nrow(coords_sampled)), function(i) as.numeric(coords_sampled[i, ]))
  list(coordinates = coordinates_list, clusters = cluster_ids)
}

# Validate Gene Symbols in Seurat Object
.validate_gene_symbols <- function(seurat_obj, gene_symbols){
  gene_values <- tryCatch({

    assay_name <- .resolve_seurat_assay_rna(seurat_obj)
    assay_obj <- tryCatch(seurat_obj[[assay_name]], error = function(e) NULL)
    meta_features <- tryCatch(
      assay_obj@meta.features,
      error = function(e) tryCatch(assay_obj@meta.data, error = function(e2) NULL)
    )
    if (is.null(meta_features)) stop("No feature metadata found on assay")

    symbols <- meta_features[[gene_symbols]]

    if (is.null(symbols) || all(is.na(symbols)) || length(symbols) == 0) {
      stop("No valid gene symbols found in meta.features")
    }

    return(symbols)

  }, error = function(e) {
    # Fallback to rownames
    message("Gene symbols not found in meta.features, using rownames")
    return(rownames(seurat_obj))
  })

  if (length(gene_values) == 0){
    log_warn("Gene values not found or are NA values")
  }

  id_pattern <- "ENS[A-Z]*G[0-9]{11}$|^[NX][MR]_[0-9]+$"
  matching_genes <- gene_values[grepl(id_pattern, gene_values)]

  if (length(matching_genes)/length(gene_values) > 0.5){
    log_warn("More than half of the gene values appears to be gene ids. The annotation might not be accurate. Consider using a column that contains human-readable gene symbols (e.g., 'TSPAN6', 'DPM1', 'SCYL3') instead of database identifiers.")
  }

}

# Validate Seurat Object Structure
.validate_seurat <- function(seurat_obj, group_key, gene_symbols, coordinates_key){
  if (!inherits(seurat_obj, "Seurat")){
    stop("Please provide a Seurat Object")
  }
  .validate_gene_symbols(seurat_obj,gene_symbols)

  if (!(coordinates_key %in% names(seurat_obj@reductions))){
    log_info("Coordinates key {coordinates_key} not found in reductions.")
  }
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

  # gene symbol column exist?
  # has gene symbols?
  if ("gene" %in% table_cols){
    gene_sym_pattern <- "^[A-Z]([A-Z0-9-]+|[a-z0-9-]+)$"
    if (is.null(grepl(gene_sym_pattern,marker_table$gene))){
      log_warn("Not gene symbols?")
    }
    else {log_info(paste("Found gene symbols", cli::symbol$tick))}
  }
  # cluster labels == seurat obj labels?
  if (setequal(sorted_clusters, as.vector(marker_table$cluster))){
    log_info(paste("Correct cluster labels", cli::symbol$tick))
  }
  else{
    log_error("Please check if cluster labels are consistent between marker table and seurat obj!")
  }

  log_info(paste("Markers check: done", cli::symbol$tick))
}

# Store job details in Seurat object misc (no auth_token in stored list).
.store_job_details_seurat <- function(obj, job_id, api_url, results_prefix, group_key = NULL, cluster_labels = NULL) {
  if (is.null(obj@misc)) obj@misc <- list()
  obj@misc[[paste0(results_prefix, "_jobDetails")]] <- list(
    job_id = job_id,
    report_url = file.path(api_url, "report", job_id),
    api_url = api_url,
    group_key = group_key,
    cluster_labels = cluster_labels
  )
  obj
}

# Normalize API result annotations to a flat list (handles dict or list format).
.normalize_result_annotations <- function(result) {
  raw <- result$annotations
  if (is.null(raw) || !is.list(raw)) return(result)
  items <- if (length(names(raw)) > 0L && !identical(names(raw), as.character(seq_along(raw)))) {
    lapply(seq_along(raw), function(i) {
      x <- raw[[i]]
      ann <- if (is.list(x$latest) && is.list(x$latest$annotation)) x$latest$annotation else x
      list(
        clusterId = ann$clusterId %||% names(raw)[i] %||% "",
        annotation = ann$annotation %||% "Unknown",
        ontologyTerm = ann$cellOntologyTermName %||% ann$cellOntologyTerm %||% "Unknown",
        ontologyTermID = ann$cellOntologyTerm %||% "Unknown",
        cellState = ann$cellState %||% "",
        granularAnnotation = ann$granularAnnotation %||% ""
      )
    })
  } else {
    lapply(raw, function(x) {
      ann <- if (is.list(x$latest) && is.list(x$latest$annotation)) x$latest$annotation else x
      list(
        clusterId = ann$clusterId %||% "",
        annotation = ann$annotation %||% "Unknown",
        ontologyTerm = ann$cellOntologyTermName %||% ann$cellOntologyTerm %||% ann$ontologyTerm %||% "Unknown",
        ontologyTermID = ann$cellOntologyTerm %||% ann$ontologyTermID %||% "Unknown",
        cellState = ann$cellState %||% "",
        granularAnnotation = ann$granularAnnotation %||% ""
      )
    })
  }
  result$annotations <- items
  result
}

# Add annotation columns to obj@meta.data and store full result in obj@misc.
.store_annotations_seurat <- function(obj, result, job_id, results_prefix, group_key, cluster_labels) {
  ann_list <- result$annotations
  if (is.null(ann_list) || length(ann_list) == 0L) return(obj)
  orig_to_id <- setNames(names(cluster_labels), as.character(cluster_labels))
  cells_group <- as.character(obj[[group_key, drop = TRUE]])
  clusters_per_cell <- as.character(orig_to_id[cells_group])
  .add_col <- function(field_key, col_name, default = "") {
    field_map <- setNames(
      vapply(ann_list, function(a) as.character(a[[field_key]] %||% default), character(1)),
      vapply(ann_list, function(a) as.character(a$clusterId %||% ""), character(1))
    )
    vals <- as.character(field_map[clusters_per_cell])
    vals[is.na(vals)] <- default
    obj@meta.data[[col_name]] <<- factor(vals)
    NULL
  }
  prefix <- results_prefix
  gk <- group_key
  .add_col("annotation", paste0(prefix, "_annotation_", gk), "Unknown")
  .add_col("ontologyTerm", paste0(prefix, "_cellOntologyTerm_", gk), "Unknown")
  .add_col("ontologyTermID", paste0(prefix, "_cellOntologyTermID_", gk), "Unknown")
  .add_col("cellState", paste0(prefix, "_cellState_", gk), "")
  if (is.null(obj@misc)) obj@misc <- list()
  obj@misc[[paste0(results_prefix, "_results")]] <- list(job_id = job_id, result = result)
  obj
}

# Transform CyteType Results for Seurat Integration (handles raw or normalized annotations).
.transform_results_seurat <- function(raw_results = NULL, job_id = NULL, filename = NULL, cluster_map = NULL){
  raw_annotations <- raw_results$annotations

  df <- map_dfr(raw_annotations, ~ {
    ann <- if (is.character(.x$annotation %||% NULL)) .x else .x$latest$annotation
    if (!is.list(ann)) ann <- list()
    data.frame(
      clusterId = as.character(cluster_map[ann$clusterId] %||% ann$clusterId %||% ""),
      annotation = as.character(ann$annotation %||% 'Unknown'),
      ontologyTerm = as.character(ann$cellOntologyTermName %||% ann$cellOntologyTerm %||% ann$ontologyTerm %||% 'Unknown'),
      ontologyTermID = as.character(ann$cellOntologyTerm %||% ann$ontologyTermID %||% 'Unknown'),
      granularAnnotation = as.character(ann$granularAnnotation %||% ''),
      cellState = as.character(ann$cellState %||% ''),
      justification = as.character(ann$justification %||% ''),
      supportingMarkers = paste(as.character(ann$supportingMarkers %||% ''), collapse = "; "),
      conflictingMarkers = paste(as.character(ann$conflictingMarkers %||% ''), collapse = "; "),
      missingExpression = as.character(ann$missingExpression %||% ''),
      unexpectedExpression = as.character(ann$unexpectedExpression %||% ''),
      stringsAsFactors = FALSE
    )
  })

  rownames(df) <- df$clusterId

  #cluster categories
  raw_cats <- raw_results$clusterCategories

  # df$clusterCategories

  return(df)
}

