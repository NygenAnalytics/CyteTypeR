
#'
#'
#'
#' @importFrom purrr list_rbind map_dfr
#' @importFrom tibble tibble
#' @importFrom Matrix rowSums
#'
# Calculate Expression Percentages by Cluster
.calculate_pcent <- function(
    seurat_obj,
    group_key,
    cluster_map,
    batch_size = 2000
){
  gene_names <- rownames(seurat_obj)
  clusters <- seurat_obj[[group_key]][[group_key]]
  unique_clusters <- levels(clusters)

  pcent <- list()
  n_genes <- dim(seurat_obj)[1]
  n_batches <- ceiling(n_genes / batch_size)

  # Initialize results list
  results_list <- list()


  for (batch_idx in 1:n_batches) {
    start_idx <- (batch_idx - 1) * batch_size + 1
    end_idx <- min(batch_idx * batch_size, n_genes)


    batch_genes <- gene_names[start_idx:end_idx]
    batch_expr_matrix <- GetAssayData(seurat_obj, layer = "data")[batch_genes, , drop = FALSE]


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
    lapply(unique_groups, function(x) list()),
    unique_groups
  )

  for (column in colnames(metadata)){
    if (column == group_key){
      next # skip group_key group in metadata
    }
    if (class(metadata[[column]]) %in% c("factor", "character")){

      # Create "crosstab": rows = group vlaues in a metadata cat, columns = groups in group_key
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

# Validate Gene Symbols in Seurat Object
.validate_gene_symbols <- function(seurat_obj, gene_symbols){
  gene_values <- tryCatch({

    meta_features <- seurat_obj[["RNA"]]@meta.features

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


.validate_marker_table <- function(marker_table){
  # check if marker table has genes
  # check if marker table has clusters
  # check if marker table had logfold, scores etc to order by
  # if cant sort/order, skip and take first n_top_genes that was specified
}

# Transform CyteType Results for Seurat Integration

.transform_results_seurat <- function(raw_results = NULL, job_id = NULL, filename = NULL, cluster_map = NULL){
  # if (is.null(job_id)){
  #   stop("The job id or the file path for the results file is missing.")
  # }

  # if (is.null(raw_results)){
  #     filename <- filename %||% paste0('cytetypeR_results_', job_id,'.json')
  #     raw_results <- read_json(filename)
  # }

  raw_annotations <- raw_results$annotations
  annotations_list <- list()

  df <- map_dfr(raw_annotations, ~ {
    ann <- .x$latest$annotation
    data.frame(
      clusterId = as.character(cluster_map[ann$clusterId] %||% ann$clusterId),
      annotation = as.character(ann$annotation %||% 'Unknown'),
      ontologyTerm = as.character(ann$cellOntologyTerm %||% 'Unknown'),
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


  return(df)
}

