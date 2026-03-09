#' Prepare Seurat Object for CyteType Analysis
#'
#' @description
#' Prepares a Seurat object and a gene marker table for CyteType API by extracting and formatting
#' cluster labels, metadata, marker genes, visualization coordinates, and
#' expression data into the required data structure.
#'
#' @param obj A Seurat object with normalized data and clustering results.
#' @param marker_table Data frame containing marker genes with columns:
#'   'cluster', 'gene', and 'avg_log2FC'. Output from Seurat's
#'   `FindAllMarkers()` function.
#' @param group_key Character string specifying the metadata column containing
#'   cluster assignments. Default is "seurat_clusters".
#' @param gene_symbols Character string specifying the gene symbol field name. Default is 'gene_symbols'.
#' @param n_top_genes Integer specifying the maximum number of top marker genes
#'   per cluster to include (filtered by avg_log2FC > 1). Default is 50.
#' @param aggregate_metadata Logical indicating whether to aggregate metadata
#'   across cells within each cluster. Default is `TRUE`.
#' @param min_percentage Numeric threshold for minimum percentage.
#'   Default is 10.
#' @param pcent_batch_size Integer specifying batch size for expression percentage
#'   calculations. Default is 5000.
#' @param coordinates_key Character string specifying which dimensional reduction
#'   to use for visualization coordinates (e.g., "umap", "tsne"). Default is "umap".
#' @param max_cells_per_group Integer specifying maximum cells per cluster for
#'   subsampling (currently unused). Default is 1000.
#' @param vars_h5_path Character string specifying the local file path for the
#'   generated vars.h5 artifact (feature expression). Default is `"vars.h5"`.
#' @param obs_duckdb_path Character string specifying the local file path for the
#'   generated obs.duckdb artifact (cell metadata). Default is `"obs.duckdb"`.
#'
#' @return Named list containing formatted data for CyteType analysis:
#'   \itemize{
#'     \item `clusterLabels`: List mapping numeric cluster IDs to original labels
#'     \item `clusterMetadata`: Aggregated metadata by cluster (if enabled)
#'     \item `markerGenes`: List of top marker genes per cluster
#'     \item `visualizationData`: Coordinates and cluster assignments for plotting
#'     \item `expressionData`: Expression percentages by cluster
#'     \item `group_key`: The metadata column name used for cluster assignments
#'     \item `build_succeeded`: Logical indicating whether artifact files were built successfully
#'     \item `vars_h5_path`: Path to the generated vars.h5 file
#'     \item `obs_duckdb_path`: Path to the generated obs.duckdb file
#'   }
#'
#' @details
#' The function performs the following steps:
#' 1. Validates the Seurat object structure
#' 2. Creates a mapping from original cluster labels to sequential numeric IDs
#' 3. Aggregates metadata across cells within clusters (optional)
#' 4. Filters and selects top marker genes per cluster
#' 5. Extracts dimensional reduction coordinates for visualization
#' 6. Calculates expression percentages across clusters
#' 7. Builds a vars.h5 artifact from the normalized expression matrix
#' 8. Builds an obs.duckdb artifact from the cell metadata
#'
#' Clusters are renumbered sequentially (1, 2, 3, ...) to ensure consistent
#' formatting regardless of original cluster naming.
#'
#' @examples
#' \dontrun{
#' # Prepare Seurat object with UMAP coordinates
#' prepped_data <- PrepareCyteTypeR(
#'   obj = seurat_obj,
#'   marker_table = markers_df,
#'   group_key = "seurat_clusters",
#'   n_top_genes = 30,
#'   coordinates_key = "umap"
#' )
#' }
#'
#' @importFrom logger log_info log_debug log_error log_warn
#' @importFrom dplyr group_by filter slice_head ungroup arrange desc %>%
#' @importFrom tidyr pivot_longer
#' @importFrom stats setNames
#' @importFrom Seurat Embeddings GetAssayData AddMetaData
#' @importFrom jsonlite write_json fromJSON
#' @export
PrepareCyteTypeR <- function(obj,
                             marker_table,
                             group_key = "seurat_clusters",
                             gene_symbols = 'gene_symbols',
                             n_top_genes = 50,
                             aggregate_metadata = TRUE,
                             min_percentage = 10,
                             pcent_batch_size = 5000,
                             coordinates_key = "umap",
                             max_cells_per_group = 1000,
                             vars_h5_path = "vars.h5",
                             obs_duckdb_path = "obs.duckdb"
){
  .validate_seurat(obj, group_key, gene_symbols, coordinates_key)

  sorted_clusters <- sort(unique(obj[[group_key, drop = TRUE]]))
  cluster_map <- setNames(as.character(1:length(sorted_clusters)), sorted_clusters)

  .validate_marker_table(marker_table,sorted_clusters)

  if (aggregate_metadata){
    print("Aggregating metadata...")
    group_metadata <- .aggregate_metadata(obj, group_key, min_percentage = min_percentage)
    # Map cluster ids to use those natural numbers
    names(group_metadata) <- cluster_map[names(group_metadata)]

  } else{group_metadata <- list()
  }
  print(paste("Preparing marker genes with top",n_top_genes,"genes..."))

  marker_genes <- marker_table %>%
    group_by(cluster) %>%
    arrange(desc(avg_log2FC)) %>%
    slice_head(n = n_top_genes) %>%
    ungroup() %>%
    {split(.$gene, as.character(.$cluster))}
  names(marker_genes) <- cluster_map[names(marker_genes)]

  if (any(sapply(marker_genes, function(x) !is.vector(x) || length(x) < 5))) {
    stop("Invalid marker genes, some clusters have fewer than 5 markers")
  }

  coords <- NULL
  visualization_data <- NULL
  tryCatch({
    coords <- Seurat::Embeddings(obj, reduction = coordinates_key)
    print("Preparing visualisation data...")
    visualization_data <- .sample_visualization_data(obj, group_key, coordinates_key, cluster_map, max_cells_per_group)
  }, error = function(e) {
    log_warn(paste("Could not extract coordinates for reduction '", coordinates_key,
                   "'. Continuing without visualization data. Error:", conditionMessage(e)))
  })

  print("Calculating expression percentages...")
  expression_percentages <- .calculate_pcent(obj, group_key, cluster_map, pcent_batch_size)

  # Prep cluster_map to named list for each "group/cluster"
  cluster_map <- as.list(as.character(sorted_clusters))
  names(cluster_map) <- as.character(1:length(sorted_clusters))

  # build artefacts for upload
  build_succeeded <- FALSE

  tryCatch({
    log_info("Building vars.h5 from normalized counts (cells x features)...")
    default_assay <- .resolve_seurat_assay_rna(obj)
    # Seurat stores expression as features x cells; API expects cells x features (n_obs x n_vars).
    expr_mat <- tryCatch(
      Seurat::GetAssayData(obj, assay = default_assay, layer = "data"),
      error = function(e) Seurat::GetAssayData(obj, assay = default_assay, slot = "data")
    )
    mat <- Matrix::t(expr_mat)

    feature_df <- tryCatch(
      as.data.frame(Seurat::GetAssay(obj, default_assay)@meta.features),
      error = function(e) tryCatch(
        as.data.frame(Seurat::GetAssay(obj, default_assay)@meta.data),
        error = function(e2) NULL
      )
    )
    feature_names <- tryCatch(rownames(obj), error = function(e) NULL)
    raw_mat <- tryCatch(
      tryCatch(
        Seurat::GetAssayData(obj, assay = default_assay, layer = "counts"),
        error = function(e) Seurat::GetAssayData(obj, assay = default_assay, slot = "counts")
      ),
      error = function(e) NULL
    )
    .save_vars_h5(vars_h5_path, mat, raw_mat = raw_mat, feature_df = feature_df, feature_names = feature_names)
    log_info("Built vars.h5 successfully.")

    log_info("Building obs.duckdb (API) from cell metadata (Seurat obj@meta.data)...")

    .save_obs_duckdb(obs_duckdb_path, obj@meta.data,
                     coordinates = coords, coordinates_key = coordinates_key)
    log_info("Built obs.duckdb successfully.")

    build_succeeded <- TRUE
  }, error = function(e) {
    log_error("Error building artifacts: {conditionMessage(e)}")
  })

  prepped_data <- list(
    clusterLabels = cluster_map,
    clusterMetadata = group_metadata,
    markerGenes = marker_genes,
    visualizationData = visualization_data,
    expressionData = expression_percentages,
    group_key = group_key,
    build_succeeded = build_succeeded,
    vars_h5_path = vars_h5_path,
    obs_duckdb_path = obs_duckdb_path,
    coordinates_key = coordinates_key
  )
  # Store query
  obj@misc$query <- prepped_data

  print("Done!")
  return(prepped_data)
}
#'
#' Submit CyteType Analysis Job
#'
#' @description
#' Submits a prepared dataset to CyteType for automated cell type annotation
#' using large language models. Handles job submission, polling for results,
#' and integration of annotations back into the Seurat object.
#'
#' @param obj A Seurat object (will be returned with annotations added).
#' @param prepped_data Named list containing prepared data from `PrepareCyteTypeR()`.
#' @param study_context Optional character. Biological context for the experimental setup (e.g. organisms, tissues, diseases, developmental stages, single_cell methods, experimental conditions). Default is `NULL`.
#' @param llm_configs Optional list of LLM configs. Each element must match the LLMModelConfig schema with required `provider` and `name`; either `apiKey` or all AWS credentials (`awsAccessKeyId`, `awsSecretAccessKey`, `awsDefaultRegion`) must be provided. Default is `NULL` (API default model).
#' @param metadata Optional named list. Custom metadata tags for the report header; URL-like values are made clickable in the report. Default is `NULL`.
#' @param n_parallel_clusters Integer. Number of parallel requests to the model (max 50). High values can trigger rate limits. Default is 2.
#' @param results_prefix Character. Prefix for keys added to `obj@meta.data` and `obj@misc` storing results; the annotation column is `obj@meta.data[[paste(results_prefix, group_key, sep = "_")]]`. Default is `"cytetype"`.
#' @param poll_interval_seconds Integer. How often (seconds) to poll the API for results. Default from options (10).
#' @param timeout_seconds Integer. Maximum time (seconds) to wait for results before erroring. Default from options (7200).
#' @param api_url Optional character. CyteType API base URL. If `NULL`, uses the option/default URL. Default is `NULL`.
#' @param auth_token Optional character. Bearer token for API auth. If `NULL`, uses none. Default is `NULL`.
#' @param save_query Logical. Whether to save the request payload to a JSON file. Default is `TRUE`.
#' @param query_filename Character. Filename for the saved query when `save_query` is `TRUE`. Default is `"query.json"`.
#' @param upload_timeout_seconds Integer. Socket read timeout (seconds) for each artifact upload. Default is 3600.
#' @param require_artifacts Logical. If `TRUE`, an error during artifact build or upload stops the run; if `FALSE`, failures are skipped and annotation continues without artifacts. Default is `TRUE`.
#' @param show_progress Logical. Whether to show progress (spinner and cluster status). Set `FALSE` to disable. Default is `TRUE`.
#' @param override_existing_results Logical. If `TRUE`, allow overwriting existing results with the same `results_prefix`. If `FALSE` and results exist, an error is raised. Default is `FALSE`.
#'
#' @return The input Seurat object with added annotation metadata (and artifact uploads when artifacts are built and uploaded).
#'
#' @details
#' The function performs the following workflow:
#' 1. Constructs the analysis query from prepared data
#' 2. Saves query and job details to local JSON files
#' 3. Submits job to CyteType API
#' 4. Polls for job completion with progress updates
#' 5. Retrieves results and integrates annotations into Seurat object
#'
#' Job details are automatically saved to `job_details_{job_id}.json` for
#' later reference or manual result retrieval.
#'
#' @examples
#' \dontrun{
#' # Basic usage with prepared data
#' annotated_seurat <- CyteTypeR(
#'   obj = seurat_obj,
#'   prepped_data = prepped_data,
#'   study_context = "Single-cell analysis of human brain tissue"
#' )
#'
#' # With custom settings and metadata
#' annotated_seurat <- CyteTypeR(
#'   obj = seurat_obj,
#'   prepped_data = prepped_data,
#'   study_context = "Alzheimer's disease brain samples",
#'   metadata = list(
#'     "DOI" = "10.1038/example",
#'     "GEO_Accession" = "GSE123456"
#'   ),
#'   timeout_seconds = 1800,
#'   poll_interval_seconds = 60
#' )
#' }
#'
#' @seealso [PrepareCyteTypeR()] for data preparation, [GetResults()] for manual result retrieval
#' @importFrom jsonlite write_json
#' @export
CyteTypeR <- function(obj,
                      prepped_data,
                      study_context = NULL,
                      llm_configs = NULL,
                      metadata = NULL,
                      n_parallel_clusters = 2L,
                      results_prefix = "cytetype",
                      poll_interval_seconds = NULL,
                      timeout_seconds = NULL,
                      api_url = NULL,
                      auth_token = NULL,
                      save_query = TRUE,
                      query_filename = "query.json",
                      upload_timeout_seconds = 3600L,
                      require_artifacts = TRUE,
                      show_progress = TRUE,
                      override_existing_results = FALSE
 ){
  api_url <- api_url %||% .get_default_api_url()
  poll_interval_seconds <- poll_interval_seconds %||% .get_default_poll_interval()
  timeout_seconds <- timeout_seconds %||% .get_default_timeout()
  if (upload_timeout_seconds <= 0) {
    stop("upload_timeout_seconds must be greater than 0")
  }

  n_parallel_clusters <- as.integer(n_parallel_clusters)
  if (n_parallel_clusters < 1L || n_parallel_clusters > 50L) {
    stop("n_parallel_clusters must be between 1 and 50")
  }

  job_details_key <- paste0(results_prefix, "_jobDetails")
  if (!is.null(obj@misc) && !is.null(obj@misc[[job_details_key]]) && !override_existing_results) {
    existing_id <- obj@misc[[job_details_key]]$job_id
    stop(
      "Results with prefix '", results_prefix, "' already exist in this object (job_id: ", existing_id, "). ",
      "Use a different results_prefix, set override_existing_results = TRUE, or GetResults(obj, results_prefix = '", results_prefix, "') to retrieve stored results."
    )
  }

  prepped_data$studyInfo <- study_context %||% ""
  prepped_data$infoTags <- metadata %||% list()
  prepped_data$nParallelClusters <- n_parallel_clusters
  prepped_data$clientInfo <- list(
    clientType = "seurat",
    clientVersion = tryCatch(as.character(utils::packageVersion("CyteTypeR")), error = function(e) NULL)
  )

  group_key <- prepped_data$group_key
  coordinates_key <- prepped_data$coordinates_key %||% "umap"

  .validate_input_data(prepped_data)

  if (!is.null(llm_configs) && length(llm_configs) > 0L) {
    if (is.null(names(llm_configs)) && is.list(llm_configs[[1]])) {
      # Multiple configs - apply to each
      llm_configs <- lapply(llm_configs, function(config) {
        do.call(LLMModelConfig, config)
      })
    } else {
      # Single config - apply directly
      llm_configs <- list(do.call(LLMModelConfig, llm_configs))
    }
  }

  query_list <- list(
    input_data = prepped_data,
    llm_configs = llm_configs
  )

  build_succeeded <- isTRUE(prepped_data$build_succeeded)

  if (!build_succeeded && require_artifacts) {
    stop("Artifact build did not succeed. Set require_artifacts = FALSE to continue without artifacts.")
  }

  if (build_succeeded) {
    tryCatch({
      vars_h5_path <- prepped_data$vars_h5_path
      obs_duckdb_path <- prepped_data$obs_duckdb_path

      log_info("Uploading obs.duckdb (cell metadata)...")
      cell_metadata_upload <- .upload_obs_duckdb(api_url, auth_token, obs_duckdb_path, upload_timeout_seconds)
      log_info("Uploaded obs.duckdb successfully.")

      log_info("Uploading vars.h5 (feature expression)...")
      feature_expression_upload <- .upload_vars_h5(api_url, auth_token, vars_h5_path, upload_timeout_seconds)
      log_info("Uploaded vars.h5 successfully.")

      query_list$uploaded_files <- list(
        obs_duckdb = cell_metadata_upload$upload_id,
        vars_h5 = feature_expression_upload$upload_id
      )
    }, error = function(e) {
      if (require_artifacts) {
        log_error("Uploading artifacts failed: {conditionMessage(e)}")
        stop(e)
      } else {
        log_warn(paste(
          "Uploading artifacts failed. Continuing without artifacts.",
          "Set `require_artifacts=TRUE` to raise this as an error.",
          "Original error:", conditionMessage(e)
        ))
      }
    })
  }

  query_for_json <- .prepare_query_for_json(query_list)
  if (save_query) {
    write_json(query_for_json, path = query_filename, auto_unbox = TRUE, pretty = TRUE)
  }

  job_id <- .submit_job(query_for_json, api_url, auth_token)

  if (is.na(job_id)) {
    stop("Job submission failed.")
  }

  obj <- .store_job_details_seurat(obj, job_id, api_url, results_prefix, group_key, prepped_data$clusterLabels)

  # poll for results
  result <- .poll_for_results(
    job_id,
    api_url,
    poll_interval_seconds,
    timeout_seconds,
    auth_token = auth_token,
    show_progress = show_progress
  )
  if (!is.null(result)){
    result <- .normalize_result_annotations(result)
    obj <- .store_annotations_seurat(
      obj, result, job_id, results_prefix, group_key,
      prepped_data$clusterLabels
    )
    transformed_results <- .transform_results_seurat(result, cluster_map = prepped_data$clusterLabels)
    obj@misc$cytetype_results <- transformed_results

    ann_colname <- paste(results_prefix, group_key, sep = "_")
    onto_colname <- paste(results_prefix, "ontologyTerm", group_key, sep = "_")
    ontoID_colname <- paste(results_prefix, "ontologyID", group_key, sep = "_")
    cluster_ann_map <- setNames(transformed_results$annotation, transformed_results$clusterId)
    cluster_onto_map <- setNames(transformed_results$ontologyTerm, transformed_results$clusterId)
    cluster_onto_id_map <- setNames(transformed_results$ontologyTermID, transformed_results$clusterId)
    cells_group <- as.character(as.vector(obj@meta.data[[group_key]]))
    obj@meta.data[[ann_colname]] <- factor(cluster_ann_map[cells_group])
    obj@meta.data[[onto_colname]] <- factor(cluster_onto_map[cells_group])
    obj@meta.data[[ontoID_colname]] <- factor(cluster_onto_id_map[cells_group])

    return(obj)
  }

  return(obj)
}

#' Retrieve CyteType Analysis Results
#'
#' @description
#' If \code{obj} and \code{results_prefix} are given, tries to load from \code{obj@misc} first;
#' if not found but job details exist, fetches from the API and stores in \code{obj}.
#' If only \code{job_id} is given, fetches from the API and returns (saves JSON).
#'
#' @param obj Seurat object that may contain stored results (optional).
#' @param job_id Job ID from submission (optional if \code{obj} + \code{results_prefix} have stored job details).
#' @param results_prefix Prefix used when storing results. Default \code{"cytetype"}.
#' @param auth_token Optional bearer token for API fetch.
#' @return Result list (annotations, summary, etc.), or transformed results when fetching by \code{job_id} only.
#' @seealso [CyteTypeR()] for job submission
#' @importFrom jsonlite write_json
#' @export
GetResults <- function(obj = NULL, job_id = NULL, results_prefix = "cytetype", auth_token = NULL) {
  if (!is.null(obj) && !is.null(results_prefix)) {
    results_key <- paste0(results_prefix, "_results")
    job_details_key <- paste0(results_prefix, "_jobDetails")
    if (!is.null(obj@misc) && !is.null(obj@misc[[results_key]])) {
      return(obj@misc[[results_key]]$result)
    }
    if (!is.null(obj@misc) && !is.null(obj@misc[[job_details_key]])) {
      details <- obj@misc[[job_details_key]]
      job_id <- details$job_id
      api_url <- details$api_url
      token <- auth_token %||% details$auth_token
      status_resp <- .make_results_request(job_id, api_url, token)
      if (status_resp$status == "completed" && !is.null(status_resp$result)) {
        result <- .normalize_result_annotations(status_resp$result)
        if (!is.null(details$group_key) && !is.null(details$cluster_labels))
          obj <- .store_annotations_seurat(obj, result, job_id, results_prefix, details$group_key, details$cluster_labels)
        if (is.null(obj@misc)) obj@misc <- list()
        obj@misc[[results_key]] <- list(job_id = job_id, result = result)
        return(result)
      }
      if (status_resp$status == "failed") stop("Job ", job_id, " failed.")
      return(NULL)
    }
  }
  if (is.null(job_id)) {
    if (is.null(obj)) stop("Provide either obj (with stored job details) or job_id.")
    stop("No stored results or job details for prefix '", results_prefix, "'.")
  }
  api_url <- .get_default_api_url()
  response <- .api_response_helper(job_id, api_url, "results", auth_token)
  write_json(response$data,
             path = paste0("cytetypeR_results_", job_id, ".json"),
             auto_unbox = TRUE,
             pretty = TRUE)
  if (!is.null(response$data)) {
    transformed_results <- .transform_results_seurat(response$data)
    return(transformed_results)
  }
  return(response$data)
}


#' Clean Up Artifacts
#'
#' @description
#' Cleans up the artifact files after the run.
#'
#' @param prepped_data Named list containing prepared data from `PrepareCyteTypeR()`.
#' @importFrom logger log_info log_warn
#' @export
CleanUpArtifacts <- function(prepped_data) {
  if (isTRUE(prepped_data$build_succeeded)) {
    log_info("Cleaning up artifact files...")
    tryCatch(file.remove(prepped_data$vars_h5_path), error = function(e) NULL)
    log_info("Removed vars.h5 file.")
    tryCatch(file.remove(prepped_data$obs_duckdb_path), error = function(e) NULL)
    log_info("Removed obs.duckdb file.")
    log_info("Artifact files cleaned up successfully.")
  } else {
    log_warn("Artifact files not built. No cleanup performed.")
  }
}






















