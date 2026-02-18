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
#'   calculations. Default is 2000.
#' @param coordinates_key Character string specifying which dimensional reduction
#'   to use for visualization coordinates (e.g., "umap", "tsne"). Default is "umap".
#' @param max_cells_per_group Integer specifying maximum cells per cluster for
#'   subsampling (currently unused). Default is 1000.
#'
#' @return Named list containing formatted data for CyteType analysis:
#'   \itemize{
#'     \item `clusterLabels`: List mapping numeric cluster IDs to original labels
#'     \item `clusterMetadata`: Aggregated metadata by cluster (if enabled)
#'     \item `markerGenes`: List of top marker genes per cluster
#'     \item `visualizationData`: Coordinates and cluster assignments for plotting
#'     \item `expressionData`: Expression percentages by cluster
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
                             pcent_batch_size = 2000,
                             coordinates_key = "umap",
                             max_cells_per_group = 1000
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
    {split(.$gene, .$cluster)}
  names(marker_genes) <- cluster_map[names(marker_genes)]

  if (any(sapply(marker_genes, function(x) !is.vector(x) || length(x) < 5))) {
    stop("Invalid marker genes, some clusters have fewer than 5 markers")
  }

  print("Preparing visualisation data...")
  visualization_data <- .sample_visualization_data(
    obj, group_key, coordinates_key, cluster_map, max_cells_per_group
  )

  print("Calculating expression percentages...")
  expression_percentages <- .calculate_pcent(obj, group_key, cluster_map, pcent_batch_size)

  # Prep cluster_map to named list for each "group/cluster"
  cluster_map <- as.list(as.character(sorted_clusters))
  names(cluster_map) <- as.character(1:length(sorted_clusters))

  prepped_data <- list(
    clusterLabels = cluster_map,
    clusterMetadata = group_metadata,
    markerGenes = marker_genes,
    visualizationData = visualization_data,
    expressionData = expression_percentages,
    group_key = group_key
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
#' @param study_context Optional character string providing background information
#'   about the study for better annotation context. Default is `NULL`.
#' @param llm_configs Optional list of LLM configuration objects specifying
#'   which models to use. If `NULL`, uses default models. Default is `NULL`.
#' @param metadata Optional named list containing additional study metadata
#'   (e.g., DOI, GEO accession). Default is `NULL`.
#' @param results_prefix Character string prefix for result filenames.
#'   Default is "cytetype".
#' @param poll_interval_seconds Integer specifying how often to check job status
#'   (in seconds). Default is 30.
#' @param timeout_seconds Integer specifying maximum wait time for job completion
#'   (in seconds). Default is 1200 (20 minutes).
#' @param n_parallel_clusters Number of parallel requests to make to the model. Maximum is 50. Note than high values can lead to rate limit errors.
#' @param override_existing_results Logical. If `TRUE`, re-runs annotation even if
#'   results for this job already exist locally. Default is `FALSE`.
#' @param upload_artifacts Logical. If `TRUE`, build API artifacts from the Seurat object: feature expression
#'   to vars.h5 (API naming) and cell metadata to obs.duckdb (API naming), then upload. Requires rhdf5 (Bioc) and duckdb. Default is FALSE.
#' @param vars_h5_path Path for the vars.h5 artifact. Used when upload_artifacts = TRUE. Default "vars.h5".
#' @param obs_duckdb_path Path for the obs.duckdb artifact. Used when upload_artifacts = TRUE. Default "obs.duckdb".
#' @param upload_timeout_seconds Timeout in seconds for each artifact upload. Default is 3600.
#' @param cleanup_artifacts If TRUE, delete the artifact files at vars_h5_path and obs_duckdb_path
#'   after the run (success or failure). Default is FALSE.
#' @param api_url Optional character string specifying custom API URL.
#'   If `NULL`, uses default URL. Default is `NULL`.
#' @param save_query Logical indicating whether to save the query as JSON file.
#'   Default is `TRUE`.
#' @param query_filename Character string specifying filename for saved query.
#'   Default is "query.json".
#' @param auth_token Optional authentication token for API access.
#'   Default is `NULL`.
#' @param show_progress Logical indicating whether to display progress updates
#'   during job execution. Default is `TRUE`.
#'
#' @return The input Seurat object with added annotation metadata and (if upload_artifacts)
#'   with artifact uploads matching the Python CyteType workflow.
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
                      override_existing_results = FALSE,
                      upload_artifacts = FALSE,
                      vars_h5_path = "vars.h5",
                      obs_duckdb_path = "obs.duckdb",
                      upload_timeout_seconds = 3600L,
                      cleanup_artifacts = FALSE,
                      poll_interval_seconds = NULL,
                      timeout_seconds = NULL,
                      api_url = NULL,
                      save_query = TRUE,
                      query_filename = "query.json",
                      auth_token = NULL,
                      show_progress = TRUE
){
  api_url <- api_url %||% .get_default_api_url()
  poll_interval_seconds <- poll_interval_seconds %||% .get_default_poll_interval()
  timeout_seconds <- timeout_seconds %||% .get_default_timeout()
  if (upload_artifacts && upload_timeout_seconds <= 0) {
    stop("upload_timeout_seconds must be greater than 0 when upload_artifacts = TRUE")
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
  prepped_data$group_key <- NULL

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

  artifact_paths <- character(0)
  if (upload_artifacts) {
    log_info("Building vars.h5 from normalized counts (cells x features)...")
    # GetAssayData returns genes x cells; API expects cells x genes (n_obs x n_vars).
    mat <- Matrix::t(Seurat::GetAssayData(obj, layer = "data"))

    default_assay <- Seurat::DefaultAssay(obj)

    feature_df <- tryCatch(
      as.data.frame(Seurat::GetAssay(obj, default_assay)@meta.features),
      error = function(e) NULL
    )
    feature_names <- tryCatch(rownames(obj), error = function(e) NULL)

    .save_vars_h5(vars_h5_path, mat, feature_df = feature_df, feature_names = feature_names)

    artifact_paths <- c(artifact_paths, vars_h5_path)
    log_info("Building obs.duckdb (API) from cell metadata (Seurat obj@meta.data)...")

    .save_obs_duckdb(obs_duckdb_path, obj@meta.data)
    artifact_paths <- c(artifact_paths, obs_duckdb_path)

    log_info("Uploading obs.duckdb (cell metadata)...")
    cell_metadata_upload <- .upload_obs_duckdb(api_url, auth_token, obs_duckdb_path, upload_timeout_seconds)

    log_info("Uploading vars.h5 (feature expression)...")
    feature_expression_upload <- .upload_vars_h5(api_url, auth_token, vars_h5_path, upload_timeout_seconds)

    query_list$uploaded_files <- list(
      obs_duckdb = cell_metadata_upload$upload_id,
      vars_h5 = feature_expression_upload$upload_id
    )
  }

  query_for_json <- .prepare_query_for_json(query_list)

  if (upload_artifacts && cleanup_artifacts && length(artifact_paths) > 0) {
    on.exit({
      for (f in artifact_paths) tryCatch(file.remove(f), error = function(e) NULL)
    }, add = TRUE)
  }

  job_id <- .submit_job(query_for_json, api_url, auth_token)
  if (is.na(job_id)) {
    stop("Job submission failed.")
  }

  report_url <- file.path(api_url, 'report', job_id)
  job_details <- list(
    job_id = job_id,
    report_url = report_url,
    api_url = api_url,
    auth_token = auth_token
  )
  write_json(job_details, path = paste0('job_details_', job_id, '.json'), auto_unbox = TRUE, pretty = TRUE)

  obj <- .store_job_details_seurat(obj, job_id, api_url, results_prefix, group_key, prepped_data$clusterLabels)

  if (save_query){
    write_json(query_for_json, path = query_filename, auto_unbox = TRUE, pretty = TRUE)
  }

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




























