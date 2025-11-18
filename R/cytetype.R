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
#' @importFrom dplyr group_by filter slice_head ungroup %>%
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
    group_metadata <- .aggregate_metadata(obj,group_key)
    # Map cluster ids to use those natural numbers
    names(group_metadata) <- cluster_map[names(group_metadata)]

  } else{group_metadata <- list()
  }
  print(paste("Preparing marker genes with top",n_top_genes,"genes..."))

  marker_genes <- marker_table %>%
    group_by(cluster) %>%
    dplyr::filter(avg_log2FC > 1) %>%
    slice_head(n = n_top_genes) %>%
    ungroup() %>%
    {split(.$gene, .$cluster)}
  names(marker_genes) <- cluster_map[names(marker_genes)]

  print("Preparing visualisation data...")
  visualization_data <- list(
    coordinates = Embeddings(obj, reduction = coordinates_key),
    clusters = cluster_map[obj[[group_key]][[group_key]]]
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
#' @return The input Seurat object with added `cytetype_annotations` metadata
#'   containing the automated cell type predictions.
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
                      results_prefix = "cytetype",
                      poll_interval_seconds = 30,
                      timeout_seconds = 1200,
                      api_url = NULL,
                      save_query = TRUE,
                      query_filename = "query.json",
                      auth_token = NULL,
                      show_progress = TRUE
){
  api_url <- api_url %||% .get_default_api_url()
  prepped_data$studyInfo <- study_context %||% ""
  prepped_data$infoTags <- metadata %||% list()

  group_key <- prepped_data$group_key
  prepped_data$group_key <- NULL

  .validate_input_data(prepped_data)

  if (!is.null(llm_configs)){
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

  if (save_query){
    write_json(query_list, path = query_filename, auto_unbox = TRUE, pretty = TRUE)
  }
  ## NA value check on all data before submitting job


  # Job submission

  job_id <- .submit_job(query_list, api_url, auth_token)
  if (is.na(job_id)) {
    stop("Job submission failed.")
  }


  # Save job details
  report_url <- file.path(api_url, 'report',job_id)

  job_details <- list(
    job_id = job_id,
    report_url = report_url,
    api_url = api_url,
    auth_token = auth_token
  )

  if (!is.null(job_details)){
    write_json(job_details, path = paste0('job_details_', job_id, '.json'), auto_unbox = TRUE, pretty = TRUE)

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
  # store results
  if (!is.null(result)){

    transformed_results <- .transform_results_seurat(result,cluster_map = prepped_data$clusterLabels)

    obj@misc$cytetype_results <- transformed_results

    ann_colname <- paste(results_prefix, group_key, sep = "_" )
    onto_colname <- paste("cytetype_cell_ontology", group_key, sep = "_")

    cluster_ann_map <- setNames(transformed_results$annotation,
                                transformed_results$clusterId)
    cluster_onto_map <- setNames(transformed_results$ontologyTerm,
                                transformed_results$clusterId)

    obj@meta.data[[ann_colname]] <- factor(cluster_ann_map[as.vector(obj@meta.data[[group_key]])])
    obj@meta.data[[onto_colname]] <- factor(cluster_onto_map[as.vector(obj@meta.data[[group_key]])])


    return(obj)
  }
}

#' Retrieve CyteType Analysis Results
#'
#' @description
#' Retrieves and saves results from a completed CyteType analysis job using
#' the job ID. Results are saved as a JSON file for further analysis.
#' @param job_id Character string specifying the CyteType job ID. If `NULL`,
#'   attempts to read from saved job details file (currently WIP).
#'   Default is `NULL`.
#'
#' @return List containing the analysis results data structure with cell type
#'   annotations and associated metadata.
#'
#' @details
#' The function:
#' 1. Makes an API request to retrieve results for the specified job ID
#' 2. Saves results to `cytetypeR_results_{job_id}.json`
#' 3. Returns the parsed results data
#'
#' This function is useful for retrieving results from jobs that were submitted
#' previously or from different R sessions.
#'
#' @examples
#' \dontrun{
#' # Retrieve results by job ID
#' results <- GetResults(job_id = "abc123-def456-ghi789")
#'
#' # Results are automatically saved to:
#' # cytetypeR_results_abc123-def456-ghi789.json
#' }
#'
#' @seealso [CyteTypeR()] for job submission
#' @importFrom jsonlite write_json
#' @export
GetResults <- function(job_id = NULL){
  tryCatch({
    # if (is.null(job_id)){
    #   job_details <- fromJSON(paste0('job_details_', job_id, '.json'))
    #   job_id <- job_details$job_id
    # }

    api_url <- .get_default_api_url()
    response <- .api_response_helper(job_id, api_url, 'results')

    write_json(response$data,
               path = paste0("cytetypeR_results_", job_id, ".json"),
               auto_unbox = TRUE,
               pretty = TRUE)

    if (!is.null(response$data)){

      transformed_results <- .transform_results_seurat(response$data)

      print("Results table retrieved")
    }
      return(transformed_results)

  }, error = function(e) {
    stop(paste("Error retrieving results for job", {job_id}, ": ", e$message))
  })

}




























