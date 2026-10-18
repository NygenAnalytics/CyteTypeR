#' Prepare Seurat Object for CyteType Analysis
#'
#' @description
#' Prepares a Seurat object and a gene marker table for CyteType API by extracting and formatting
#' cluster labels, metadata, marker genes, visualization coordinates, and
#' expression data into the required data structure. It also writes the two
#' files that [CyteTypeR()] uploads with the job: `vars.h5` (expression) and
#' `obs.duckdb` (cell metadata and coordinates).
#'
#' @param obj A Seurat object with normalized data and clustering results. It
#'   needs an assay named `"RNA"` with a normalized `data` layer.
#' @param marker_table Data frame containing marker genes with the columns
#'   `cluster`, `gene` and `avg_log2FC`, such as the output of Seurat's
#'   `FindAllMarkers()`. Its clusters must be exactly the clusters in
#'   `group_key`, each with at least five marker genes.
#' @param group_key Character string specifying the metadata column containing
#'   cluster assignments. Every cell needs a cluster: `NA` values stop the
#'   preparation with an error. Default is `"seurat_clusters"`.
#' @param gene_symbols Character string naming the column of the RNA assay's
#'   feature metadata that holds gene symbols. It is used only to check that
#'   the genes are symbols rather than database identifiers; when the column
#'   does not exist, the row names of the RNA assay are checked instead.
#'   Default is `"gene_symbols"`.
#' @param n_top_genes Integer specifying the maximum number of top marker genes
#'   per cluster to include after ordering by `avg_log2FC`. Filter
#'   `marker_table` before calling this function when a fold-change threshold is
#'   required. Default is 50.
#' @param aggregate_metadata Logical indicating whether to aggregate metadata
#'   across cells within each cluster. Default is `TRUE`.
#' @param min_percentage Numeric threshold for minimum percentage.
#'   Default is 10.
#' @param max_metadata_categories Integer. Maximum number of unique values a
#'   categorical metadata column may have to be included in aggregation. Columns
#'   exceeding this limit are silently skipped. Default is 500.
#' @param pcent_batch_size Integer specifying batch size for expression percentage
#'   calculations. Default is 5000.
#' @param coordinates_key Character string naming the dimensional reduction used
#'   for the report's visualization, such as `"umap"` or `"tsne"`. This
#'   reduction is tried first, then `"umap"`, `"tsne"` and `"pca"`, matched
#'   case-insensitively; the first one with a row for every cell and at least
#'   two dimensions is used. When none qualifies, the job runs without
#'   visualization. Default is `"umap"`.
#' @param max_cells_per_group Integer specifying maximum cells per cluster for
#'   visualization subsampling. Default is 1000.
#' @param vars_h5_path Character string specifying the local file path for the
#'   generated vars.h5 artifact (feature expression). Default is `"vars.h5"`.
#' @param obs_duckdb_path Character string specifying the local file path for the
#'   generated obs.duckdb artifact (cell metadata). Default is `"obs.duckdb"`.
#'
#' @return A named list to pass to [CyteTypeR()]:
#'   \itemize{
#'     \item `clusterLabels`: List mapping the CyteType cluster IDs (`"1"`,
#'       `"2"`, ...) to your cluster labels
#'     \item `clusterMetadata`: Aggregated metadata by cluster (if enabled)
#'     \item `markerGenes`: List of top marker genes per cluster
#'     \item `visualizationData`: Sampled coordinates and cluster assignments
#'       for plotting, or `NULL` when no coordinates were found
#'     \item `expressionData`: Expression percentages by cluster
#'     \item `group_key`: The metadata column name used for cluster assignments
#'     \item `build_succeeded`: Logical indicating whether artifact files were built successfully
#'     \item `vars_h5_path`: Path to the generated vars.h5 file
#'     \item `obs_duckdb_path`: Path to the generated obs.duckdb file
#'     \item `coordinates_key`: The name of the reduction whose coordinates
#'       were used for the visualization, or `NULL` when none was used
#'   }
#'
#' @details
#' The function performs the following steps:
#' 1. Validates the Seurat object structure and the cluster labels
#' 2. Creates a mapping from original cluster labels to sequential numeric IDs
#' 3. Aggregates metadata across cells within clusters (optional)
#' 4. Filters and selects top marker genes per cluster
#' 5. Extracts dimensional reduction coordinates for visualization
#' 6. Calculates expression percentages across clusters
#' 7. Builds a vars.h5 artifact from the normalized expression matrix, with
#'    the raw counts when they are whole numbers
#' 8. Builds an obs.duckdb artifact from the cell metadata
#'
#' Clusters are renumbered sequentially (1, 2, 3, ...) in the sorted order of
#' your labels (level order for a factor) to ensure consistent formatting
#' regardless of original cluster naming.
#'
#' Expression always comes from the `"RNA"` assay, whatever the default assay
#' is, and Seurat v5 layers split by sample are joined on a copy (your object is
#' not changed). The gene names come from the same assay. When the default
#' assay is not `"RNA"`, a warning says that `"RNA"` is used.
#'
#' Errors while building the two files are logged and recorded as
#' `build_succeeded = FALSE`; [CyteTypeR()] then stops unless
#' `require_artifacts = FALSE`.
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
#' @seealso [CyteTypeR()] to submit the job, [CleanUpArtifacts()] to delete the
#'   files afterwards.
#' @importFrom logger log_info log_debug log_error log_warn
#' @importFrom dplyr group_by filter slice_head ungroup arrange desc %>%
#' @importFrom stats setNames
#' @importFrom jsonlite write_json fromJSON
#' @export
PrepareCyteTypeR <- function(obj,
                             marker_table,
                             group_key = "seurat_clusters",
                             gene_symbols = 'gene_symbols',
                             n_top_genes = 50,
                             aggregate_metadata = TRUE,
                             min_percentage = 10,
                             max_metadata_categories = 500L,
                             pcent_batch_size = 5000,
                             coordinates_key = "umap",
                             max_cells_per_group = 1000,
                             vars_h5_path = "vars.h5",
                             obs_duckdb_path = "obs.duckdb"
){
  .validate_seurat(obj, group_key, gene_symbols, coordinates_key)
  # duckdb writes obs.duckdb; check it before the (slow) vars.h5 write.
  .require_duckdb()

  sorted_clusters <- sort(unique(obj[[group_key, drop = TRUE]]))
  cluster_map <- setNames(as.character(seq_along(sorted_clusters)), as.character(sorted_clusters))

  .validate_marker_table(marker_table,sorted_clusters)

  if (aggregate_metadata){
    log_info("Aggregating metadata...")
    group_metadata <- .aggregate_metadata(obj, group_key, min_percentage = min_percentage,
                                            max_metadata_categories = max_metadata_categories)
    # Map cluster ids to use those natural numbers
    names(group_metadata) <- cluster_map[names(group_metadata)]

  } else{group_metadata <- list()
  }
  log_info("Preparing marker genes with top {n_top_genes} genes...")

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
  # The reduction actually used; stays NULL when there is none.
  used_coordinates_key <- NULL
  found_coordinates_key <- .resolve_coordinates_key(obj, coordinates_key)
  if (!is.null(found_coordinates_key)) {
    visualization <- tryCatch({
      log_info("Preparing visualization data...")
      list(
        coords = .reduction_coordinates(obj, found_coordinates_key),
        data = .sample_visualization_data(obj, group_key, found_coordinates_key, cluster_map, max_cells_per_group)
      )
    }, error = function(e) {
      log_warn(paste0("Could not extract coordinates for reduction '", found_coordinates_key,
                      "'. Continuing without visualization data. Error: ", conditionMessage(e)))
      NULL
    })
    if (!is.null(visualization)) {
      coords <- visualization$coords
      visualization_data <- visualization$data
      used_coordinates_key <- found_coordinates_key
    }
  }

  log_info("Calculating expression percentages...")
  expr <- .get_rna_layer(obj, "data")
  if (is.null(expr)) {
    stop("The RNA assay has no normalized 'data' layer. Run NormalizeData() first.", call. = FALSE)
  }
  expression_percentages <- .calculate_pcent(obj, group_key, cluster_map, pcent_batch_size, expr = expr)

  # Prep cluster_map to named list for each "group/cluster"
  cluster_map <- as.list(as.character(sorted_clusters))
  names(cluster_map) <- as.character(seq_along(sorted_clusters))

  # build artefacts for upload
  build_succeeded <- FALSE

  tryCatch({
    log_info("Building vars.h5 from normalized counts (cells x features)...")
    # Seurat stores expression as features x cells; API expects cells x features (n_obs x n_vars).
    mat <- Matrix::t(expr)
    feature_names <- rownames(expr)
    feature_df <- .align_feature_metadata(.assay_feature_metadata(obj, "RNA"), feature_names)
    raw_mat <- tryCatch(
      .align_raw_counts(.get_rna_layer(obj, "counts"), feature_names),
      error = function(e) {
        log_warn("Could not read the RNA counts layer, skipping raw counts: {conditionMessage(e)}")
        NULL
      }
    )
    .save_vars_h5(vars_h5_path, mat, raw_mat = raw_mat, feature_df = feature_df, feature_names = feature_names)
    log_info("Built vars.h5 successfully.")

    log_info("Building obs.duckdb (API) from cell metadata (Seurat obj@meta.data)...")

    .save_obs_duckdb(obs_duckdb_path, obj@meta.data,
                     coordinates = coords, coordinates_key = used_coordinates_key)
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
    coordinates_key = used_coordinates_key
  )

  log_info("Preparation complete.")
  return(prepped_data)
}

# Feature metadata in the order of `features`, or NULL when it cannot be aligned.
.align_feature_metadata <- function(feature_df, features) {
  if (is.null(feature_df)) {
    return(NULL)
  }
  if (identical(rownames(feature_df), features)) {
    return(feature_df)
  }
  if (!is.null(rownames(feature_df)) && all(features %in% rownames(feature_df))) {
    return(feature_df[features, , drop = FALSE])
  }
  log_warn("The RNA feature metadata does not match the genes of the data layer; writing vars.h5 without it.")
  NULL
}

# Raw counts with the genes of the data layer in the same order, or NULL.
.align_raw_counts <- function(raw_mat, features) {
  if (is.null(raw_mat)) {
    return(NULL)
  }
  if (identical(rownames(raw_mat), features)) {
    return(raw_mat)
  }
  if (!is.null(rownames(raw_mat)) && all(features %in% rownames(raw_mat))) {
    return(raw_mat[features, , drop = FALSE])
  }
  log_warn("The RNA counts layer does not contain the genes of the data layer; skipping raw counts in vars.h5.")
  NULL
}

.has_namespace <- function(package) {
  requireNamespace(package, quietly = TRUE)
}

.require_duckdb <- function() {
  if (!.has_namespace("duckdb")) {
    stop(
      "Package 'duckdb' is required to build obs.duckdb. ",
      "Install it with install.packages(\"duckdb\").",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.JOB_VISIBILITY <- c("private", "org", "public")

.validate_visibility <- function(visibility) {
  if (is.null(visibility)) {
    return(invisible(NULL))
  }
  if (!.is_string(visibility) || !visibility %in% .JOB_VISIBILITY) {
    stop(
      "visibility must be \"private\", \"org\", \"public\" or NULL",
      call. = FALSE
    )
  }
  invisible(visibility)
}

# The server gives API keys of users without an organization license the
# prefix "cyt_p_" (organization keys get "cyt_") and refuses "org" visibility
# for them when the job is submitted, after the upload. The prefix only
# suggests a personal key: an organization key's random part can begin with
# "p_". So warn before uploading, and leave the decision to the server.
.PERSONAL_API_KEY_PREFIX <- "cyt_p_"

.warn_org_visibility_key <- function(visibility, auth_token) {
  if (
    identical(visibility, "org") &&
      .is_string(auth_token) &&
      startsWith(auth_token, .PERSONAL_API_KEY_PREFIX)
  ) {
    # Logged at once, before the upload starts (a warning() would only be
    # shown when CyteTypeR() returns). Name the kind of key, never the key.
    log_warn(paste0(
      "visibility = \"org\" needs an organization API key, and this key looks ",
      "like a personal key (cyt_p_...). If it is, the server rejects the job ",
      "after the upload. Use an organization key, or choose \"private\" or ",
      "\"public\"."
    ))
  }
  invisible(NULL)
}

.results_hint <- function(job_id) {
  paste0("GetResults(job_id = \"", job_id, "\")")
}

# Warn about clusters that have no result; their cells are marked "Unknown".
.log_unannotated_clusters <- function(result, cluster_labels) {
  annotated <- vapply(
    .normalized_entries(result),
    function(entry) .scalar_text(.json_get(entry, "clusterId"), ""),
    character(1)
  )
  missing <- setdiff(names(cluster_labels), annotated)
  if (length(missing) > 0L) {
    labels <- .map_cluster_labels(missing, cluster_labels)
    log_warn(paste0(
      "No annotations received for clusters: ", paste(labels, collapse = ", "),
      ". Their cells are marked as 'Unknown'."
    ))
  }
  invisible(missing)
}

.log_result_locations <- function(results_prefix, group_key) {
  column <- function(field) paste0("obj@meta.data$", results_prefix, "_", field, "_", group_key)
  results_location <- if (identical(results_prefix, "cytetype")) {
    "obj@misc$cytetype_results"
  } else {
    paste0("obj@misc$", results_prefix, "_results")
  }
  log_info(paste0(
    "Annotation labels: ", column("annotation"), "\n",
    "Cell Ontology terms: ", column("cellOntologyTerm"), "\n",
    "Cell Ontology term IDs: ", column("cellOntologyTermID"), "\n",
    "Cell states: ", column("cellState"), "\n",
    "Reviewer confidence: ", column("reviewerConfidence"), "\n",
    "Full results: ", results_location
  ))
}

#' Submit CyteType Analysis Job
#'
#' @description
#' Submits a prepared dataset to CyteType for automated cell type annotation
#' using large language models. Handles job submission, polling for results,
#' and integration of annotations back into the Seurat object.
#'
#' @param obj A Seurat object (will be returned with annotations added).
#' @param prepped_data Named list containing prepared data from `PrepareCyteTypeR()`.
#' @param study_context Optional character. Biological context for the experimental setup (e.g. organisms, tissues, diseases, developmental stages, single-cell methods, experimental conditions). Default is `NULL`.
#' @param llm_configs Optional list of LLM configs. Each element must match the LLMModelConfig schema with required `provider` and `name`; either `apiKey` or all AWS credentials (`awsAccessKeyId`, `awsSecretAccessKey`, `awsDefaultRegion`) must be provided. `provider` is one of `"anthropic"`, `"bedrock"`, `"deepseek"`, `"fireworks"`, `"google"`, `"groq"`, `"huggingface"`, `"mistral"`, `"nebius"`, `"openai"`, `"openrouter"`, `"together"`, `"vertex"` or `"xai"`. Default is `NULL` (API default model).
#' @param metadata Optional named list. Custom metadata tags for the report header; URL-like values are made clickable in the report. Default is `NULL`.
#' @param n_parallel_clusters Integer. Number of parallel requests to the model (max 50). High values can trigger rate limits. Default is 2.
#' @param results_prefix Character. Prefix for the results added to `obj`. The
#'   factor columns `<prefix>_annotation_<group_key>`,
#'   `<prefix>_cellOntologyTerm_<group_key>`,
#'   `<prefix>_cellOntologyTermID_<group_key>`, `<prefix>_cellState_<group_key>`
#'   and `<prefix>_reviewerConfidence_<group_key>` are added to `obj@meta.data`,
#'   with three copies under older names: `<prefix>_<group_key>` (label),
#'   `<prefix>_ontologyTerm_<group_key>` and `<prefix>_ontologyID_<group_key>`.
#'   The job details are stored in `obj@misc[[paste0(prefix, "_jobDetails")]]`
#'   and the results as described under Value. Default is `"cytetype"`.
#' @param poll_interval_seconds Integer. How often (seconds) to poll the API for results. Default from options (10).
#' @param timeout_seconds Integer. Maximum time (seconds) to wait for results.
#'   When it is reached, `CyteTypeR()` stops with an error of class
#'   `cytetype_timeout_error` that names the job ID; the job keeps running on
#'   the server and [GetResults()] retrieves its results later. Default from
#'   options (7200).
#' @param api_url Optional character. CyteType API server origin. If `NULL`,
#'   uses `CYTETYPE_API_URL`, the configured R option, or
#'   `https://cytetype.nygen.io`.
#' @param auth_token Optional character. Bearer token for API authentication.
#'   If `NULL`, uses credentials saved by [SetupCyteTypeR()] or
#'   [LoginCyteTypeR()].
#' @param save_query Logical. Whether to save the request payload to a JSON
#'   file. Default is `TRUE`. Set this to `FALSE` when `llm_configs` contains
#'   provider credentials because the saved payload includes the complete LLM
#'   configuration.
#' @param query_filename Character. Filename for the saved query when `save_query` is `TRUE`. Default is `"query.json"`.
#' @param upload_timeout_seconds Integer. Socket read timeout (seconds) for each artifact upload. Default is 3600.
#' @param upload_max_workers Integer. Maximum number of parallel upload workers. Default is 6.
#' @param require_artifacts Logical. If `TRUE`, an error during artifact build or upload stops the run; if `FALSE`, failures are skipped and annotation continues without artifacts. Default is `TRUE`.
#' @param show_progress Logical. Whether to show progress (spinner and cluster status). Set `FALSE` to disable. Default is `TRUE`.
#' @param override_existing_results Logical. If `TRUE`, allow overwriting existing results with the same `results_prefix`. If `FALSE` and results exist, an error is raised. Default is `FALSE`.
#' @param visibility Optional character. Who can view the report: `"private"`,
#'   `"org"` (your organization; needs an organization API key) or `"public"`
#'   (anyone with the link). Default is `NULL`, which uses the server default
#'   (`"private"`). With `"org"` and an API key that looks like a personal key
#'   (it starts with `cyt_p_`), a warning is logged before the files are
#'   uploaded and the run continues; the server checks the key when the job is
#'   submitted and rejects personal keys.
#'
#' @return The Seurat object `obj` with the results added. Assign it, for
#'   example `obj <- CyteTypeR(obj, prepped_data)`; the object you pass in is
#'   not changed.
#'   \itemize{
#'     \item `obj@meta.data` gains the factor columns listed under
#'       `results_prefix`. Cells in clusters without a result get `"Unknown"`
#'       (`""` for the cell state) in the first five columns and `NA` in the
#'       three older ones. The reviewer confidence column holds `"High"`,
#'       `"Moderate"`, `"Low"`, or `"Unknown"` when it is missing; its levels
#'       follow that order, and only the levels that occur are kept.
#'     \item `obj@misc[[paste0(prefix, "_jobDetails")]]` holds `job_id`,
#'       `report_url`, `api_url`, `group_key`, `cluster_labels` and, when set,
#'       `visibility`.
#'     \item With the default prefix, `obj@misc$cytetype_results` is a
#'       data.frame with one row per annotated cluster and the columns
#'       `clusterId` (your cluster label), `annotation`, `ontologyTerm`,
#'       `ontologyTermID`, `granularAnnotation`, `cellState`, `justification`,
#'       `supportingMarkers`, `conflictingMarkers`, `missingExpression`,
#'       `unexpectedExpression`, `reviewerConfidence`, `isHeterogeneous` and
#'       `heterogeneityType`. Its attributes `"job_id"` and `"result"` hold the
#'       job ID and the full results in the form returned by [GetResults()].
#'     \item With another prefix, `obj@misc[[paste0(prefix, "_results")]]` is a
#'       list with `job_id` and `result` (the full results), and
#'       `obj@misc$cytetype_results` is left as it is.
#'   }
#'
#' @details
#' The function performs the following workflow:
#' 1. Constructs the analysis query from prepared data
#' 2. Optionally saves the query to a JSON file
#' 3. Uploads the artifact files and submits the job
#' 4. Polls for job completion with progress updates
#' 5. Retrieves results and integrates annotations into the Seurat object
#'
#' The evidence columns of the results table (`justification`,
#' `supportingMarkers`, `conflictingMarkers`, `missingExpression` and
#' `unexpectedExpression`) are filled from the structured output of current
#' jobs when the stored fields are empty. `supportingMarkers` and
#' `conflictingMarkers` list genes separated by `"; "`.
#'
#' While waiting, network errors are retried until `timeout_seconds` is
#' reached. Other errors while checking the job stop the run after five in a
#' row. When the results of a completed job cannot be fetched, the rate limit
#' and server errors (HTTP 500, 502, 503 and 504) are retried in the same way,
#' and other errors, such as a deleted report (HTTP 410), stop the run at once.
#' These errors signal a [cytetype_api_error()] condition. If the run stops or
#' is interrupted, the job keeps running on the server: retrieve its results
#' with `GetResults(job_id = "<job_id>")`, using the job ID printed when the job
#' was submitted.
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
#'   poll_interval_seconds = 60,
#'   visibility = "public"
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
                      upload_max_workers = 6L,
                      require_artifacts = TRUE,
                      show_progress = TRUE,
                      override_existing_results = FALSE,
                      visibility = NULL
 ){
  api_url <- .resolve_api_url(api_url)
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

  .validate_visibility(visibility)

  prepped_data$studyInfo <- study_context %||% ""
  prepped_data$infoTags <- metadata %||% list()
  prepped_data$nParallelClusters <- n_parallel_clusters
  prepped_data$clientInfo <- list(
    clientType = "seurat",
    clientVersion = tryCatch(as.character(utils::packageVersion("CyteTypeR")), error = function(e) NULL)
  )

  group_key <- prepped_data$group_key

  .validate_input_data(prepped_data)
  auth_token <- .resolve_auth_token(api_url, auth_token)
  .warn_org_visibility_key(visibility, auth_token)

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
  if (!is.null(visibility)) {
    query_list$visibility <- visibility
  }

  build_succeeded <- isTRUE(prepped_data$build_succeeded)

  if (!build_succeeded && require_artifacts) {
    stop("Artifact build did not succeed. Set require_artifacts = FALSE to continue without artifacts.")
  }

  if (build_succeeded) {
    tryCatch({
      vars_h5_path <- prepped_data$vars_h5_path
      obs_duckdb_path <- prepped_data$obs_duckdb_path

      log_info("Uploading obs.duckdb (cell metadata)...")
      cell_metadata_upload <- .upload_obs_duckdb(api_url, auth_token, obs_duckdb_path,
                                                  upload_timeout_seconds, max_workers = upload_max_workers, show_progress = show_progress)
      log_info("Uploaded obs.duckdb successfully.")

      log_info("Uploading vars.h5 (feature expression)...")
      feature_expression_upload <- .upload_vars_h5(api_url, auth_token, vars_h5_path,
                                                    upload_timeout_seconds, max_workers = upload_max_workers, show_progress = show_progress)
      log_info("Uploaded vars.h5 successfully.")

      query_list$uploaded_files <- list(
        obs_duckdb = cell_metadata_upload$upload_id,
        vars_h5 = feature_expression_upload$upload_id
      )
    }, error = function(e) {
      if (require_artifacts) {
        log_error(paste("Uploading artifacts failed:", conditionMessage(e),
        "Set `require_artifacts=FALSE` to continue without uploading artifacts."))
        stop(e)
      } else {
        log_warn(paste(
          "Uploading artifacts failed. Continuing without artifacts.",
          "Set `require_artifacts=TRUE` to stop on upload failures.",
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

  if (is.null(job_id) || is.na(job_id)) {
    stop("Job submission failed: no job ID returned.", call. = FALSE)
  }

  obj <- .store_job_details_seurat(
    obj, job_id, api_url, results_prefix, group_key, prepped_data$clusterLabels,
    visibility = visibility
  )

  # The object holding the job details is lost if the session is interrupted,
  # so point to the job ID instead.
  result <- withCallingHandlers(
    .poll_for_results(
      job_id,
      api_url,
      poll_interval_seconds,
      timeout_seconds,
      auth_token = auth_token,
      show_progress = show_progress
    ),
    interrupt = function(cnd) {
      cat("\n")
      message(
        "The CyteType job ", job_id, " keeps running on the server. ",
        "Retrieve its results later with ", .results_hint(job_id), "."
      )
    }
  )

  if (!is.null(result)){
    result <- .normalize_results(result)
    results_table <- .transform_results_seurat(result, cluster_map = prepped_data$clusterLabels)
    obj <- .store_annotations_seurat(
      obj, result, job_id, results_prefix, group_key,
      prepped_data$clusterLabels,
      results_table = results_table
    )
    obj <- .add_legacy_annotation_columns(obj, results_table, results_prefix, group_key)
    .log_unannotated_clusters(result, prepped_data$clusterLabels)
    .log_result_locations(results_prefix, group_key)
  }

  return(obj)
}

#' Retrieve CyteType Analysis Results
#'
#' @description
#' If \code{obj} and \code{results_prefix} are given, tries to load from
#' \code{obj@misc} first. If no local result exists but job details are
#' available, fetches the result from the API.
#' If only \code{job_id} is given, fetches the result from the API.
#'
#' @param obj Seurat object that may contain stored results (optional).
#' @param job_id Job ID from submission (optional if \code{obj} + \code{results_prefix} have stored job details).
#' @param results_prefix Prefix used when storing results. Default \code{"cytetype"}.
#' @param auth_token Optional bearer token for API fetch. If `NULL`, uses
#'   saved CyteType credentials. When retrieving a job stored in `obj`, an
#'   explicit token also requires a matching `api_url`.
#' @param api_url Optional CyteType server origin for standalone or legacy job
#'   retrieval. When `NULL`, uses `CYTETYPE_API_URL` or
#'   `https://cytetype.nygen.io`.
#' @param save_json Logical. When the results are fetched by `job_id`, whether
#'   to save the server's response as `cytetypeR_results_<job_id>.json` in the
#'   working directory. Default is `TRUE`.
#' @return Depends on where the results come from:
#'   \itemize{
#'     \item `GetResults(obj)` after a run with the default prefix: the
#'       data.frame in `obj@misc$cytetype_results` (see [CyteTypeR()]).
#'     \item Results stored under another prefix, or fetched for the job stored
#'       in `obj`: a list with `annotations` (one list per annotated cluster
#'       with `clusterId`, the CyteType cluster ID, `annotation`,
#'       `ontologyTerm`, `ontologyTermID`, `granularAnnotation`, `cellState`,
#'       `justification`, `supportingMarkers`, `conflictingMarkers`,
#'       `missingExpression`, `unexpectedExpression`, `reviewerConfidence`,
#'       `isHeterogeneous` and `heterogeneityType`), `raw_annotations` (the
#'       server's per-cluster results, unchanged) and the server's
#'       `studyContext`, `summary`, `semanticOrder`, `clusterCategories` and
#'       `displayLevel`.
#'     \item `GetResults(job_id = ...)`: a data.frame with the same columns as
#'       `obj@misc$cytetype_results`, where `clusterId` is the CyteType cluster
#'       ID. Its attributes `"job_id"` and `"result"` hold the job ID and the
#'       list described above.
#'     \item `NULL`, with a message, when the job has not finished yet or is
#'       not found.
#'   }
#' @details
#' Fetching asks the server for the job's status and, when the job has
#' completed, for its results: two requests, without waiting. A failed job
#' stops with an error. When the status or the results cannot be fetched,
#' `GetResults()` signals a [cytetype_api_error()] condition whose
#' `error_code` holds the server's code when it sent one, such as
#' `"REPORT_DELETED"`. Problems with the API key signal its subclass
#' `cytetype_auth_error`.
#'
#' `reviewerConfidence` is `"High"`, `"Moderate"` or `"Low"` (`NULL` in the
#' list and `NA` in the table when missing), `isHeterogeneous` is `TRUE` or
#' `FALSE` (or missing), and `heterogeneityType` names the kind of
#' heterogeneity the reviewer found. In the list, `supportingMarkers` and
#' `conflictingMarkers` are lists of gene names.
#'
#' Fetched results are not added to `obj`; R does not modify the object you
#' pass in.
#' @examples
#' \dontrun{
#' # Results stored in the object by CyteTypeR()
#' results <- GetResults(annotated_seurat)
#'
#' # Results of a job by its ID, without saving the JSON file
#' results <- GetResults(job_id = "<JOB_ID>", save_json = FALSE)
#' }
#' @seealso [CyteTypeR()] for job submission
#' @importFrom jsonlite write_json
#' @export
GetResults <- function(obj = NULL,
                       job_id = NULL,
                       results_prefix = "cytetype",
                       auth_token = NULL,
                       api_url = NULL,
                       save_json = TRUE) {
  if (!is.null(obj) && !is.null(results_prefix)) {
    results_key <- paste0(results_prefix, "_results")
    job_details_key <- paste0(results_prefix, "_jobDetails")
    if (!is.null(obj@misc) && !is.null(obj@misc[[results_key]])) {
      stored_results <- obj@misc[[results_key]]
      if (
        is.list(stored_results) &&
          !is.data.frame(stored_results) &&
          all(c("job_id", "result") %in% names(stored_results))
      ) {
        # Results stored by earlier versions gain the keys added since.
        return(.normalize_results(stored_results$result))
      }
      return(stored_results)
    }
    if (!is.null(obj@misc) && !is.null(obj@misc[[job_details_key]])) {
      details <- obj@misc[[job_details_key]]
      job_id <- details$job_id
      job_api_url <- .resolve_api_url(details$api_url %||% api_url)
      if (!is.null(auth_token)) {
        if (is.null(api_url)) {
          .stop_authentication(
            paste(
              "api_url is required when auth_token is supplied",
              "for a job stored in a Seurat object"
            )
          )
        }
        explicit_api_url <- .validate_api_url(api_url)
        if (!identical(explicit_api_url, job_api_url)) {
          .stop_authentication(
            paste(
              "api_url must match the API origin stored with the job",
              "before auth_token can be used"
            ),
            "API_ORIGIN_MISMATCH"
          )
        }
      }
      token <- .resolve_auth_token(job_api_url, auth_token)
      status_resp <- .make_results_request(job_id, job_api_url, token)
      result <- .completed_result_or_null(status_resp, job_id, job_api_url)
      if (is.null(result)) {
        return(NULL)
      }
      return(.normalize_results(result))
    }
  }
  if (is.null(job_id)) {
    if (is.null(obj)) stop("Provide either obj (with stored job details) or job_id.")
    stop("No stored results or job details for prefix '", results_prefix, "'.")
  }
  api_url <- .resolve_api_url(api_url)
  auth_token <- .resolve_auth_token(api_url, auth_token)
  status_resp <- .make_results_request(job_id, api_url, auth_token)
  data <- .completed_result_or_null(status_resp, job_id, api_url)
  if (is.null(data)) {
    return(NULL)
  }
  if (isTRUE(save_json)) {
    json_path <- paste0("cytetypeR_results_", job_id, ".json")
    write_json(data, path = json_path, auto_unbox = TRUE, pretty = TRUE)
    log_info("Saved the server's response to {json_path}.")
  }
  result <- .normalize_results(data)
  .results_table_with_attributes(result, job_id)
}

# The results of a completed job; NULL with a message while the job is still
# running or cannot be found; an error when the job failed, and a
# cytetype_api_error when its status or results could not be fetched.
.completed_result_or_null <- function(status_resp, job_id, api_url) {
  status <- status_resp[["status"]] %||% "error"
  if (identical(status, "completed") && !is.null(status_resp[["result"]])) {
    return(status_resp[["result"]])
  }
  condition <- status_resp[["condition"]]
  if (inherits(condition, "cytetype_api_error")) {
    stop(condition)
  }
  if (identical(status, "failed")) {
    stop(status_resp[["message"]] %||% paste("Job", job_id, "failed."), call. = FALSE)
  }
  if (identical(status, "error")) {
    stop(cytetype_api_error(
      message = status_resp[["message"]] %||% paste("Could not retrieve job", job_id),
      call = status_resp[["error_type"]] %||% "api"
    ))
  }
  report_url <- .url_path(api_url, "report", job_id)
  if (status %in% c("pending", "processing")) {
    message(
      "Job ", job_id, " is still ", status, "; its results are not available yet. ",
      "Follow it at ", report_url, " and run ", .results_hint(job_id), " again once it has finished."
    )
  } else if (identical(status, "not_found")) {
    message(
      "Job ", job_id, " was not found on ", api_url, ". ",
      "Check the job ID and the API URL, or try again shortly after submitting a job."
    )
  } else {
    message("Job ", job_id, " has status '", status, "'; no results are available.")
  }
  NULL
}


#' Clean Up Artifacts
#'
#' @description
#' Deletes the artifact files (`vars.h5` and `obs.duckdb`) that
#' [PrepareCyteTypeR()] wrote, once [CyteTypeR()] has uploaded them.
#'
#' @param prepped_data Named list containing prepared data from `PrepareCyteTypeR()`.
#' @return Invisibly, `TRUE` when the artifact files had been built and are now
#'   removed, and `FALSE` when they were never built (nothing is deleted).
#' @examples
#' \dontrun{
#' prepped_data <- PrepareCyteTypeR(pbmc, pbmc.markers)
#' pbmc <- CyteTypeR(pbmc, prepped_data, study_context = "Human PBMCs")
#' CleanUpArtifacts(prepped_data)
#' }
#' @importFrom logger log_info log_warn
#' @export
CleanUpArtifacts <- function(prepped_data) {
  if (!isTRUE(prepped_data$build_succeeded)) {
    log_warn("Artifact files not built. No cleanup performed.")
    return(invisible(FALSE))
  }
  log_info("Cleaning up artifact files...")
  for (path in c(prepped_data$vars_h5_path, prepped_data$obs_duckdb_path)) {
    if (is.character(path) && length(path) == 1L && file.exists(path)) {
      unlink(path, force = TRUE)
      log_info("Removed {path}.")
    }
  }
  log_info("Artifact files cleaned up successfully.")
  invisible(TRUE)
}
