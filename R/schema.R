# Valid LLM Provider Names
LLM_PROVIDERS <- c("google", "openai", "anthropic", "groq", "mistral", "openrouter", "bedrock")

# Valid Agent Type Names
AGENT_TYPES <- c("contextualizer", "annotator", "reviewer", "summarizer", "clinician", "chat")

# Create LLM Model Configuration Object
LLMModelConfig <- function(provider,
                           name,
                           apiKey = NULL,
                           baseUrl = NULL,
                           awsAccessKeyId = NULL,
                           awsSecretAccessKey = NULL,
                           awsDefaultRegion = NULL,
                           modelSettings = NULL,
                           targetAgents = character(0),
                           skipValidation = FALSE,
                           allowFallback = FALSE) {

  # Create the object
  obj <- list(
    provider = provider,
    name = name,
    apiKey = apiKey,
    baseUrl = baseUrl,
    awsAccessKeyId = awsAccessKeyId,
    awsSecretAccessKey = awsSecretAccessKey,
    awsDefaultRegion = awsDefaultRegion,
    modelSettings = modelSettings,
    targetAgents = targetAgents,
    skipValidation = skipValidation,
    allowFallback = allowFallback
  )

  # Set class
  class(obj) <- "LLMModelConfig"

  # Validate the object
  .validate_llm_model_config(obj)

  return(obj)
}

# Validate LLM Model Configuration

.validate_llm_model_config <- function(obj) {
  # Check provider is valid
  if (!obj$provider %in% LLM_PROVIDERS) {
    stop(paste("provider must be one of:", paste(LLM_PROVIDERS, collapse = ", ")))
  }

  # Check required fields
  if (!is.character(obj$name) || length(obj$name) != 1) {
    stop("name must be a single character string")
  }

  # Check target agents (can be NULL or character vector)
  if (!is.null(obj$targetAgents)) {
    if (!is.character(obj$targetAgents)) {
      stop("targetAgents must be a character vector or NULL")
    }
    if (length(obj$targetAgents) > 0) {
      invalid_agents <- setdiff(obj$targetAgents, AGENT_TYPES)
      if (length(invalid_agents) > 0) {
        stop(paste("Invalid agent types:", paste(invalid_agents, collapse = ", ")))
      }
    }
  }

  # Check boolean fields
  if (!is.logical(obj$skipValidation) || length(obj$skipValidation) != 1) {
    stop("skipValidation must be a single logical value")
  }

  if (!is.logical(obj$allowFallback) || length(obj$allowFallback) != 1) {
    stop("allowFallback must be a single logical value")
  }

  # AWS credentials validation (equivalent to Python's model_validator)
  .check_aws_credentials(obj)

  return(TRUE)
}

.prepare_query_for_json <- function(query_list) {
  # Convert LLMModelConfig objects to plain lists
  if (!is.null(query_list$llm_configs)) {
    query_list$llm_configs <- lapply(query_list$llm_configs, function(config) {
      obj_list <- unclass(config)
      # Remove NULL values
      obj_list[!sapply(obj_list, is.null)]
    })
  }
  return(query_list)
}

# Check AWS Credentials
.check_aws_credentials <- function(obj) {
  # If apiKey is provided, validation passes
  if (!is.null(obj$apiKey)) {
    return(TRUE)
  }

  # Check if ANY AWS credential is provided
  aws_fields_provided <- c(
    !is.null(obj$awsAccessKeyId),
    !is.null(obj$awsSecretAccessKey),
    !is.null(obj$awsDefaultRegion)
  )

  # If any AWS credential is provided, ALL must be provided
  if (any(aws_fields_provided)) {
    if (!all(aws_fields_provided)) {
      stop("All AWS credentials must be provided if any of them are provided")
    }
    return(TRUE)
  }

  # If no apiKey and no AWS credentials, throw error
  stop("Either apiKey or all AWS credentials must be provided")
}

# Create Input Data Object for CyteType Analysis
InputData <- function(studyInfo = "",
                      infoTags = list(),
                      clusterLabels = list(),
                      clusterMetadata = list(),
                      markerGenes = list(),
                      visualizationData = NULL,
                      expressionData = list()) {

  # Create the object
  obj <- list(
    studyInfo = studyInfo,
    infoTags = infoTags,
    clusterLabels = clusterLabels,
    clusterMetadata = clusterMetadata,
    markerGenes = markerGenes,
    visualizationData = visualizationData,
    expressionData = expressionData
  )

  # Set class
  class(obj) <- "InputData"

  # Validate the object
  .validate_input_data(obj)

  return(obj)
}

# Validate Input Data Object

.validate_input_data <- function(obj) {
  # Check required types
  if (!is.character(obj$studyInfo) || length(obj$studyInfo) != 1) {
    stop(paste("studyInfo must be a single character string. String: ",length(obj$studyInfo), "\nClass?:", class(obj$studyInfo)))
  }

  if (!is.list(obj$infoTags)) {
    stop("infoTags must be a named list")
  }

  # Validate infoTags structure (all values should be character)
  if (length(obj$infoTags) > 0) {
    tag_types <- sapply(obj$infoTags, is.character)
    if (!all(tag_types)) {
      stop("All infoTags values must be character strings")
    }
  }

  if (!is.list(obj$clusterLabels)) {
    stop("clusterLabels must be a named list")
  }

  # Validate clusterLabels structure (all values should be character)
  if (length(obj$clusterLabels) > 0) {
    label_types <- sapply(obj$clusterLabels, is.character)
    if (!all(label_types)) {
      stop("All clusterLabels values must be character strings")
    }
  }

  if (!is.list(obj$clusterMetadata)) {
    stop("clusterMetadata must be a nested list structure")
  }

  if (!is.list(obj$markerGenes)) {
    stop("markerGenes must be a named list")
  }

  if (!is.null(obj$visualizationData) && !is.list(obj$visualizationData)) {
    stop("visualizationData must be NULL or a list")
  }

  if (!is.list(obj$expressionData)) {
    stop("expressionData must be a named list")
  }

  # Validate nested structure of clusterMetadata
  if (length(obj$clusterMetadata) > 0) {
    for (cluster_id in names(obj$clusterMetadata)) {
      cluster_data <- obj$clusterMetadata[[cluster_id]]
      if (!is.list(cluster_data)) {
        stop(paste("clusterMetadata for", cluster_id, "must be a list"))
      }

      if (length(cluster_data) > 0) {
        for (metadata_col in names(cluster_data)) {
          metadata_values <- cluster_data[[metadata_col]]
          if (!is.list(metadata_values)) {
            stop(paste("clusterMetadata values for", cluster_id, metadata_col, "must be a list"))
          }
          if (length(metadata_values) > 0) {
            numeric_check <- sapply(metadata_values, is.numeric)
            if (!all(numeric_check)) {
              stop(paste("clusterMetadata values for", cluster_id, metadata_col, "must be numeric"))
            }
          }
        }
      }
    }
  }

  # Validate markerGenes structure
  if (length(obj$markerGenes) > 0) {
    for (cluster_id in names(obj$markerGenes)) {
      genes <- obj$markerGenes[[cluster_id]]
      if (!is.character(genes) && !is.null(genes)) {
        stop(paste("markerGenes for", cluster_id, "must be a character vector or NULL"))
      }
    }
  }

  # Validate expressionData structure
  if (length(obj$expressionData) > 0) {
    for (gene_name in names(obj$expressionData)) {
      gene_data <- obj$expressionData[[gene_name]]
      if (!is.list(gene_data)) {
        stop(paste("expressionData for", gene_name, "must be a list"))
      }
      if (length(gene_data) > 0) {
        numeric_check <- sapply(gene_data, is.numeric)
        if (!all(numeric_check)) {
          stop(paste("expressionData for", gene_name, "must contain only numeric values"))
        }
      }
    }
  }

  return(TRUE)
}

# Get Example Input Data
get_example_input_data <- function() {
  InputData(
    studyInfo = "Adult human brain tissue samples from healthy controls and Alzheimer's disease patients, analyzed using 10X Genomics single-cell RNA-seq. Samples include cortical and hippocampal regions.",

    infoTags = list(
      "Study" = "Alzheimer's Disease Brain Atlas",
      "DOI" = "https://doi.org/10.1038/s41586-023-06063-y",
      "GEO Accession" = "GSE157827",
      "PubMed" = "https://pubmed.ncbi.nlm.nih.gov/37258686/",
      "Dataset" = "10X Genomics scRNA-seq",
      "Tissue" = "Human brain cortex and hippocampus"
    ),

    clusterLabels = list(
      "Cluster1" = "Astrocytes",
      "Cluster2" = "Neurons"
    ),

    clusterMetadata = list(
      "Cluster1" = list(
        "condition" = list("healthy" = 60, "alzheimer" = 40),
        "region" = list("cortex" = 70, "hippocampus" = 30),
        "donor" = list("donor1" = 25, "donor2" = 35, "donor3" = 40)
      ),
      "Cluster2" = list(
        "condition" = list("healthy" = 80, "alzheimer" = 20),
        "region" = list("cortex" = 90, "hippocampus" = 10),
        "donor" = list("donor1" = 30, "donor2" = 30, "donor3" = 40)
      )
    ),

    markerGenes = list(
      "Cluster1" = c("GFAP", "S100B", "AQP4"),
      "Cluster2" = c("RBFOX3", "MAP2", "SYP")
    ),

    visualizationData = list(
      coordinates = list(
        c(-2.1, 1.5),
        c(3.2, -0.8),
        c(1.7, 2.3),
        c(-1.1, -1.9),
        c(2.8, 1.1),
        c(-0.5, 3.1),
        c(4.2, -2.1),
        c(0.3, 0.7)
      ),
      clusters = c(
        "Cluster1", "Cluster1", "Cluster2", "Cluster2",
        "Cluster1", "Cluster2", "Cluster1", "Cluster2"
      )
    ),

    expressionData = list(
      "GFAP" = list(
        "Cluster1" = 85.2,
        "Cluster2" = 3.1,
        "Cluster3" = 4.5
      ),
      "S100B" = list(
        "Cluster1" = 92.7,
        "Cluster2" = 2.8,
        "Cluster3" = 5.2
      )
    )
  )
}
