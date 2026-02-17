test_that("LLMModelConfig accepts valid config with apiKey", {
  cfg <- CyteTypeR:::LLMModelConfig(provider = "openai", name = "gpt-4", apiKey = "sk-fake")
  expect_s3_class(cfg, "LLMModelConfig")
  expect_identical(cfg$provider, "openai")
  expect_identical(cfg$name, "gpt-4")
})

test_that("LLMModelConfig rejects invalid provider", {
  expect_error(
    CyteTypeR:::LLMModelConfig(provider = "invalid_provider", name = "x", apiKey = "k"),
    "provider must be one of"
  )
})

test_that("LLMModelConfig rejects invalid targetAgents", {
  expect_error(
    CyteTypeR:::LLMModelConfig(provider = "openai", name = "x", apiKey = "k", targetAgents = c("annotator", "invalid_agent")),
    "Invalid agent types"
  )
})

test_that("LLMModelConfig accepts valid targetAgents", {
  cfg <- CyteTypeR:::LLMModelConfig(provider = "openai", name = "x", apiKey = "k", targetAgents = c("annotator", "reviewer"))
  expect_identical(cfg$targetAgents, c("annotator", "reviewer"))
})

test_that("LLMModelConfig requires apiKey or all AWS credentials", {
  expect_error(
    CyteTypeR:::LLMModelConfig(provider = "bedrock", name = "x"),
    "Either apiKey or all AWS credentials"
  )
  expect_error(
    CyteTypeR:::LLMModelConfig(provider = "bedrock", name = "x", awsAccessKeyId = "only-one"),
    "All AWS credentials must be provided"
  )
})

test_that("LLMModelConfig accepts partial AWS when apiKey set", {
  cfg <- CyteTypeR:::LLMModelConfig(provider = "bedrock", name = "x", apiKey = "k")
  expect_s3_class(cfg, "LLMModelConfig")
})

test_that("get_example_input_data returns valid InputData structure", {
  ex <- CyteTypeR:::get_example_input_data()
  expect_s3_class(ex, "InputData")
  expect_identical(length(ex$clusterLabels), 2L)
  expect_true(is.character(ex$studyInfo) && length(ex$studyInfo) == 1L)
  expect_identical(ex$nParallelClusters, 5)
})

test_that("InputData rejects nParallelClusters out of range", {
  valid <- list(
    studyInfo = "", infoTags = list(), clusterLabels = list(),
    clusterMetadata = list(), markerGenes = list(), visualizationData = NULL,
    expressionData = list(), nParallelClusters = 5
  )
  expect_error(
    CyteTypeR:::InputData(studyInfo = "x", infoTags = list(), clusterLabels = list(),
                          clusterMetadata = list(), markerGenes = list(),
                          visualizationData = NULL, expressionData = list(), nParallelClusters = 0),
    "n_parallel_clusters"
  )
  expect_error(
    CyteTypeR:::InputData(studyInfo = "x", infoTags = list(), clusterLabels = list(),
                          clusterMetadata = list(), markerGenes = list(),
                          visualizationData = NULL, expressionData = list(), nParallelClusters = 51),
    "n_parallel_clusters"
  )
})

test_that(".prepare_query_for_json strips jobDetails and converts llm_configs", {
  cfg <- CyteTypeR:::LLMModelConfig(provider = "openai", name = "x", apiKey = "k")
  input_data <- list(
    studyInfo = "x", infoTags = list(), clusterLabels = list("1" = "A"),
    clusterMetadata = list(), markerGenes = list("1" = "G1"),
    visualizationData = NULL, expressionData = list(), nParallelClusters = 2,
    cytetype_jobDetails = list(job_id = "j1")
  )
  query_list <- list(input_data = input_data, llm_configs = list(cfg))
  out <- CyteTypeR:::.prepare_query_for_json(query_list)
  expect_false("cytetype_jobDetails" %in% names(out$input_data))
  expect_true(is.list(out$llm_configs[[1]]) && !inherits(out$llm_configs[[1]], "LLMModelConfig"))
})
