## Configuration

### LLM configurations

``` r

# For one LLM configuration:
llm_configs <- list(
  list(
    provider = "openai",
    name = "gpt-4o-mini",
    apiKey = "your-openai-key",
    baseUrl = "https://api.openai.com/v1", 
    modelSettings = list(temperature = 0.0, max_tokens = 4096L)
  )
)


# List of named lists for multiple configurations
llm_configs <- list(
  list(
    provider = "openai",
    name = "gpt-4o-mini",
    apiKey = "your-openai-key",
    baseUrl = "https://api.openai.com/v1", 
    modelSettings = list(temperature = 0.0, max_tokens = 4096L)
  ),
  list(
    provider = "anthropic", 
    name = "claude-3-sonnet-123124",
    apiKey = "your-anthropic-key",
    modelSettings = list(temperature = 0.0, max_tokens = 4096L)
  )
)

# Example of usage:
result <- CyteTypeR(obj = pbmc, 
                    prepped_data = prepped_data,
                    study_context = "pbmc blood samples from humans",
                    metadata = metadata,
                    llm_configs = llm_configs
)
```

