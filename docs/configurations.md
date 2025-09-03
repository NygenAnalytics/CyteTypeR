## Configuration

### LLM configurations

``` r


# Use a named list for llm configs
llm_configs=list(
  "provider" = "openai",   # one of: anthropic, bedrock, google, groq, mistral, openai, openrouter
  "name" = "gpt-4o-mini",
  "apiKey" = "your-api-key",
  "baseUrl" = "https://api.openai.com/v1",   # optional
  "modelSettings" = list(                       # optional
    "temperature" = 0.0,
    "max_tokens" = 4096
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

