## API configuration

CyteTypeR uses `https://cytetype.nygen.io` by default. An explicit `api_url` takes precedence over the `CYTETYPE_API_URL` environment variable and the package default.

For normal production use, no URL configuration is required:

```r
SetupCyteTypeR()
```

### Development API

Replace `<URL>` with the development URL supplied for your environment:

```r
SetupCyteTypeR(api_url = "<URL>")
```

```sh
export CYTETYPE_API_URL="<URL>"
cytetyper setup
```

You can also configure one command:

```sh
cytetyper setup --api-url "<URL>"
```

Remote API URLs must use HTTPS. Plain HTTP is accepted only for `localhost` and `127.0.0.1`.

## Authentication

`SetupCyteTypeR()` validates matching saved credentials or opens the browser-based email and one-time-code flow when none are available. If the saved key is invalid or inactive, use `SetupCyteTypeR(force = TRUE)` or `cytetyper setup --force` to authenticate with a new key. `LoginCyteTypeR()` validates and saves an existing API key through a hidden prompt.

```r
SetupCyteTypeR()
LoginCyteTypeR()
LogoutCyteTypeR()
```

```sh
cytetyper setup
cytetyper login
cytetyper logout
```

CyteTypeR and the Python client share the same local credentials. The active credentials are tied to the API URL used during sign-in.

## LLM configurations

An external LLM key is optional. Without `llm_configs`, CyteType uses its managed default model.

Store provider keys in environment variables instead of writing them directly in scripts:

```r
llm_configs <- list(
  provider = "openai",
  name = "gpt-4o-mini",
  apiKey = Sys.getenv("OPENAI_API_KEY"),
  baseUrl = "https://api.openai.com/v1",
  modelSettings = list(temperature = 0.0, max_tokens = 4096L)
)

annotated_pbmc <- CyteTypeR(
  obj = pbmc,
  prepped_data = prepped_data,
  study_context = "PBMC blood samples from humans",
  metadata = metadata,
  llm_configs = llm_configs,
  save_query = FALSE
)
```

Set `save_query = FALSE` whenever `llm_configs` contains provider credentials. Current query-file output includes the complete LLM configuration.

Supported provider names in this package version are:

```r
c(
  "anthropic", "bedrock", "fireworks", "google", "groq",
  "huggingface", "mistral", "openai", "openrouter", "vertex", "xai"
)
```

### Multiple models

Pass an unnamed list of model configuration lists:

```r
llm_configs <- list(
  list(
    provider = "openai",
    name = "gpt-4o-mini",
    apiKey = Sys.getenv("OPENAI_API_KEY")
  ),
  list(
    provider = "anthropic",
    name = "claude-sonnet-4-20250514",
    apiKey = Sys.getenv("ANTHROPIC_API_KEY"),
    allowFallback = TRUE
  )
)

annotated_pbmc <- CyteTypeR(
  obj = pbmc,
  prepped_data = prepped_data,
  llm_configs = llm_configs,
  save_query = FALSE
)
```

