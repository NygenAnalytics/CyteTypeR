# CyteTypeR 0.6.0

## Results

* The evidence columns of `obj@misc$cytetype_results` (`justification`,
  `supportingMarkers`, `conflictingMarkers`, `missingExpression` and
  `unexpectedExpression`) are populated again. They had been empty since
  0.3.0, even for older jobs, because only six fields of each result were
  kept. For jobs that store the evidence in their structured output, an empty
  field is filled from it (the conclusion, the key supporting genes, the
  unexpected genes and the explanations of missing and unexpected genes);
  stored values are never overwritten. The CyteType server and the Python
  client apply the same rules.
* The results table gains three columns after the existing eleven:
  `reviewerConfidence` (`"High"`, `"Moderate"`, `"Low"` or `NA`),
  `isHeterogeneous` (`TRUE`, `FALSE` or `NA`) and `heterogeneityType`.
* `CyteTypeR()` adds the factor column
  `<prefix>_reviewerConfidence_<group_key>` to `obj@meta.data`, with the
  levels `"High"`, `"Moderate"`, `"Low"` and `"Unknown"` in that order (only
  the levels that occur are kept). The four existing columns and the three
  columns kept under older names are unchanged.
* With the default prefix, `obj@misc$cytetype_results` is still the
  per-cluster data.frame. It now carries the job ID and the full results as
  the attributes `"job_id"` and `"result"`.
* The full results (returned by `GetResults()` for other prefixes and for
  jobs fetched through the object) list every field for each cluster, plus
  `reviewerConfidence`, `isHeterogeneous` and `heterogeneityType`, and keep
  the server's per-cluster results unchanged as `raw_annotations`. Full
  results that earlier versions stored under another prefix gain the three
  keys (as `NULL`) when `GetResults()` reads them, but not the evidence
  fields, because those versions kept neither the evidence nor the raw
  results. With the default prefix, `GetResults(obj)` returns the stored
  table unchanged, so a table saved by an earlier version keeps its eleven
  columns and empty evidence; fetch the job again with
  `GetResults(job_id = ...)` for the new columns.
* A run with another `results_prefix` no longer overwrites
  `obj@misc$cytetype_results`; it only writes
  `obj@misc[[paste0(prefix, "_results")]]`.
* Clusters without a result (for example clusters that failed) no longer stop
  `CyteTypeR()` at the end of a run, which lost the annotated object: with two
  or more such clusters, 0.5.1 failed on duplicated `"NULL"` row names. These
  clusters now have no row in the results table, their cells are marked
  `"Unknown"`, and a warning names them. Raw results and already flat entries
  are recognized per cluster, and a result without a cluster ID takes the ID
  under which the server returned it.

## Retrieving results

* `GetResults(job_id = ...)` checks the job's status first. While the job is
  still running it returns `NULL` with a message instead of an empty table,
  and an unknown job also returns `NULL` with a message. A failed job is an
  error. HTTP 202 (`JOB_PROCESSING`) from the results endpoint is treated as
  "still running".
* `GetResults()` gains `save_json`. `GetResults(job_id = ...)` writes
  `cytetypeR_results_<job_id>.json` only when results are available, and
  `save_json = FALSE` skips the file. The returned table carries the job ID
  and the full results as attributes.
* `GetResults(obj)` prints a message when the stored job has not finished yet.
* When the status or the results of a job cannot be fetched, `GetResults()`
  signals a `cytetype_api_error` with one message, such as "Error while
  checking job status: HTTP 410 [REPORT_DELETED] This report has been deleted
  by the owner", and the server's code in `error_code`. 0.5.1 raised a plain
  error for jobs stored in the object. `cytetype_api_error()` gains the
  `error_code` argument, and these conditions print as `Error: <message>`
  instead of `Error in "api" : <message>`. Problems with the API key still
  signal `cytetype_auth_error`.

## Running jobs

* `timeout_seconds` is enforced. 0.5.1 compared the elapsed time in automatic
  `difftime` units (minutes or hours), so the timeout effectively never
  fired. When it is reached, `CyteTypeR()` stops with an error of class
  `cytetype_timeout_error` that names the job ID and the
  `GetResults(job_id = ...)` call to fetch the results later; the job keeps
  running on the server. The default is still 7200 seconds, so jobs that take
  longer now stop waiting.
* While waiting, network errors (no response from the server) are retried
  until `timeout_seconds` is reached, as in the Python client. Other errors
  still stop the run after five in a row, now with a `cytetype_api_error`
  whose message says how to fetch the results later. An error while fetching
  the results of a completed job also stops the run with a
  `cytetype_api_error`.
* The hint printed when a job is submitted, and the message printed when a run
  is interrupted, show `GetResults(job_id = "<job_id>")`. The earlier
  `GetResults()` hint could not work, because an interrupted run does not
  return the object that holds the job details.
* New `visibility` argument of `CyteTypeR()`: `"private"`, `"org"` or
  `"public"`, sent as the request's top-level `visibility` and stored in the
  job details. The default, `NULL`, uses the server default (`"private"`).
* `llm_configs` accepts the `deepseek`, `nebius` and `together` providers.
* The rate-limit message no longer says that your own LLM API key bypasses the
  free-tier limits. The limits apply to every job, including jobs that use
  your own LLM.
* Error messages from the server are read in all the forms it sends: the
  server's own `{"error_code", "message"}`, a plain text explanation, and the
  list of validation errors of an HTTP 422 response. 0.5.1 showed only the
  HTTP status for the last two, for example "Job submission failed: HTTP 400
  Bad Request.".

## Preparing data

* Seurat v5 objects whose RNA layers are split (for example by sample) are
  supported. The layers are joined on a copy; your object is not changed.
* A default assay other than `"RNA"` (such as `"SCT"`) no longer breaks the
  preparation. Expression, gene names and the uploaded files all come from the
  RNA assay, and the warning that RNA is used is shown once.
* `NA` values in the `group_key` column stop `PrepareCyteTypeR()` early with
  a clear error instead of "subscript out of bounds".
* Sampling cells for the report's plot no longer resets the global random
  number generator (`set.seed(42)` was called in your session); a local seed
  keeps the sample reproducible. A cluster with a single cell now keeps that
  cell; 0.5.1 could pick another one.
* Expression percentages are mapped to the right clusters when the cluster
  column is a factor with unused levels.
* Coordinates fall back from `coordinates_key` to `"umap"`, `"tsne"` and
  `"pca"`, matched case-insensitively. Without any, the job runs without the
  plot and the preparation continues. `prepped_data$coordinates_key` names the
  reduction that was used, or is `NULL` when none was.
* Raw counts are written to `vars.h5` only when they are whole numbers. 0.5.1
  truncated other values silently.
* The counts and normalized data are read directly from the RNA assay. With
  SeuratObject 4, 0.5.1 read the normalized values where it meant the raw
  counts.
* The gene symbol checks work: a warning is logged when most genes of the
  marker table or of the RNA assay look like database IDs (Ensembl IDs with
  or without a version suffix, RefSeq accessions or numeric IDs). The checks
  only warn.
* DuckDB spills to a private temporary directory that is removed afterwards,
  instead of `tmp/duckdb` in the working directory. With duckdb versions that
  support it, writing `obs.duckdb` no longer uses the shared `~/.duckdb`
  directory.
* The feature index in `vars.h5` holds gene names for Seurat v5 objects
  instead of row numbers.
* `PrepareCyteTypeR()` no longer assigns `obj@misc$query`, which never reached
  the caller's object.

## Sign-in and the cytetyper command

* `cytetyper login` works. The launcher runs R non-interactively, where the
  hidden prompt returned nothing, so the command stopped with "API key is
  required". The key is now read from standard input and can be piped in:
  `printf '%s\n' "$CYTETYPE_API_KEY" | cytetyper login`. Typed into a
  terminal, the key is hidden on Linux and macOS (through `stty`) but shown on
  Windows. `LoginCyteTypeR()` reads standard input the same way when R is not
  interactive.
* `OpenCyteTypeDashboard()` documents that it opens
  `https://cytetype.nygen.io/dashboard` when no credentials are saved.

## Package

* CyteTypeR is now released under the BSD 3-Clause License.
* `duckdb`, `SeuratObject` and `withr` are required dependencies, and R 4.2 or
  newer is needed (the current duckdb requires it). `tibble` and `tidyr` are
  no longer used. `duckdb` is checked before `vars.h5` is written.
* `CleanUpArtifacts()` returns `TRUE` invisibly when it removed the files and
  `FALSE` when they were never built.
* New package help page (`?"CyteTypeR-package"`), `URL` and `BugReports`
  fields, and corrected help for `results_prefix`, the return values and the
  examples.
* The documentation moved to <https://docs.nygen.io/CyteType/>, which covers
  the R and Python clients. The "Getting Started" vignette is now a short
  overview with links, the "Configurations" vignette was removed, and
  `inst/cytetypeR_table_export.tsv` was removed because it no longer matched
  the results table.
