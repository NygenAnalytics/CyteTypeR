# Get Started
This guide demonstrates a basic CyteType job using a sample PBMC dataset from 10X Genomics.

**For AnnData or the Python client, see [CyteType](https://github.com/NygenAnalytics/CyteType).**

## Authentication

Sign in once on each machine before submitting a job:

```R
library(CyteTypeR)
SetupCyteTypeR()
```

The browser flow verifies your email with a one-time code and saves a personal API key locally. If you already have an API key, use `LoginCyteTypeR()` to enter it through a hidden prompt.

To use CyteTypeR directly from the terminal, first install the launcher into the active R environment:

```R
InstallCyteTypeRCli()
```

```sh
cytetyper setup
cytetyper dashboard
cytetyper view <JOB_ID>
cytetyper logout
```

The R and Python clients share the same local CyteType credentials file.

### Development environments

Replace `<URL>` with the development URL supplied for your environment:

```R
SetupCyteTypeR(api_url = "<URL>")
```

```sh
cytetyper setup --api-url "<URL>"
```

Alternatively, configure the current terminal session:

```sh
export CYTETYPE_API_URL="<URL>"
cytetyper setup
```

## Quick Start Example
``` R
# Load package
library(CyteTypeR)

# Sign in if this machine is not configured
SetupCyteTypeR()

prepped_data <- PrepareCyteTypeR(
  pbmc,
  pbmc.markers,
  n_top_genes = 10,
  group_key = "seurat_clusters",
  aggregate_metadata = TRUE,
  coordinates_key = "umap"
)

metadata <- list(
  title = "My scRNA-seq analysis of human PBMCs",
  run_label = "initial_analysis",
  experiment_name = "pbmc_human_samples_study"
)

annotated_pbmc <- CyteTypeR(
  obj = pbmc,
  prepped_data = prepped_data,
  study_context = "PBMC blood samples from humans",
  metadata = metadata
)
```

## Pre-processing

CyteTypeR requires a Seurat object with normalized RNA expression, cluster assignments, marker genes, and optionally a dimensional reduction for report visualization.

An example of the expected marker genes table can be found here: [marker-table-example.tsv](/inst/marker-table-example.tsv)

``` R
# Load libraries
library(dplyr)
library(patchwork)
library(Matrix)
library(Seurat)
library(CyteTypeR)

# Load an example dataset from MTX format files from 10X
pbmc.data <- Read10X(data.dir = "./data/filtered_gene_bc_matrices/hg19/")

# Initialize and normalize the Seurat object
pbmc <- CreateSeuratObject(counts = pbmc.data, project = "pbmc3k", min.cells = 3, min.features = 200)
pbmc <- NormalizeData(pbmc, normalization.method = "LogNormalize", scale.factor = 10000)
pbmc <- FindVariableFeatures(pbmc, selection.method = "vst", nfeatures = 2000)

all.genes <- rownames(pbmc)
pbmc <- ScaleData(pbmc, features = all.genes)

# Cluster the cells and run UMAP
pbmc <- FindNeighbors(pbmc, dims = 1:10)
pbmc <- FindClusters(pbmc, resolution = 0.5)
pbmc <- RunUMAP(pbmc, dims = 1:10)

# Find and filter markers for all clusters
pbmc.markers <- FindAllMarkers(pbmc, only.pos = TRUE)
pbmc.markers <- pbmc.markers %>%
  group_by(cluster) %>%
  dplyr::filter(avg_log2FC > 1)
```

## Running CyteTypeR

CyteTypeR first prepares local artifacts, then submits the job and retrieves the result.

``` R
# Prepare data for submission
prepped_data <- PrepareCyteTypeR(
  pbmc,
  pbmc.markers,
  n_top_genes = 10,
  group_key = "seurat_clusters",
  aggregate_metadata = TRUE,
  coordinates_key = "umap"
)

# Add metadata to the report
metadata <- list(
  title = "My scRNA-seq analysis of human PBMCs",
  run_label = "initial_analysis",
  experiment_name = "pbmc_human_samples_study"
)

# Submit the job and add results to the Seurat object
annotated_pbmc <- CyteTypeR(
  obj = pbmc,
  prepped_data = prepped_data,
  study_context = "PBMC blood samples from humans",
  metadata = metadata
)
```

## Open reports and retrieve results

Successful submissions print a report URL under `https://cytetype.nygen.io`. Open the dashboard or a known job from R:

```R
OpenCyteTypeDashboard()
ViewCyteTypeJob("<JOB_ID>")
```
### Results table
CyteType results are saved under "misc" in the seurat object e.g.```seurat_obj@misc$cytetype_results```
Example of the results table: [cytetypeR_table_export.tsv](/inst/cytetypeR_table_export.tsv)

```
## View results table
> View(pbmc@misc[["cytetype_results"]])

## Cluster annotation and result are stored in each row, use names() to check result table fields.
> names(pbmc_small@misc[["cytetype_results"]])
 [1] "clusterId"            "annotation"           "ontologyTerm"         "granularAnnotation"   "cellState"           
 [6] "justification"        "supportingMarkers"    "conflictingMarkers"   "missingExpression"    "unexpectedExpression"


Using CLI:

```sh
cytetyper dashboard
cytetyper view <JOB_ID>
```

Results from a completed run are stored in the returned Seurat object. With the default prefix, the transformed result table is available at:

```R
View(annotated_pbmc@misc[["cytetype_results"]])
GetResults(annotated_pbmc)
```
