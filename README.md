# CyteTypeR

## Introduction

CyteTypeR is the R version of CyteType, a python package for deep characterization of cell clusters from single cell RNA-seq data <https://github.com/NygenAnalytics/CyteType>. Current version of this package interfaces with Seurat objects after clustering and characterize cell clusters through CyteType API.

This vignettte demonstrates a basic workflow using a sample PBMC dataset that has been processed through the standard Seurat analysis pipeline.

## Installation
```{r eval=FALSE}
# Using devtools
install.packages("devtools")
# Install from GitHub
library(devtools)
install_github("NygenAnalytics/CyteTypeR")

```

## Quick Start
```{r eval=FALSE}
# Load package
library(CyteTypeR)

prepped_data <- PrepareCyteTypeR(pbmc,
         pbmc.markers,
         n_top_genes = 10,
         group_key = 'seurat_clusters',
         aggregate_metadata = TRUE,
         coordinates_key = "umap")

metadata <- list(
  title = 'My scRNA-seq analysis of human pbmc',
  run_label = 'initial_analysis',
  experiment_name: 'pbmc_human_samples_study')

results <- CyteTypeR(prepped_data = prepped_data, 
                          study_context = "pbmc blood samples from humans", 
                          metadata = metadata
                          )
```

## Pre-processing
Current version of CyteTypeR works with Seurat objects and requires minimally some basic pre-processing before CyteTypeR can be used.
``` {r eval=FALSE}

# Load libraries ####
library(dplyr)
library(patchwork)
library(Matrix)
library(Seurat)


# Load the dataset ####
pbmc.data <- Read10X(data.dir = "./data/filtered_gene_bc_matrices/hg19/")
# Initialize the Seurat object with the raw (non-normalized data).
pbmc <- CreateSeuratObject(counts = pbmc.data, project = "pbmc3k", min.cells = 3, min.features = 200)
pbmc <- NormalizeData(pbmc, normalization.method = "LogNormalize", scale.factor = 10000)
pbmc <- FindVariableFeatures(pbmc, selection.method = "vst", nfeatures = 2000)

all.genes <- rownames(pbmc)
pbmc <- ScaleData(pbmc, features = all.genes)

# Cluster the cells and run UMAP #####
pbmc <- FindNeighbors(pbmc, dims = 1:10)
pbmc <- FindClusters(pbmc, resolution = 0.5)

pbmc <- RunUMAP(pbmc, dims = 1:10)

# Find markers for all Clusters #####
pbmc.markers <- FindAllMarkers(pbmc, only.pos = TRUE)

# (optional:)
pbmc.markers %>%
  group_by(cluster) %>%
  dplyr::filter(avg_log2FC > 1)

```

## Run CyteTypeR
``` {r eval=FALSE}
## Prep data for job submission to cytetype api
prepped_data <- PrepareCyteTypeR(pbmc,
         pbmc.markers,
         n_top_genes = 10,
         group_key = 'seurat_clusters',
         aggregate_metadata = TRUE,
         coordinates_key = "umap")

## Adding metadata on 
metadata <- list(
  title = 'My scRNA-seq analysis of human pbmc',
  run_label = 'initial_analysis',
  experiment_name: 'pbmc_human_samples_study')


## Submit job to cytetype
results <- CyteTypeR(prepped_data = prepped_data, 
                          study_context = "pbmc blood samples from humans", 
                          metadata = metadata
                          )

```
