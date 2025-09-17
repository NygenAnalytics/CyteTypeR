# CyteTypeR

CyteTypeR is the R version of CyteType, a single‑cell RNA‑seq cluster annnotation using multi-agent workflow [CyteType](https://github.com/NygenAnalytics/CyteType). 

Current version of this package interfaces with Seurat objects after clustering and characterize cell clusters through CyteType API.

**For AnnData .h5ad, check out the python client: [CyteType](https://github.com/NygenAnalytics/CyteType)**


## Installation
``` R
# Using devtools
install.packages("devtools")

# Install from GitHub
library(devtools)
install_github("NygenAnalytics/CyteTypeR")

```

## Quick Start Example
``` R
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
  experiment_name = 'pbmc_human_samples_study')

results <- CyteTypeR(obj=pbmc,
                          prepped_data = prepped_data, 
                          study_context = "pbmc blood samples from humans", 
                          metadata = metadata
                          )
```

## Documentations
- [Get started](docs/get-started.md)
- [Configurations](docs/configurations.md)

## License

Licensed under CC BY‑NC‑SA 4.0 — see: [LICENSE.md](LICENSE.md)
