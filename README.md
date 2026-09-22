<h1 align="left">CyteTypeR</h1>
<h3 align="left">Agentic, Evidence-Based Cell Type Annotation for Single-Cell RNA-seq in R</h3>

<p align="left">
  <a href="https://github.com/NygenAnalytics/CyteTypeR/actions/workflows/R-CMD-check.yaml">
    <img src="https://github.com/NygenAnalytics/CyteTypeR/actions/workflows/R-CMD-check.yaml/badge.svg" alt="R-CMD-check">
  </a>
  <img src="https://img.shields.io/badge/R-%E2%89%A5%204.1.0-blue.svg" alt="R Version">
  <a href="LICENSE.md">
    <img src="https://img.shields.io/badge/License-CC%20BY--NC--SA%204.0-lightgrey.svg" alt="License: CC BY-NC-SA 4.0">
  </a>
</p>

**CyteTypeR** is an end-to-end cell type annotation system for **single-cell RNA sequencing (scRNA-seq)**, designed for repeatable analysis pipelines rather than one-off prompting. It combines cluster-level marker genes, expression context, study metadata, literature retrieval, ontology mapping, and a dedicated review step in a structured workflow that operates directly on Seurat objects.

For AnnData workflows, use [CyteType](https://github.com/NygenAnalytics/CyteType).

> [!IMPORTANT]
> Already installed CyteTypeR? Open R and run `devtools::install_github("NygenAnalytics/CyteTypeR", force = TRUE)` to update to the latest version before you start.
>
> CyteType requires an API key. API use is free for academic and non-commercial research. Commercial API use requires a [license](#license).

## Why CyteTypeR?

Manual cell type annotation takes weeks and varies between experts. CyteTypeR delivers consistent, expert-level annotations in minutes using a multi-agent AI system where specialized agents collaborate on marker analysis, literature evidence, and Cell Ontology mapping.

## Quick Start

### 1. Install

```R
install.packages("devtools")
devtools::install_github("NygenAnalytics/CyteTypeR")
```

### 2. Set up your API key

```R
library(CyteTypeR)
SetupCyteTypeR()
```

This opens passwordless CyteType sign-in in your browser and saves the API key locally for automatic use from R. You can also [create or manage API keys in the dashboard](https://cytetype.nygen.io/dashboard).

Already have an API key? Save and validate it locally once:

```R
LoginCyteTypeR()
```

### 2.1. (Optional) Install and use the CLI

Install the `cytetyper` command into the active R environment:

```R
InstallCyteTypeRCli()
```

Then authenticate directly from the terminal:

```sh
cytetyper setup
cytetyper dashboard
cytetyper view <JOB_ID>
cytetyper logout
```

### 3. Annotate with Seurat

```R
# Assumes a normalized, clustered Seurat object and marker table
prepped_data <- PrepareCyteTypeR(
  seurat_obj,
  marker_genes,
  group_key = "seurat_clusters",
  coordinates_key = "umap"
)

metadata <- list(
  title = 'My scRNA-seq analysis of human pbmc',
  run_label = 'initial_analysis',
  experiment_name = 'pbmc_human_samples_study'
)

annotated_seurat <- CyteTypeR(
  obj = seurat_obj,
  prepped_data = prepped_data,
  study_context = "Human PBMC from a healthy donor",
  metadata = metadata
)
```

[Read the complete Seurat workflow](docs/get-started.md).


## What You Get

- **Annotations:** Cell type, subtype, and activation state for every cluster
- **Cell Ontology mapping:** Standardized CL IDs for comparison across studies
- **Confidence and quality control:** Confidence values, plus match scores against your existing labels
- **Supporting evidence:** Publications and condition-specific references behind each call

## Example Report

Each analysis generates an HTML report with annotation decisions, reviewer comments, supporting evidence, and an embedded chat interface connected to your expression data.

<img width="1000" alt="CyteType HTML report showing cell type annotations and marker genes" src="https://github.com/user-attachments/assets/e5373fdd-7173-42db-b863-76a1e8ecfe01" />

[View example report](https://cytetype.nygen.io/report/e70e2883-7713-4121-94f2-5b57eabd1468?v=260303)

## Benchmarks

Across PBMC, bone marrow, tumor microenvironment, and cross-species datasets, the multi-agent approach outperforms existing annotation methods:

| Compared with | Improvement |
|---------------|-------------|
| GPTCellType | +388% |
| CellTypist | +268% |
| SingleR | +101% |

Methods and full results are in the [preprint](https://www.biorxiv.org/content/10.1101/2025.11.06.686964v1). You can also [browse results on atlas-scale datasets](docs/examples.md).

## Resources

- 📖 [Get Started](docs/get-started.md): Prepare a Seurat object and run an annotation.
- ⚙️ [Configuration](docs/configurations.md): Configure API access, LLM providers, and runtime settings.
- 🧬 [Example Datasets and Reports](docs/examples.md): Browse atlas-scale analyses.
- 🐍 [Python client](https://github.com/NygenAnalytics/CyteType): Use CyteType with AnnData and Scanpy.
- 🎥 [Introduction video](https://vimeo.com/nygen/cytetype): Watch a quick overview of CyteType.
- 💬 [Discord community](https://discord.gg/V6QFM4AN): Ask questions and get support.

## Citation

> Ahuja G, Antill A, Su Y, Dall'Olio GM, Basnayake S, Karlsson G, Dhapola P. Multi-agent AI enables evidence-based cell annotation in single-cell transcriptomics. *bioRxiv* 2025. doi: [10.1101/2025.11.06.686964](https://www.biorxiv.org/content/10.1101/2025.11.06.686964v1)

```bibtex
@article{cytetype2025,
  title={Multi-agent AI enables evidence-based cell annotation in single-cell transcriptomics},
  author={Gautam Ahuja, Alex Antill, Yi Su, Giovanni Marco Dall'Olio, Sukhitha Basnayake, Göran Karlsson, Parashar Dhapola},
  journal={bioRxiv},
  year={2025},
  doi={10.1101/2025.11.06.686964},
  url={https://www.biorxiv.org/content/10.1101/2025.11.06.686964v1}
}
```

## License

CyteTypeR code and documentation are available under [CC BY-NC-SA 4.0](LICENSE.md).
For commercial licensing, contact [contact@nygen.io](mailto:contact@nygen.io).

CyteType API access is free for academic and non-commercial research. Commercial API use and higher-rate access require a separate license from Nygen Analytics AB. Contact [contact@nygen.io](mailto:contact@nygen.io).
