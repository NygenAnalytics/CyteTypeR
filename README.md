<h1 align="left">CyteTypeR</h1>
<h3 align="left">Agentic, Evidence-Based Cell Type Annotation for Single-Cell RNA-seq in R</h3>

<p align="left">
  <a href="https://github.com/NygenAnalytics/CyteTypeR/actions/workflows/R-CMD-check.yaml">
    <img src="https://github.com/NygenAnalytics/CyteTypeR/actions/workflows/R-CMD-check.yaml/badge.svg" alt="R-CMD-check">
  </a>
  <img src="https://img.shields.io/badge/R-%E2%89%A5%204.2.0-blue.svg" alt="R Version">
  <a href="LICENSE.md">
    <img src="https://img.shields.io/badge/License-BSD%203--Clause-blue.svg" alt="License: BSD 3-Clause">
  </a>
</p>

**CyteTypeR** is an end-to-end cell type annotation system for **single-cell RNA sequencing (scRNA-seq)**, designed for repeatable analysis pipelines rather than one-off prompting. It combines cluster-level marker genes, expression context, study metadata, literature retrieval, ontology mapping, and a dedicated review step in a structured workflow that operates directly on Seurat objects.

For AnnData workflows, use [CyteType](https://github.com/NygenAnalytics/CyteType).

> [!IMPORTANT]
> Already installed CyteTypeR? Open R and run `devtools::install_github("NygenAnalytics/CyteTypeR", force = TRUE)` to update to the latest version before you start.
>
> CyteType requires an API key. Use is free for academic and non-commercial research. Commercial use requires a [license](#license).

## Why CyteTypeR?

Manual cell type annotation takes weeks and varies between experts. CyteTypeR delivers consistent, expert-level annotations in minutes using a multi-agent AI system where specialized agents collaborate on marker analysis, literature evidence, and Cell Ontology mapping.

## Quick Start

### 1. Install

```R
install.packages("devtools")
devtools::install_github("NygenAnalytics/CyteTypeR")
```

CyteTypeR needs R 4.2 or newer. See [Installation](https://docs.nygen.io/CyteType/getting-started/installation/) for the dependencies and for updating.

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

See [Sign in and API keys](https://docs.nygen.io/CyteType/getting-started/sign-in/) for remote servers and automation.

### 2.1. (Optional) Install and use the CLI

Install the `cytetyper` command into the active R environment:

```R
InstallCyteTypeRCli()
```

Then authenticate directly from the terminal:

```sh
cytetyper setup
cytetyper login        # save an existing key; it can also be piped in
cytetyper dashboard
cytetyper view <JOB_ID>
cytetyper logout
```

See the [command-line reference](https://docs.nygen.io/CyteType/reference/cli/).

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

Every cell gets its cluster's label, Cell Ontology term, cell state and reviewer confidence as columns of `annotated_seurat@meta.data`, and `annotated_seurat@misc$cytetype_results` has one row per cluster with the evidence behind each label.

[Read the complete Seurat workflow](https://docs.nygen.io/CyteType/getting-started/quickstart-r/).


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

- 📖 [Documentation](https://docs.nygen.io/CyteType/): Guides and references for the R and Python clients.
- 🚀 [Quickstart (R)](https://docs.nygen.io/CyteType/getting-started/quickstart-r/): Prepare a Seurat object and run an annotation.
- 🧪 [Prepare your data](https://docs.nygen.io/CyteType/guides/prepare-data/): What CyteTypeR needs from your Seurat object and marker table.
- ⚙️ [Run, monitor and rerun jobs](https://docs.nygen.io/CyteType/guides/running-jobs/) and [use your own LLM](https://docs.nygen.io/CyteType/guides/llm-configuration/): Runtime settings and LLM providers.
- 📋 [Results in AnnData and Seurat](https://docs.nygen.io/CyteType/results/results-in-your-data/): The columns and tables CyteTypeR adds to your object.
- 📚 [R function reference](https://docs.nygen.io/CyteType/reference/r-api/): Every exported function and its arguments.
- 🛠️ [Troubleshooting](https://docs.nygen.io/CyteType/support/troubleshooting/): Fixes for sign-in, data preparation, upload and job problems.
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

The CyteTypeR package is open source software released under the [BSD 3-Clause License](LICENSE.md) and maintained by [Nygen](https://nygen.io).

### CyteType service terms

Annotation runs on the CyteType service, which requires an API key.

- **Academic and non-commercial use** is free within the [usage limits](https://docs.nygen.io/CyteType/platform/limits/). Data generated under academic use is licensed under [CC BY-NC-SA 4.0](https://creativecommons.org/licenses/by-nc-sa/4.0/).
- **Commercial use and higher limits** require a commercial license from Nygen Analytics AB, which includes commercial rights to the generated data.

For licensing, contact [contact@nygen.io](mailto:contact@nygen.io).
