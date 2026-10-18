#' CyteTypeR: CyteType for R
#'
#' @description
#' CyteTypeR is the R client of CyteType, an agentic, evidence-based cell type
#' annotation service for single-cell RNA-seq data. It prepares a clustered
#' Seurat object and its marker genes, submits an annotation job to the
#' CyteType API and adds the results to the object: a cell type label, a Cell
#' Ontology term, a cell state and the reviewer's confidence for every
#' cluster. Every job also produces an interactive report.
#'
#' @section Workflow:
#' 1. Sign in once per computer with [SetupCyteTypeR()], or save an API key
#'    you already have with [LoginCyteTypeR()].
#' 2. Run [PrepareCyteTypeR()] on a normalized, clustered Seurat object and the
#'    marker table from `Seurat::FindAllMarkers()`.
#' 3. Run [CyteTypeR()] to upload the data, submit the job, wait for it and add
#'    the results to the object.
#' 4. Use [GetResults()] to read the results again, or to fetch the results of
#'    a job by its ID, and [CleanUpArtifacts()] to delete the uploaded files.
#'
#' [OpenCyteTypeDashboard()] and [ViewCyteTypeJob()] open the dashboard and a
#' job's report. [InstallCyteTypeRCli()] installs the `cytetyper` terminal
#' command, which signs in and opens reports from a shell.
#'
#' @section Documentation:
#' The CyteType documentation at <https://docs.nygen.io/CyteType/> covers both
#' the R and the Python client: installation, sign-in, the R quickstart, the
#' results, the report and the usage limits.
#'
#' @section License:
#' The package code is released under the BSD 3-Clause License. Annotation runs
#' on the CyteType service, which requires an API key: academic and
#' non-commercial use is free within the usage limits, and commercial use
#' requires a license from Nygen Analytics AB (<contact@nygen.io>).
#'
#' @keywords internal
"_PACKAGE"
