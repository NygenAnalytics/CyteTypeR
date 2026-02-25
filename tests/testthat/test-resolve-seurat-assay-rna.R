test_that(".resolve_seurat_assay_rna returns 'RNA' when RNA is default assay", {
  skip_if_not_installed("Seurat")
  counts <- Matrix::sparseMatrix(
    i = c(1L, 2L), j = c(1L, 2L), x = c(1.0, 1.0),
    dims = c(2L, 2L), dimnames = list(c("g1", "g2"), c("c1", "c2"))
  )
  obj <- suppressWarnings(Seurat::CreateSeuratObject(counts = counts, assay = "RNA"))
  result <- CyteTypeR:::.resolve_seurat_assay_rna(obj)
  expect_identical(result, "RNA")
})

test_that(".resolve_seurat_assay_rna warns when DefaultAssay is not RNA but RNA exists", {
  skip_if_not_installed("Seurat")
  counts <- Matrix::sparseMatrix(
    i = c(1L, 2L), j = c(1L, 2L), x = c(1.0, 1.0),
    dims = c(2L, 2L), dimnames = list(c("g1", "g2"), c("c1", "c2"))
  )
  obj <- suppressWarnings(Seurat::CreateSeuratObject(counts = counts, assay = "RNA"))
  obj[["SCT"]] <- obj[["RNA"]]
  Seurat::DefaultAssay(obj) <- "SCT"

  expect_warning(
    result <- CyteTypeR:::.resolve_seurat_assay_rna(obj),
    "DefaultAssay.*SCT.*Using assay='RNA'"
  )
  expect_identical(result, "RNA")
})

test_that(".resolve_seurat_assay_rna errors when no RNA assay exists", {
  skip_if_not_installed("Seurat")
  counts <- Matrix::sparseMatrix(
    i = c(1L, 2L), j = c(1L, 2L), x = c(1.0, 1.0),
    dims = c(2L, 2L), dimnames = list(c("g1", "g2"), c("c1", "c2"))
  )
  obj <- suppressWarnings(Seurat::CreateSeuratObject(counts = counts, assay = "SCT"))

  expect_error(
    CyteTypeR:::.resolve_seurat_assay_rna(obj),
    "requires an assay named 'RNA'.*Available assays: SCT"
  )
})

test_that(".resolve_seurat_assay_rna errors when object has no assays", {
  skip_if_not_installed("Seurat")
  counts <- Matrix::sparseMatrix(
    i = c(1L, 2L), j = c(1L, 2L), x = c(1.0, 1.0),
    dims = c(2L, 2L), dimnames = list(c("g1", "g2"), c("c1", "c2"))
  )
  obj <- suppressWarnings(Seurat::CreateSeuratObject(counts = counts, assay = "RNA"))
  obj@assays <- list()

  expect_error(
    CyteTypeR:::.resolve_seurat_assay_rna(obj),
    "No assays found"
  )
})

test_that(".resolve_seurat_assay_rna does not warn when DefaultAssay is already RNA", {
  skip_if_not_installed("Seurat")
  counts <- Matrix::sparseMatrix(
    i = c(1L, 2L), j = c(1L, 2L), x = c(1.0, 1.0),
    dims = c(2L, 2L), dimnames = list(c("g1", "g2"), c("c1", "c2"))
  )
  obj <- suppressWarnings(Seurat::CreateSeuratObject(counts = counts, assay = "RNA"))

  expect_silent(
    result <- CyteTypeR:::.resolve_seurat_assay_rna(obj)
  )
  expect_identical(result, "RNA")
})

test_that(".resolve_seurat_assay_rna handles DefaultAssay error gracefully", {
  skip_if_not_installed("Seurat")
  counts <- Matrix::sparseMatrix(
    i = c(1L, 2L), j = c(1L, 2L), x = c(1.0, 1.0),
    dims = c(2L, 2L), dimnames = list(c("g1", "g2"), c("c1", "c2"))
  )
  obj <- suppressWarnings(Seurat::CreateSeuratObject(counts = counts, assay = "RNA"))

  local_mocked_bindings(
    DefaultAssay = function(...) stop("broken"),
    .package = "Seurat"
  )

  result <- CyteTypeR:::.resolve_seurat_assay_rna(obj)
  expect_identical(result, "RNA")
})
