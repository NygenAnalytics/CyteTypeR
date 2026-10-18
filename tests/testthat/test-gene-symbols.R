# Gene ID detection, ported from _is_gene_id_like in the Python SDK.

test_that(".is_gene_id_like flags gene IDs", {
  ids <- c(
    "ENSG00000000003", "ENSG00000000003.14", "ENSMUSG00000000001", "ensg00000000003",
    "NM_001301", "NR_046018", "XM_011541", "XR_001737",
    "7157", "672", "11286", "0",
    "7157.0", "672.0", "11286.0", "0.0",
    "AFFY_HG_U133A.207163_S_AT", "ILLUMINA_HUMANHT_12_V4.ILMN_1762337",
    " ENSG00000000003 "
  )
  expect_true(all(CyteTypeR:::.is_gene_id_like(ids)))
})

test_that(".is_gene_id_like does not flag gene symbols or blanks", {
  values <- c("TSPAN6", "DPM1", "SCYL3", "TP53", "BRCA1", "CD8A", "MS4A1", "HLA-DRA",
              "", "   ", "7157.5", NA, "XENSG00000000003", "AC000061.1")
  expect_false(any(CyteTypeR:::.is_gene_id_like(values)))
})

test_that(".id_like_percentage matches the Python SDK", {
  pct <- CyteTypeR:::.id_like_percentage
  expect_identical(pct(c("TSPAN6", "DPM1", "SCYL3", "TP53", "BRCA1")), 0)
  expect_identical(pct(sprintf("ENSG%011d", 0:19)), 100)
  expect_identical(pct(c("7157.0", "672.0", "3845.0", "11286.0", "9952.0")), 100)
  expect_equal(pct(c("TSPAN6", "DPM1", "SCYL3", "7157.0", "672.0", "3845.0", "11286.0", "9952.0", "904.0", "405.0")), 70)
  expect_identical(pct(character(0)), 100)
})

test_that("the marker table check warns when the genes are IDs", {
  markers <- data.frame(
    cluster = rep(c("0", "1"), each = 5),
    gene = sprintf("ENSG%011d", 1:10),
    avg_log2FC = 1
  )

  output <- capture.output(
    CyteTypeR:::.validate_marker_table(markers, c("0", "1")),
    type = "message"
  )

  expect_true(any(grepl("100.0% of the genes in the marker table look like gene IDs", output, fixed = TRUE)))
})

test_that("the marker table check accepts gene symbols", {
  markers <- data.frame(
    cluster = rep(c("0", "1"), each = 5),
    gene = c("CD3E", "CD8A", "GZMK", "NKG7", "CCL5", "MS4A1", "CD79A", "CD19", "PAX5", "BANK1"),
    avg_log2FC = 1
  )

  output <- capture.output(
    CyteTypeR:::.validate_marker_table(markers, c("0", "1")),
    type = "message"
  )

  expect_false(any(grepl("look like gene IDs", output, fixed = TRUE)))
  expect_true(any(grepl("Found gene symbols", output, fixed = TRUE)))
})

test_that("the gene symbol check warns for versioned Ensembl row names", {
  counts <- Matrix::sparseMatrix(
    i = 1:6, j = 1:6, x = 1, dims = c(6L, 6L),
    dimnames = list(sprintf("ENSG%011d.%d", 1:6, 2L), paste0("c", 1:6))
  )
  obj <- suppressWarnings(SeuratObject::CreateSeuratObject(counts = counts))

  output <- capture.output(
    suppressMessages(CyteTypeR:::.validate_gene_symbols(obj, "gene_symbols")),
    type = "message"
  )

  expect_true(any(grepl("appear to be gene IDs", output, fixed = TRUE)))
})

test_that("the gene symbol check reads the named feature column", {
  counts <- Matrix::sparseMatrix(
    i = 1:4, j = 1:4, x = 1, dims = c(4L, 4L),
    dimnames = list(sprintf("ENSG%011d", 1:4), paste0("c", 1:4))
  )
  obj <- suppressWarnings(SeuratObject::CreateSeuratObject(counts = counts))
  symbols <- stats::setNames(c("CD3E", "CD8A", "MS4A1", "CD19"), rownames(obj))
  obj[["RNA"]] <- SeuratObject::AddMetaData(obj[["RNA"]], symbols, col.name = "gene_symbols")

  output <- capture.output(
    expect_no_message(CyteTypeR:::.validate_gene_symbols(obj, "gene_symbols")),
    type = "message"
  )

  expect_false(any(grepl("gene IDs", output, fixed = TRUE)))
})
