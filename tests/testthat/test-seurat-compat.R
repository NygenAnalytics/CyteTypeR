# Tests for Seurat v3/v4/v5 cross-version compatibility patterns used in
# .calculate_pcent (GetAssayData layer vs slot fallback) and
# .validate_gene_symbols (meta.features vs meta.data fallback).

# --- GetAssayData layer/slot fallback (used in .calculate_pcent and CyteTypeR) ---

test_that("GetAssayData layer= is tried first, slot= used as fallback", {
  skip_if_not_installed("Seurat")
  skip_if_not_installed("Matrix")

  dummy_mat <- Matrix::sparseMatrix(
    i = c(1L, 2L), j = c(1L, 1L), x = c(1.0, 2.0), dims = c(2L, 2L),
    dimnames = list(c("g1", "g2"), c("c1", "c2"))
  )

  call_log <- character(0)
  local_mocked_bindings(
    GetAssayData = function(object, ...) {
      args <- list(...)
      if ("layer" %in% names(args)) {
        call_log <<- c(call_log, "layer")
        stop("layer not supported")
      }
      if ("slot" %in% names(args)) {
        call_log <<- c(call_log, "slot")
        return(dummy_mat)
      }
      stop("unexpected")
    },
    .package = "Seurat"
  )

  result <- tryCatch(
    Seurat::GetAssayData(NULL, assay = "RNA", layer = "data"),
    error = function(e) Seurat::GetAssayData(NULL, assay = "RNA", slot = "data")
  )
  expect_equal(result, dummy_mat)
  expect_identical(call_log, c("layer", "slot"))
})

test_that("GetAssayData layer= succeeds without fallback on v5-like", {
  skip_if_not_installed("Seurat")
  skip_if_not_installed("Matrix")

  dummy_mat <- Matrix::sparseMatrix(
    i = 1L, j = 1L, x = 5.0, dims = c(2L, 2L),
    dimnames = list(c("g1", "g2"), c("c1", "c2"))
  )

  call_log <- character(0)
  local_mocked_bindings(
    GetAssayData = function(object, ...) {
      args <- list(...)
      if ("layer" %in% names(args)) {
        call_log <<- c(call_log, "layer")
        return(dummy_mat)
      }
      call_log <<- c(call_log, "other")
      stop("unexpected")
    },
    .package = "Seurat"
  )

  result <- tryCatch(
    Seurat::GetAssayData(NULL, assay = "RNA", layer = "data"),
    error = function(e) Seurat::GetAssayData(NULL, assay = "RNA", slot = "data")
  )
  expect_equal(result, dummy_mat)
  expect_identical(call_log, "layer")
})

# --- meta.features / meta.data fallback (used in .validate_gene_symbols) ---

test_that("feature metadata fallback: meta.features used when available", {
  mf <- data.frame(gene_symbols = c("TP53", "BRCA1"), row.names = c("g1", "g2"))
  mock_assay <- structure(list(), class = "Assay")
  attr(mock_assay, "meta.features") <- mf
  slot_fn <- function(x, name) {
    if (name == "meta.features") return(mf)
    stop("no such slot")
  }

  result <- tryCatch(
    slot_fn(mock_assay, "meta.features"),
    error = function(e) tryCatch(slot_fn(mock_assay, "meta.data"), error = function(e2) NULL)
  )
  expect_identical(result, mf)
})

test_that("feature metadata fallback: meta.data used when meta.features fails", {
  md <- data.frame(gene_symbols = c("TP53", "BRCA1"), row.names = c("g1", "g2"))
  slot_fn <- function(x, name) {
    if (name == "meta.features") stop("no meta.features")
    if (name == "meta.data") return(md)
    stop("no such slot")
  }

  result <- tryCatch(
    slot_fn(NULL, "meta.features"),
    error = function(e) tryCatch(slot_fn(NULL, "meta.data"), error = function(e2) NULL)
  )
  expect_identical(result, md)
})

test_that("feature metadata fallback: NULL when neither slot exists", {
  slot_fn <- function(x, name) stop(paste("no", name))

  result <- tryCatch(
    slot_fn(NULL, "meta.features"),
    error = function(e) tryCatch(slot_fn(NULL, "meta.data"), error = function(e2) NULL)
  )
  expect_null(result)
})
