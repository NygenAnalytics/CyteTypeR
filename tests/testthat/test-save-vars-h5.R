test_that(".save_vars_h5 creates file with vars structure", {
  skip_if_not_installed("rhdf5")
  mat <- Matrix::sparseMatrix(
    i = c(1L, 2L, 1L, 3L),
    j = c(1L, 1L, 2L, 3L),
    x = c(1.0, 2.0, 0.5, 3.0),
    dims = c(4L, 3L)
  )
  out <- tempfile(fileext = ".h5")
  on.exit(if (file.exists(out)) unlink(out))
  CyteTypeR:::.save_vars_h5(out, mat)
  expect_true(file.exists(out))
  attrs <- rhdf5::h5readAttributes(out, "/")
  expect_equal(as.integer(attrs[["vars/n_obs"]]), 4L)
  expect_equal(as.integer(attrs[["vars/n_vars"]]), 3L)
  expect_true(length(rhdf5::h5read(out, "vars/indptr")) == 4L)
})

test_that(".save_vars_h5 round-trip preserves sparse structure", {
  skip_if_not_installed("rhdf5")
  mat <- Matrix::sparseMatrix(
    i = c(1L, 2L),
    j = c(1L, 2L),
    x = c(1.5, 2.5),
    dims = c(3L, 3L)
  )
  m_csc <- as(mat, "CsparseMatrix")
  out <- tempfile(fileext = ".h5")
  on.exit(if (file.exists(out)) unlink(out))
  CyteTypeR:::.save_vars_h5(out, mat)
  indices <- rhdf5::h5read(out, "vars/indices")
  data <- rhdf5::h5read(out, "vars/data")
  indptr <- rhdf5::h5read(out, "vars/indptr")
  expect_length(indptr, 4L)
  expect_identical(indptr[1], 0L)
  expect_equal(sum(m_csc@x), sum(data))
})

test_that(".save_vars_h5 writes feature metadata when feature_df provided", {
  skip_if_not_installed("rhdf5")
  mat <- Matrix::sparseMatrix(i = 1L, j = 1L, x = 1.0, dims = c(2L, 3L))
  feature_df <- data.frame(
    gene_id = c("ENS1", "ENS2", "ENS3"),
    n_cells = c(10L, 5L, 3L),
    row.names = c("GENE1", "GENE2", "GENE3")
  )
  out <- tempfile(fileext = ".h5")
  on.exit(if (file.exists(out)) unlink(out))
  CyteTypeR:::.save_vars_h5(out, mat, feature_df = feature_df)
  var_names <- as.character(rhdf5::h5read(out, "info/var/var_names"))
  expect_identical(var_names, c("GENE1", "GENE2", "GENE3"))
  expect_identical(as.character(rhdf5::h5read(out, "info/var/index")), c("GENE1", "GENE2", "GENE3"))
})

test_that(".save_vars_h5 uses feature_names when provided", {
  skip_if_not_installed("rhdf5")
  mat <- Matrix::sparseMatrix(i = 1L, j = 1L, x = 1.0, dims = c(1L, 2L))
  feature_df <- data.frame(x = 1:2, row.names = c("a", "b"))
  out <- tempfile(fileext = ".h5")
  on.exit(if (file.exists(out)) unlink(out))
  CyteTypeR:::.save_vars_h5(out, mat, feature_df = feature_df, feature_names = c("GeneA", "GeneB"))
  expect_identical(as.character(rhdf5::h5read(out, "info/var/var_names")), c("GeneA", "GeneB"))
})
