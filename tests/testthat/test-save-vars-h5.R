test_that(".save_vars_h5 creates file with vars structure", {
  skip_if_not_installed("rhdf5")
  skip_if_not_installed("rhdf5filters")
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
  attrs <- rhdf5::h5readAttributes(out, "vars")
  expect_equal(as.integer(attrs[["n_obs"]]), 4L)
  expect_equal(as.integer(attrs[["n_vars"]]), 3L)
  expect_true(length(rhdf5::h5read(out, "vars/indptr")) == 4L)
})

test_that(".save_vars_h5 round-trip preserves sparse structure", {
  skip_if_not_installed("rhdf5")
  skip_if_not_installed("rhdf5filters")
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
  expect_equal(as.numeric(indptr[1]), 0)
  expect_equal(sum(m_csc@x), sum(data))
})

test_that(".save_vars_h5 writes feature metadata when feature_df provided", {
  skip_if_not_installed("rhdf5")
  skip_if_not_installed("rhdf5filters")
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
  skip_if_not_installed("rhdf5filters")
  mat <- Matrix::sparseMatrix(i = 1L, j = 1L, x = 1.0, dims = c(1L, 2L))
  feature_df <- data.frame(x = 1:2, row.names = c("a", "b"))
  out <- tempfile(fileext = ".h5")
  on.exit(if (file.exists(out)) unlink(out))
  CyteTypeR:::.save_vars_h5(out, mat, feature_df = feature_df, feature_names = c("GeneA", "GeneB"))
  expect_identical(as.character(rhdf5::h5read(out, "info/var/var_names")), c("GeneA", "GeneB"))
})

test_that(".save_vars_h5 attrs are on /vars group, not root", {
  skip_if_not_installed("rhdf5")
  skip_if_not_installed("rhdf5filters")
  mat <- Matrix::sparseMatrix(i = 1L, j = 1L, x = 1.0, dims = c(2L, 2L))
  out <- tempfile(fileext = ".h5")
  on.exit(if (file.exists(out)) unlink(out))
  CyteTypeR:::.save_vars_h5(out, mat)
  root_attrs <- rhdf5::h5readAttributes(out, "/")
  vars_attrs <- rhdf5::h5readAttributes(out, "vars")
  expect_false("vars/n_obs" %in% names(root_attrs))
  expect_false("vars/n_vars" %in% names(root_attrs))
  expect_true("n_obs" %in% names(vars_attrs))
  expect_true("n_vars" %in% names(vars_attrs))
  expect_equal(as.integer(vars_attrs[["n_obs"]]), 2L)
  expect_equal(as.integer(vars_attrs[["n_vars"]]), 2L)
})

test_that(".save_vars_h5 uses BLOSC LZ4 compression on datasets", {
  skip_if_not_installed("rhdf5")
  skip_if_not_installed("rhdf5filters")
  mat <- Matrix::sparseMatrix(
    i = c(1L, 2L, 1L),
    j = c(1L, 1L, 2L),
    x = c(1.0, 2.0, 0.5),
    dims = c(3L, 2L)
  )
  out <- tempfile(fileext = ".h5")
  on.exit(if (file.exists(out)) unlink(out))
  CyteTypeR:::.save_vars_h5(out, mat)

  fid <- rhdf5::H5Fopen(out, flags = "H5F_ACC_RDONLY")
  on.exit(rhdf5::H5Fclose(fid), add = TRUE)

  blosc_filter_id <- 32001L
  for (ds_name in c("vars/indices", "vars/data", "vars/indptr")) {
    did <- rhdf5::H5Dopen(fid, ds_name)
    plist <- rhdf5::H5Dget_create_plist(did)
    n_filters <- rhdf5::H5Pget_nfilters(plist)
    expect_gte(n_filters, 1L, label = paste(ds_name, "should have at least one filter"))
    filter_info <- rhdf5::H5Pget_filter(plist, 1L)
    expect_equal(filter_info[[1]], blosc_filter_id,
                 label = paste(ds_name, "filter should be BLOSC (id 32001)"))
    rhdf5::H5Pclose(plist)
    rhdf5::H5Dclose(did)
  }
})

test_that(".save_vars_h5 indptr length equals n_vars + 1", {
  skip_if_not_installed("rhdf5")
  skip_if_not_installed("rhdf5filters")
  mat <- Matrix::sparseMatrix(
    i = c(1L, 2L, 3L, 1L),
    j = c(1L, 2L, 3L, 4L),
    x = c(1.0, 2.0, 3.0, 4.0),
    dims = c(5L, 4L)
  )
  out <- tempfile(fileext = ".h5")
  on.exit(if (file.exists(out)) unlink(out))
  CyteTypeR:::.save_vars_h5(out, mat)
  indptr <- rhdf5::h5read(out, "vars/indptr")
  attrs <- rhdf5::h5readAttributes(out, "vars")
  expect_length(indptr, as.integer(attrs[["n_vars"]]) + 1L)
})

test_that(".save_vars_h5 indices are in [0, n_obs)", {
  skip_if_not_installed("rhdf5")
  skip_if_not_installed("rhdf5filters")
  mat <- Matrix::sparseMatrix(
    i = c(1L, 3L, 5L),
    j = c(1L, 2L, 3L),
    x = c(1.0, 2.0, 3.0),
    dims = c(5L, 3L)
  )
  out <- tempfile(fileext = ".h5")
  on.exit(if (file.exists(out)) unlink(out))
  CyteTypeR:::.save_vars_h5(out, mat)
  indices <- rhdf5::h5read(out, "vars/indices")
  attrs <- rhdf5::h5readAttributes(out, "vars")
  n_obs <- as.integer(attrs[["n_obs"]])
  expect_true(all(indices >= 0L))
  expect_true(all(indices < n_obs))
})

test_that(".save_vars_h5 writes raw group in CSR format when raw_mat provided", {
  skip_if_not_installed("rhdf5")
  skip_if_not_installed("rhdf5filters")
  mat <- Matrix::sparseMatrix(
    i = c(1L, 2L, 3L),
    j = c(1L, 2L, 3L),
    x = c(1.0, 2.0, 3.0),
    dims = c(4L, 3L)
  )
  raw <- Matrix::sparseMatrix(
    i = c(1L, 2L, 3L, 1L),
    j = c(1L, 2L, 3L, 4L),
    x = c(10, 20, 30, 5),
    dims = c(3L, 4L)
  )
  out <- tempfile(fileext = ".h5")
  on.exit(if (file.exists(out)) unlink(out))
  CyteTypeR:::.save_vars_h5(out, mat, raw_mat = raw)

  raw_attrs <- rhdf5::h5readAttributes(out, "raw")
  expect_equal(as.integer(raw_attrs[["n_obs"]]), 4L)
  expect_equal(as.integer(raw_attrs[["n_vars"]]), 3L)

  raw_indptr <- rhdf5::h5read(out, "raw/indptr")
  expect_length(raw_indptr, 4L + 1L)
  expect_equal(as.numeric(raw_indptr[1]), 0)

  raw_indices <- rhdf5::h5read(out, "raw/indices")
  expect_true(all(raw_indices >= 0L))
  raw_data <- rhdf5::h5read(out, "raw/data")
  expect_equal(length(raw_indices), length(raw_data))
  expect_true(is.integer(raw_data))
})

test_that(".save_vars_h5 omits raw group when raw_mat is NULL", {
  skip_if_not_installed("rhdf5")
  skip_if_not_installed("rhdf5filters")
  mat <- Matrix::sparseMatrix(i = 1L, j = 1L, x = 1.0, dims = c(2L, 2L))
  out <- tempfile(fileext = ".h5")
  on.exit(if (file.exists(out)) unlink(out))
  CyteTypeR:::.save_vars_h5(out, mat)
  contents <- rhdf5::h5ls(out)
  expect_false("raw" %in% contents$name)
})

test_that(".save_vars_h5 fails gracefully when rhdf5filters is missing", {
  skip_if_not_installed("rhdf5")
  skip("Cannot mock base::requireNamespace; rhdf5filters guard is validated manually")
})
