# PrepareCyteTypeR() on small, real Seurat objects.

prepare_quietly <- function(...) {
  suppressMessages(PrepareCyteTypeR(...))
}

vars_h5_contents <- function(path) {
  list(
    vars_attrs = rhdf5::h5readAttributes(path, "vars"),
    vars_data = as.numeric(rhdf5::h5read(path, "vars/data")),
    vars_indices = as.integer(rhdf5::h5read(path, "vars/indices")),
    raw_data = if ("raw" %in% rhdf5::h5ls(path)$name) as.integer(rhdf5::h5read(path, "raw/data")) else NULL,
    var_names = as.character(rhdf5::h5read(path, "info/var/var_names"))
  )
}

test_that("PrepareCyteTypeR() builds the payload and both files", {
  skip_if_not_installed("rhdf5filters")
  local_temp_workdir()
  obj <- make_seurat_fixture()

  prepped <- prepare_quietly(obj, make_marker_table())

  expect_true(prepped$build_succeeded)
  expect_identical(prepped$clusterLabels, list(`1` = "0", `2` = "1", `3` = "2"))
  expect_identical(names(prepped$markerGenes), c("1", "2", "3"))
  expect_identical(prepped$coordinates_key, "umap")
  expect_length(prepped$visualizationData$coordinates, 90L)
  expect_length(prepped$expressionData, 60L)
  expect_setequal(names(prepped$expressionData[["GENE01"]]), c("1", "2", "3"))
  expect_true(file.exists("vars.h5"))
  expect_true(file.exists("obs.duckdb"))

  contents <- vars_h5_contents("vars.h5")
  expect_identical(as.integer(contents$vars_attrs$n_obs), 90L)
  expect_identical(as.integer(contents$vars_attrs$n_vars), 60L)
  expect_identical(contents$var_names, rownames(obj))
  # The counts are whole numbers, so the raw group is written.
  expect_false(is.null(contents$raw_data))
})

test_that("Seurat v5 split layers are joined on a copy", {
  skip_if_not_installed("rhdf5filters")
  local_temp_workdir()
  obj <- make_seurat_fixture()
  markers <- make_marker_table()
  joined <- prepare_quietly(obj, markers, vars_h5_path = "joined.h5", obs_duckdb_path = "joined.duckdb")

  split_obj <- obj
  split_obj[["RNA"]] <- split(split_obj[["RNA"]], f = split_obj$batch)
  layers_before <- SeuratObject::Layers(split_obj[["RNA"]])
  expect_true(length(layers_before) > 2L)

  prepped <- prepare_quietly(split_obj, markers, vars_h5_path = "split.h5", obs_duckdb_path = "split.duckdb")

  expect_true(prepped$build_succeeded)
  expect_identical(prepped$expressionData, joined$expressionData)
  expect_identical(vars_h5_contents("split.h5"), vars_h5_contents("joined.h5"))
  # The caller's object keeps its split layers.
  expect_identical(SeuratObject::Layers(split_obj[["RNA"]]), layers_before)
})

test_that(".get_rna_layer returns the joined layer in the object's cell order", {
  obj <- make_seurat_fixture()
  expected <- SeuratObject::LayerData(obj, assay = "RNA", layer = "data")
  obj[["RNA"]] <- split(obj[["RNA"]], f = obj$batch)

  data <- CyteTypeR:::.get_rna_layer(obj, "data")
  counts <- CyteTypeR:::.get_rna_layer(obj, "counts")

  expect_identical(colnames(data), colnames(obj))
  expect_equal(as.matrix(data), as.matrix(expected))
  expect_identical(dim(counts), dim(expected))
})

test_that("a default assay other than RNA uses the RNA genes", {
  skip_if_not_installed("rhdf5filters")
  local_temp_workdir()
  obj <- make_seurat_fixture()
  adt <- Matrix::sparseMatrix(
    i = rep(1:5, length.out = ncol(obj)), j = seq_len(ncol(obj)), x = 3,
    dims = c(5L, ncol(obj)),
    dimnames = list(paste0("ADT", 1:5), colnames(obj))
  )
  obj[["ADT"]] <- SeuratObject::CreateAssay5Object(counts = adt)
  SeuratObject::DefaultAssay(obj) <- "ADT"

  warnings <- character()
  prepped <- withCallingHandlers(
    prepare_quietly(obj, make_marker_table()),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  expect_true(prepped$build_succeeded)
  expect_identical(names(prepped$expressionData), rownames(obj[["RNA"]]))
  expect_identical(vars_h5_contents("vars.h5")$var_names, rownames(obj[["RNA"]]))
  # One warning that RNA is used, not one per step.
  expect_identical(sum(grepl("Using assay='RNA'", warnings)), 1L)
})

test_that("NA cluster labels stop early with a clear error", {
  local_temp_workdir()
  obj <- make_seurat_fixture()
  obj$seurat_clusters[c(3, 10)] <- NA

  expect_error(
    prepare_quietly(obj, make_marker_table()),
    "2 cells \\(2.2%\\) have NA values in 'seurat_clusters'"
  )
  expect_false(file.exists("vars.h5"))

  obj$seurat_clusters <- factor(rep(NA_character_, ncol(obj)))
  expect_error(prepare_quietly(obj, make_marker_table()), "All 90 cells have NA values")
})

test_that("a missing duckdb stops before vars.h5 is written", {
  local_temp_workdir()
  testthat::local_mocked_bindings(
    .has_namespace = function(package) !identical(package, "duckdb"),
    .package = "CyteTypeR"
  )

  expect_error(prepare_quietly(make_seurat_fixture(), make_marker_table()), "Package 'duckdb' is required")
  expect_false(file.exists("vars.h5"))
})

test_that("visualization sampling leaves the caller's random number stream alone", {
  obj <- make_seurat_fixture()
  cluster_map <- c(`0` = "1", `1` = "2", `2` = "3")
  withr::local_seed(123)
  before <- get(".Random.seed", envir = globalenv())

  first <- CyteTypeR:::.sample_visualization_data(obj, "seurat_clusters", "umap", cluster_map, max_cells_per_group = 10)

  expect_identical(get(".Random.seed", envir = globalenv()), before)
  second <- CyteTypeR:::.sample_visualization_data(obj, "seurat_clusters", "umap", cluster_map, max_cells_per_group = 10)
  expect_identical(first, second)
  expect_length(first$coordinates, 30L)
})

test_that("visualization sampling creates no seed when the session has none", {
  obj <- make_seurat_fixture()
  cluster_map <- c(`0` = "1", `1` = "2", `2` = "3")
  withr::local_preserve_seed()
  if (exists(".Random.seed", envir = globalenv())) {
    rm(".Random.seed", envir = globalenv())
  }

  CyteTypeR:::.sample_visualization_data(obj, "seurat_clusters", "umap", cluster_map, max_cells_per_group = 10)

  expect_false(exists(".Random.seed", envir = globalenv()))
})

test_that("a cluster with a single cell keeps that cell", {
  obj <- make_seurat_fixture()
  clusters <- as.character(obj$seurat_clusters)
  clusters[50] <- "3"
  obj$seurat_clusters <- factor(clusters)
  cluster_map <- c(`0` = "1", `1` = "2", `2` = "3", `3` = "4")

  sampled <- CyteTypeR:::.sample_visualization_data(obj, "seurat_clusters", "umap", cluster_map, max_cells_per_group = 5)

  single <- which(sampled$clusters == "4")
  expect_length(single, 1L)
  embedding <- SeuratObject::Embeddings(obj, "umap")
  expect_equal(sampled$coordinates[[single]], as.numeric(embedding[50, ]))
})

test_that("coordinates fall back to umap, tsne and pca", {
  obj <- make_seurat_fixture()
  embedding <- SeuratObject::Embeddings(obj, "umap")
  colnames(embedding) <- c("tSNE_1", "tSNE_2")
  obj[["tsne"]] <- SeuratObject::CreateDimReducObject(embeddings = embedding, key = "tSNE_", assay = "RNA")

  expect_identical(suppressMessages(CyteTypeR:::.resolve_coordinates_key(obj, "UMAP")), "umap")
  expect_identical(CyteTypeR:::.resolve_coordinates_key(obj, "tsne"), "tsne")

  obj[["umap"]] <- NULL
  expect_identical(CyteTypeR:::.resolve_coordinates_key(obj, "umap"), "tsne")

  obj[["tsne"]] <- NULL
  one_dim <- embedding[, 1, drop = FALSE]
  colnames(one_dim) <- "PC_1"
  obj[["pca"]] <- SeuratObject::CreateDimReducObject(embeddings = one_dim, key = "PC_", assay = "RNA")
  expect_null(CyteTypeR:::.resolve_coordinates_key(obj, "umap"))
})

test_that("PrepareCyteTypeR() runs without coordinates", {
  skip_if_not_installed("rhdf5filters")
  local_temp_workdir()
  obj <- make_seurat_fixture()
  obj[["umap"]] <- NULL

  prepped <- prepare_quietly(obj, make_marker_table())

  expect_null(prepped$visualizationData)
  expect_true(prepped$build_succeeded)
})

test_that("factor clusters with unused levels map expression to the right IDs", {
  obj <- make_seurat_fixture()
  obj$seurat_clusters <- factor(as.character(obj$seurat_clusters), levels = c("0", "1", "unused", "2"))
  cluster_map <- c(`0` = "1", `1` = "2", `2` = "3")
  expr <- CyteTypeR:::.get_rna_layer(obj, "data")

  pcent <- suppressMessages(CyteTypeR:::.calculate_pcent(obj, "seurat_clusters", cluster_map, 25, expr = expr))

  cells_2 <- which(obj$seurat_clusters == "2")
  expected <- round(sum(expr["GENE05", cells_2] > 0) / length(cells_2) * 100, 2)
  expect_identical(pcent[["GENE05"]][["3"]], expected)
})

test_that("raw counts are written only when they are integer-valued", {
  skip_if_not_installed("rhdf5filters")
  mat <- Matrix::sparseMatrix(i = c(1L, 2L, 3L), j = c(1L, 2L, 3L), x = c(1, 2, 3), dims = c(4L, 3L))
  fractional <- Matrix::sparseMatrix(i = c(1L, 2L, 3L), j = c(1L, 2L, 3L), x = c(1.5, 2, 3), dims = c(3L, 4L))
  out <- tempfile(fileext = ".h5")
  on.exit(unlink(out), add = TRUE)

  CyteTypeR:::.save_vars_h5(out, mat, raw_mat = fractional)

  expect_false("raw" %in% rhdf5::h5ls(out)$name)
})

test_that(".is_integer_valued checks every value", {
  is_integer_valued <- CyteTypeR:::.is_integer_valued
  whole <- Matrix::sparseMatrix(i = 1:3, j = 1:3, x = c(1, 2, 300))
  expect_true(is_integer_valued(whole))
  expect_true(is_integer_valued(matrix(1:4, 2)))
  expect_false(is_integer_valued(Matrix::sparseMatrix(i = 1:3, j = 1:3, x = c(1, 2, 2.5))))
  expect_false(is_integer_valued(Matrix::sparseMatrix(i = 1:2, j = 1:2, x = c(1, Inf))))
  expect_false(is_integer_valued(NULL))
  # Checked in chunks: a fraction in the last chunk is found.
  x <- c(rep(1, 25), 0.5)
  expect_false(is_integer_valued(Matrix::sparseMatrix(i = seq_along(x), j = seq_along(x), x = x), chunk_size = 10))
})

test_that("obs.duckdb spills to a private temporary directory", {
  local_temp_workdir()
  before <- list.files(tempdir(), pattern = "^cytetype-duckdb-")
  obs <- data.frame(cluster = c("a", "b"), row.names = c("c1", "c2"))
  coords <- matrix(c(1, 2, 3, 4), ncol = 2, dimnames = list(c("c2", "c1"), NULL))

  CyteTypeR:::.save_obs_duckdb("obs.duckdb", obs, coordinates = coords, coordinates_key = "umap")

  expect_true(file.exists("obs.duckdb"))
  expect_false(dir.exists("tmp"))
  expect_identical(list.files(tempdir(), pattern = "^cytetype-duckdb-"), before)
  con <- duckdb::dbConnect(CyteTypeR:::.duckdb_driver(), "obs.duckdb", read_only = TRUE)
  on.exit(duckdb::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  query <- duckdb::dbSendQuery(con, "SELECT * FROM obs")
  table <- duckdb::dbFetch(query)
  duckdb::dbClearResult(query)
  # Coordinates are matched to the cells by name.
  expect_identical(table$`__vis_coordinates_umap_1`, c(2, 1))
})

test_that(".get_rna_layer reads Seurat v3/v4 assays from their slots", {
  withr::local_options(Seurat.object.assay.version = "v3")
  counts <- Matrix::sparseMatrix(
    i = c(1L, 2L, 3L), j = c(1L, 2L, 3L), x = c(2, 4, 6), dims = c(3L, 3L),
    dimnames = list(c("CD3E", "MS4A1", "NKG7"), c("c1", "c2", "c3"))
  )
  obj <- suppressWarnings(SeuratObject::CreateSeuratObject(counts = counts))
  obj <- Seurat::NormalizeData(obj, verbose = FALSE)
  expect_true(methods::is(obj[["RNA"]], "Assay"))

  expect_equal(as.matrix(CyteTypeR:::.get_rna_layer(obj, "counts")), as.matrix(counts))
  data <- CyteTypeR:::.get_rna_layer(obj, "data")
  expect_identical(dim(data), dim(counts))
  expect_false(isTRUE(all.equal(as.matrix(data), as.matrix(counts))))

  data_only <- suppressWarnings(SeuratObject::CreateSeuratObject(
    SeuratObject::CreateAssayObject(data = log1p(counts))
  ))
  expect_null(CyteTypeR:::.get_rna_layer(data_only, "counts"))
})

test_that("coordinates_key is NULL when no reduction was used", {
  skip_if_not_installed("rhdf5filters")
  local_temp_workdir()
  obj <- make_seurat_fixture()
  obj[["umap"]] <- NULL

  prepped <- prepare_quietly(obj, make_marker_table(), coordinates_key = "umap")

  expect_true("coordinates_key" %in% names(prepped))
  expect_null(prepped$coordinates_key)

  embedding <- SeuratObject::Embeddings(make_seurat_fixture(), "umap")
  colnames(embedding) <- c("tSNE_1", "tSNE_2")
  obj[["tsne"]] <- SeuratObject::CreateDimReducObject(embeddings = embedding, key = "tSNE_", assay = "RNA")
  prepped <- prepare_quietly(obj, make_marker_table(), coordinates_key = "umap")
  expect_identical(prepped$coordinates_key, "tsne")
})
