#' @importFrom methods as
#'
# Sanitize column name for HDF5 dataset and ensure unique (mirrors Python _safe_column_dataset_name).
.safe_column_dataset_name <- function(source_name, column_index, existing_names) {
  base <- gsub("[^A-Za-z0-9_.-]", "_", source_name)
  base <- trimws(base)
  base <- sub("^_+|_+$", "", base)
  if (!nzchar(base)) base <- paste0("column_", column_index)
  candidate <- base
  suffix <- 1L
  while (candidate %in% existing_names) {
    candidate <- paste0(base, "_", suffix)
    suffix <- suffix + 1L
  }
  return(candidate)
}

# Coerce to character, NA -> "" (mirrors Python _as_string_values).
.as_string_values <- function(x) {
  y <- as.character(x)
  y[is.na(y)] <- ""
  return(y)
}

# Write optional var (feature) metadata under info/var (mirrors Python _write_var_metadata).
.write_var_metadata <- function(fid, n_cols, feature_df, feature_names) {
  if (nrow(feature_df) != n_cols) {
    stop("feature_df row count (", nrow(feature_df), ") does not match matrix columns (", n_cols, ").")
  }
  names_source <- if (!is.null(feature_names)) feature_names else rownames(feature_df)
  if (length(names_source) != n_cols) {
    stop("feature_names length (", length(names_source), ") does not match matrix columns (", n_cols, ").")
  }
  rhdf5::h5createGroup(fid, "info")
  rhdf5::h5createGroup(fid, "info/var")
  rhdf5::h5writeDataset(.as_string_values(names_source), h5loc = fid, name = "info/var/var_names", level = 5L)
  rhdf5::h5writeDataset(.as_string_values(rownames(feature_df)), h5loc = fid, name = "info/var/index", level = 5L)
  rhdf5::h5createGroup(fid, "info/var/columns")
  existing <- character(0)
  for (i in seq_len(ncol(feature_df))) {
    col_name <- names(feature_df)[i]
    dataset_name <- .safe_column_dataset_name(col_name, i - 1L, existing)
    existing <- c(existing, dataset_name)
    col_path <- paste0("info/var/columns/", dataset_name)
    vec <- feature_df[[i]]
    if (is.factor(vec)) vec <- as.character(vec)
    if (is.logical(vec)) {
      storage.mode(vec) <- "integer"
      if (any(is.na(vec))) vec[is.na(vec)] <- -1L
      rhdf5::h5writeDataset(vec, h5loc = fid, name = col_path)
    } else if (is.numeric(vec)) {
      rhdf5::h5writeDataset(as.double(vec), h5loc = fid, name = col_path)
    } else {
      rhdf5::h5writeDataset(.as_string_values(vec), h5loc = fid, name = col_path)
    }
  }
  invisible(NULL)
}

# Write a sparse matrix under a named HDF5 group.
# csr = FALSE (default): CSC — indptr over columns (genes), indices are row (cell) indices.
# csr = TRUE:            CSR — transposes internally so indptr is over rows (cells), indices are column (gene) indices.
.write_sparse_group <- function(fid, group, m, n_obs, col_batch, chunk_size, csr = FALSE) {
  if (csr) m <- as(Matrix::t(m), "CsparseMatrix")
  n_cols <- ncol(m)
  rhdf5::h5createGroup(fid, group)
  rhdf5::h5writeAttribute(as.integer(n_obs), h5obj = fid, name = "n_obs", h5loc = group, asScalar = TRUE)

  rhdf5::h5createDataset(fid, paste0(group, "/indices"), dims = 0L,
    maxdims = rhdf5::H5Sunlimited(), chunk = chunk_size,
    H5type = "H5T_NATIVE_INT32", filter = "BLOSC_LZ4")
  rhdf5::h5createDataset(fid, paste0(group, "/data"), dims = 0L,
    maxdims = rhdf5::H5Sunlimited(), chunk = chunk_size,
    H5type = "H5T_NATIVE_FLOAT", filter = "BLOSC_LZ4")

  indptr <- 0L
  current_size <- 0L
  starts <- seq(1L, n_cols, by = col_batch)
  for (start in starts) {
    end <- min(start + col_batch - 1L, n_cols)
    chunk <- as(m[, start:end, drop = FALSE], "CsparseMatrix")
    chunk_indices <- as.integer(chunk@i)
    chunk_data   <- as.numeric(chunk@x)
    chunk_nnz    <- length(chunk_indices)
    if (chunk_nnz > 0L) {
      rhdf5::h5set_extent(fid, paste0(group, "/indices"), current_size + chunk_nnz)
      rhdf5::h5writeDataset(chunk_indices, h5loc = fid, name = paste0(group, "/indices"),
        index = list((current_size + 1L):(current_size + chunk_nnz)))
      rhdf5::h5set_extent(fid, paste0(group, "/data"), current_size + chunk_nnz)
      rhdf5::h5writeDataset(chunk_data, h5loc = fid, name = paste0(group, "/data"),
        index = list((current_size + 1L):(current_size + chunk_nnz)))
      current_size <- current_size + chunk_nnz
    }
    new_indptr <- as.integer(chunk@p[-1L] + indptr[length(indptr)])
    indptr <- c(indptr, new_indptr)
  }

  rhdf5::h5createDataset(fid, paste0(group, "/indptr"), dims = length(indptr),
    H5type = "H5T_NATIVE_INT32", chunk = min(chunk_size, length(indptr)),
    filter = "BLOSC_LZ4")
  rhdf5::h5writeDataset(as.integer(indptr), h5loc = fid, name = paste0(group, "/indptr"))
  invisible(NULL)
}

.save_vars_h5 <- function(out_file, mat, raw_mat = NULL, feature_df = NULL, feature_names = NULL,
                          col_batch = NULL, min_chunk_size = 10000L) {
  n_obs <- nrow(mat)
  n_vars <- ncol(mat)

  if (!requireNamespace("rhdf5filters", quietly = TRUE)) {
    stop("Package 'rhdf5filters' is required to write vars.h5 with LZ4 compression.")
  }

  if (is.null(col_batch)) {
    col_batch <- max(1L, as.integer(100000000 / max(n_obs, 1)))
  }
  chunk_size <- max(1L, min(n_obs * 10L, min_chunk_size))

  if (file.exists(out_file) && !file.remove(out_file)) {
    stop("Could not remove existing file: ", out_file)
  }

  rhdf5::h5createFile(out_file)
  fid <- rhdf5::H5Fopen(out_file, flags = "H5F_ACC_RDWR")
  on.exit(rhdf5::H5Fclose(fid), add = TRUE)

  rhdf5::h5writeAttribute(as.integer(n_vars), h5obj = fid, name = "n_vars", h5loc = "/", asScalar = TRUE)

  .write_sparse_group(fid, "vars", mat, n_obs, col_batch, chunk_size)

  if (!is.null(raw_mat)) {
    .write_sparse_group(fid, "raw", raw_mat, n_obs, col_batch, chunk_size, csr = TRUE)
  }

  if (!is.null(feature_df)) {
    .write_var_metadata(fid, n_cols = n_vars, feature_df = feature_df, feature_names = feature_names)
  }

  invisible(out_file)
}

.save_obs_duckdb <- function(out_file, obs_df, coordinates = NULL, coordinates_key = NULL,
                             table_name = "obs",
                             threads = "4", memory_limit = "4GB", temp_directory = "tmp/duckdb") {
  if (!requireNamespace("duckdb", quietly = TRUE)) {
    stop("Package 'duckdb' is required to build obs.duckdb. Install with: install.packages('duckdb')")
  }
  if (!grepl("^[A-Za-z_][A-Za-z0-9_]*$", table_name)) {
    stop("Invalid table_name. Use letters, numbers, and underscores only.")
  }

  df <- as.data.frame(obs_df)

  if (!is.null(coordinates) && !is.null(coordinates_key)) {
    coords <- as.matrix(coordinates)
    if (ncol(coords) >= 2 && nrow(coords) == nrow(df)) {
      col1 <- paste0("__vis_coordinates_", coordinates_key, "_1")
      col2 <- paste0("__vis_coordinates_", coordinates_key, "_2")
      df[[col1]] <- as.numeric(coords[, 1])
      df[[col2]] <- as.numeric(coords[, 2])
    }
  }

  if (file.exists(out_file)) file.remove(out_file)
  config <- list(threads = as.character(threads), memory_limit = memory_limit, temp_directory = temp_directory)
  con <- duckdb::dbConnect(duckdb::duckdb(), out_file, config = config)
  on.exit(duckdb::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  duckdb::dbWriteTable(con, table_name, df, overwrite = TRUE)
  invisible(out_file)
}
