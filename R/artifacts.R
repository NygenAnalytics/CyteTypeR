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
      rhdf5::h5writeDataset(vec, h5loc = fid, name = col_path, level = 5L)
    } else if (is.numeric(vec)) {
      rhdf5::h5writeDataset(as.double(vec), h5loc = fid, name = col_path, level = 5L)
    } else {
      rhdf5::h5writeDataset(.as_string_values(vec), h5loc = fid, name = col_path, level = 5L)
    }
  }
  invisible(NULL)
}

.save_vars_h5 <- function(out_file, mat, feature_df = NULL, feature_names = NULL) {
  if (!requireNamespace("rhdf5", quietly = TRUE)) {
    stop("Package 'rhdf5' is required to build vars.h5. Install with: BiocManager::install('rhdf5')")
  }

  m <- as(mat, "CsparseMatrix")
  n_obs <- nrow(m)
  n_vars <- ncol(m)

  if (file.exists(out_file) && !file.remove(out_file)) {
    stop("Could not remove existing file: ", out_file)
  }

  rhdf5::h5createFile(out_file)
  fid <- rhdf5::H5Fopen(out_file, flags = "H5F_ACC_RDWR")
  on.exit(rhdf5::H5Fclose(fid), add = TRUE)

  rhdf5::H5Gcreate(fid, "vars")
  gid <- rhdf5::H5Gopen(fid, "vars")
  on.exit(rhdf5::H5Gclose(gid), add = TRUE)

  rhdf5::h5writeAttribute(as.integer(n_obs), h5obj = gid, name = "n_obs")
  rhdf5::h5writeAttribute(as.integer(n_vars), h5obj = gid, name = "n_vars")
  rhdf5::h5writeDataset(as.integer(m@i), h5loc = fid, name = "vars/indices", level = 5L)
  rhdf5::h5writeDataset(as.double(m@x),  h5loc = fid, name = "vars/data",    level = 5L)
  rhdf5::h5writeDataset(as.double(m@p),  h5loc = fid, name = "vars/indptr",  level = 5L)

  if (!is.null(feature_df)) {
    .write_var_metadata(fid, n_cols = n_vars, feature_df = feature_df, feature_names = feature_names)
  }

  invisible(out_file)
}

.save_obs_duckdb <- function(out_file, obs_df, table_name = "obs",
                             threads = "4", memory_limit = "4GB", temp_directory = "tmp/duckdb") {
  if (!requireNamespace("duckdb", quietly = TRUE)) {
    stop("Package 'duckdb' is required to build obs.duckdb. Install with: install.packages('duckdb')")
  }
  if (!grepl("^[A-Za-z_][A-Za-z0-9_]*$", table_name)) {
    stop("Invalid table_name. Use letters, numbers, and underscores only.")
  }
  if (file.exists(out_file)) file.remove(out_file)
  config <- list(threads = as.character(threads), memory_limit = memory_limit, temp_directory = temp_directory)
  con <- duckdb::dbConnect(duckdb::duckdb(), out_file, config = config)
  on.exit(duckdb::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  duckdb::dbWriteTable(con, table_name, as.data.frame(obs_df), overwrite = TRUE)
  invisible(out_file)
}
