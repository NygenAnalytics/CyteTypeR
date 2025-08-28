#'
#' @importFrom utils flush.console

# Visuals for cluster progress status

.display_cluster_status_cli <- function(cluster_status, job_id, is_final = FALSE, spinner_frame = 0) {
  if (length(cluster_status) == 0) return()

  # Status symbols and colors
  symbols <- list(
    completed = cli::symbol$tick,      # ✓
    processing = cli::symbol$circle_filled,  # ●
    pending = cli::symbol$circle,      # ○
    failed = cli::symbol$cross         # ✗
  )

  colors <- list(
    completed = crayon::green,
    processing = crayon::yellow,
    pending = crayon::blue,
    failed = crayon::red
  )

  # Count statuses
  status_counts <- table(unlist(cluster_status))
  total_clusters <- length(cluster_status)
  completed_count <- if("completed" %in% names(status_counts)) status_counts[["completed"]] else 0
  failed_count <- if("failed" %in% names(status_counts)) status_counts[["failed"]] else 0

  # Create progress units
  sorted_clusters <- sort(names(cluster_status))

  progress_units <- sapply(sorted_clusters, function(cluster_id) {
    status <- cluster_status[[cluster_id]]
    color_fn <- colors[[status]]
    if (is.null(color_fn)) color_fn <- identity
    symbol <- symbols[[status]]
    if (is.null(symbol)) symbol <- "?"
    color_fn(symbol)
  })

  progress_bar <- paste(progress_units, collapse = "")

  # Create status line
  if (is_final) {
    status_line <- paste0("[DONE] [", progress_bar, "] ", completed_count, "/", total_clusters)
    if (total_clusters > completed_count && failed_count > 0) {
      status_line <- paste0(status_line, " (", failed_count, " failed)")
    } else if (total_clusters == completed_count) {
      status_line <- paste0(status_line, " completed")
    }
    cat(paste0("\r", status_line, "\n"))

    # Show failed cluster details
    if (failed_count > 0) {
      failed_clusters <- names(cluster_status)[unlist(cluster_status) == "failed"]
      cluster_details <- sapply(failed_clusters, function(cluster_id) {
        paste0(crayon::red(cli::symbol$cross), " Cluster ", cluster_id)
      })

      # Group into lines of 4
      for (i in seq(1, length(cluster_details), 4)) {
        line_end <- min(i + 3, length(cluster_details))
        line_details <- cluster_details[i:line_end]
        cat(paste0("  ", paste(line_details, collapse = " | "), "\n"))
      }
    }
  } else {
    # Progress with spinner
    spinner_chars <- c("\u280b", "\u2819", "\u2839", "\u2838",
                       "\u283c", "\u2834", "\u2826", "\u2827",
                       "\u2807", "\u280f")
    spinner <- spinner_chars[(spinner_frame %% length(spinner_chars)) + 1]
    status_line <- paste0(spinner, " [", progress_bar, "] ", completed_count, "/", total_clusters, " completed")
    cat(paste0("\r", status_line), sep = "")
    flush.console()
  }
}

