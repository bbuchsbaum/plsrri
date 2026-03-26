#' Pipeline Core Helpers
#'
#' @description
#' Internal helpers shared across discovery, first-level, and analysis stages.
#'
#' @name pipeline-core
NULL

.pipeline_require <- function(pkgs) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]
  if (length(missing) > 0) {
    stop("Missing required package(s): ", paste(missing, collapse = ", "), call. = FALSE)
  }
}

.pipeline_normalize_subject <- function(x) {
  gsub("^sub-", "", as.character(x))
}

.pipeline_extract_entity <- function(path, key) {
  bn <- basename(path)
  m <- regexec(paste0("_", key, "-([^_]+)"), bn, perl = TRUE)
  reg <- regmatches(bn, m)[[1]]
  if (length(reg) < 2L) return(NA_character_)
  reg[[2]]
}

.pipeline_safe_name <- function(x) {
  gsub("[^A-Za-z0-9._-]+", "-", as.character(x))
}

.pipeline_order_runs <- function(paths) {
  run_ids <- suppressWarnings(as.integer(.pipeline_extract_entity(paths, "run")))
  ord <- order(is.na(run_ids), run_ids, basename(paths))
  paths[ord]
}

.pipeline_nifti_info <- function(path) {
  if (requireNamespace("RNifti", quietly = TRUE)) {
    hdr <- RNifti::niftiHeader(path)
    dims <- as.integer(hdr$dim_[2:5])
    list(
      n_volumes = if (length(dims) >= 4L && !is.na(dims[4])) dims[4] else 1L,
      tr = if (!is.null(hdr$pixdim) && length(hdr$pixdim) >= 5L) as.numeric(hdr$pixdim[5]) else NA_real_
    )
  } else {
    .pipeline_require("neuroim2")
    hdr <- neuroim2::read_header(path)
    dims <- as.integer(hdr@dims)
    list(
      n_volumes = if (length(dims) >= 4L && !is.na(dims[4])) dims[4] else 1L,
      tr = NA_real_
    )
  }
}

.pipeline_read_events <- function(path, run_id) {
  tab <- .pipeline_read_table_file(path)
  if (!is.data.frame(tab)) stop("Event file did not parse to a data.frame: ", path, call. = FALSE)
  tab$run <- run_id
  tab
}

# ---------------------------------------------------------------------------
# Relative-path helpers for portable manifests
# ---------------------------------------------------------------------------

#' Make paths relative to a root directory
#' @param path Character vector of file paths.
#' @param root The root directory paths should be made relative to.
#' @return Character vector of relative paths (unchanged if outside root).
#' @keywords internal
.pipeline_relativize_path <- function(path, root) {
  root <- normalizePath(root, winslash = "/", mustWork = FALSE)
  if (!endsWith(root, "/")) root <- paste0(root, "/")
  vapply(path, function(p) {
    if (is.na(p) || !nzchar(p)) return(p)
    np <- normalizePath(p, winslash = "/", mustWork = FALSE)
    if (startsWith(np, root)) {
      substring(np, nchar(root) + 1L)
    } else {
      p
    }
  }, character(1), USE.NAMES = FALSE)
}

#' Resolve relative paths against a root directory
#' @param path Character vector of file paths.
#' @param root The root directory to resolve relative paths against.
#' @return Character vector of absolute paths.
#' @keywords internal
.pipeline_resolve_path <- function(path, root) {
  vapply(path, function(p) {
    if (is.na(p) || !nzchar(p)) return(p)
    # Already absolute — return as-is
    if (startsWith(p, "/") || grepl("^[A-Za-z]:", p)) return(p)
    # Relative — resolve against root
    normalizePath(file.path(root, p), winslash = "/", mustWork = FALSE)
  }, character(1), USE.NAMES = FALSE)
}

#' Relativize path columns in a data.frame
#' @keywords internal
.pipeline_relativize_df <- function(df, root, cols) {
  for (col in intersect(cols, names(df))) {
    df[[col]] <- .pipeline_relativize_path(df[[col]], root)
  }
  df
}

#' Resolve path columns in a data.frame
#' @keywords internal
.pipeline_resolve_df <- function(df, root, cols) {
  for (col in intersect(cols, names(df))) {
    df[[col]] <- .pipeline_resolve_path(df[[col]], root)
  }
  df
}
