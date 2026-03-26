#' Pipeline Attach API
#'
#' @description
#' Public API for attaching to a completed CLI pipeline output directory.
#' These functions form the stable contract between the Shiny UI and the
#' pipeline internals — the UI should call these, not low-level helpers.
#'
#' @name pipeline-attach
NULL

#' Summarize a Pipeline Output Root
#'
#' Reads and validates a CLI pipeline output directory, returning a
#' structured summary of the first-level outputs found there.
#'
#' @param root Path to the pipeline output root directory (the folder
#'   containing \code{firstlevel/}, \code{pls/}, etc.).
#' @param remap Optional alternative root for resolving relative paths.
#'   Use when the output directory has been moved from its original location.
#'   If \code{NULL} (default), paths are resolved against \code{root}.
#'
#' @return A list with components:
#'   \describe{
#'     \item{root}{The output root path.}
#'     \item{valid}{Logical; \code{TRUE} if the output root is usable.}
#'     \item{errors}{Character vector of validation errors (empty if valid).}
#'     \item{firstlevel_plan}{First-level work plan data.frame.}
#'     \item{firstlevel_manifest}{Consolidated first-level manifest data.frame.}
#'     \item{discovery_manifest}{Discovery manifest data.frame, or \code{NULL}.}
#'     \item{summary}{Named list with \code{n_subjects}, \code{n_groups},
#'       \code{groups}, \code{types}, \code{statistics}, etc.}
#'   }
#'
#' @export
pipeline_attach_summary <- function(root, remap = NULL) {
  root <- normalizePath(root, winslash = "/", mustWork = FALSE)
  resolve_root <- if (!is.null(remap)) {
    normalizePath(remap, winslash = "/", mustWork = FALSE)
  } else {
    root
  }

  if (!dir.exists(root)) {
    return(list(root = root, valid = FALSE,
                errors = "Output root directory does not exist"))
  }

  errors <- character(0)


  # ---- Required artifacts --------------------------------------------------

  plan_path <- file.path(root, "firstlevel", "work_plan.tsv")
  if (!file.exists(plan_path)) {
    errors <- c(errors, "Missing firstlevel/work_plan.tsv")
  }

  work_dir <- file.path(root, "firstlevel", "work")
  if (!dir.exists(work_dir)) {
    errors <- c(errors, "Missing firstlevel/work/ directory")
  }

  if (length(errors) > 0) {
    return(list(root = root, valid = FALSE, errors = errors))
  }

  # ---- Read work plan ------------------------------------------------------

  plan_cols <- c("work_dir", "fit_file", "manifest_file")
  plan <- tryCatch(
    .pipeline_read_tsv(plan_path, root = resolve_root, path_cols = plan_cols),
    error = function(e) {
      errors <<- c(errors, paste("Cannot read work plan:", e$message))
      NULL
    }
  )
  if (is.null(plan)) {
    return(list(root = root, valid = FALSE, errors = errors))
  }

  tryCatch(.validate_firstlevel_plan(plan), error = function(e) {
    errors <<- c(errors, paste("Invalid work plan:", e$message))
  })
  if (length(errors) > 0) {
    return(list(root = root, valid = FALSE, errors = errors))
  }

  # ---- Read per-work manifests ---------------------------------------------

  manifest_files <- plan$manifest_file[file.exists(plan$manifest_file)]
  if (length(manifest_files) == 0) {
    return(list(root = root, valid = FALSE,
                errors = "No completed first-level work units found"))
  }

  manifest <- tryCatch(
    .pipeline_read_firstlevel_manifests(manifest_files, root = resolve_root),
    error = function(e) {
      errors <<- c(errors, paste("Error reading manifests:", e$message))
      NULL
    }
  )
  if (is.null(manifest)) {
    return(list(root = root, valid = FALSE, errors = errors))
  }

  # ---- Optional: discovery manifest ----------------------------------------

  discovery_path <- file.path(root, "discovery", "study_manifest.tsv")
  discovery <- if (file.exists(discovery_path)) {
    tryCatch(.pipeline_read_tsv(discovery_path), error = function(e) NULL)
  } else {
    NULL
  }

  # ---- Build summary -------------------------------------------------------

  mask_files <- unique(stats::na.omit(manifest$mask_file))

  summary_info <- list(
    n_work_units  = nrow(plan),
    n_completed   = length(manifest_files),
    n_subjects    = length(unique(manifest$subject)),
    n_groups      = length(unique(manifest$group)),
    groups        = sort(unique(manifest$group)),
    types         = sort(unique(manifest$type)),
    statistics    = sort(unique(manifest$statistic)),
    labels        = sort(unique(manifest$label)),
    n_masks       = length(mask_files),
    mask_file     = if (length(mask_files) == 1L) mask_files else mask_files
  )

  # Conditions (may be NA in manifests without basis parsing)
  if ("condition" %in% names(manifest) && any(!is.na(manifest$condition))) {
    summary_info$conditions   <- sort(unique(stats::na.omit(manifest$condition)))
    summary_info$n_conditions <- length(summary_info$conditions)
  }

  # Basis / lag info
  if ("basis_index" %in% names(manifest) && any(!is.na(manifest$basis_index))) {
    summary_info$has_basis <- TRUE
    summary_info$n_lags    <- length(unique(stats::na.omit(manifest$basis_index)))
  } else {
    summary_info$has_basis <- FALSE
  }

  list(
    root                = root,
    valid               = TRUE,
    errors              = character(0),
    firstlevel_plan     = plan,
    firstlevel_manifest = manifest,
    discovery_manifest  = discovery,
    summary             = summary_info
  )
}

#' Load an Analysis Plan from Attached Outputs
#'
#' Reads first-level manifests from a pipeline output root, filters by
#' output type and statistic, and returns a validated analysis plan that
#' can be passed to \code{\link{pipeline_build_pls_spec_from_ui}}.
#'
#' @param root Path to the pipeline output root directory.
#' @param analysis Analysis type (currently only \code{"pls"}).
#' @param input_type First-level output type to use (e.g., \code{"estimates"}).
#'   Auto-detected from the most common value if \code{NULL}.
#' @param statistic First-level statistic to use (e.g., \code{"estimate"}).
#'   Auto-detected from the most common value if \code{NULL}.
#' @param remap Optional alternative root for resolving relative paths.
#'
#' @return A validated analysis plan list suitable for
#'   \code{\link{pipeline_build_pls_spec_from_ui}}.
#'
#' @export
pipeline_load_analysis_plan <- function(root,
                                        analysis = "pls",
                                        input_type = NULL,
                                        statistic = NULL,
                                        remap = NULL) {
  info <- pipeline_attach_summary(root, remap = remap)
  if (!info$valid) {
    stop(paste(c("Cannot load analysis plan:", info$errors), collapse = "\n  "),
         call. = FALSE)
  }

  manifest <- info$firstlevel_manifest

  # Auto-detect type / statistic from available outputs
  if (is.null(input_type)) {
    types <- unique(manifest$type)
    input_type <- if ("estimates" %in% types) "estimates" else types[1]
  }
  if (is.null(statistic)) {
    stats_avail <- unique(manifest$statistic)
    statistic <- if ("estimate" %in% stats_avail) "estimate" else stats_avail[1]
  }

  pls_input_spec <- list(type = input_type, statistic = statistic)

  .pipeline_build_analysis_plan(
    manifest,
    pls_input_spec,
    analysis = analysis,
    fallback_mask = NULL
  )
}

#' Build a PLS Specification from UI Options
#'
#' Converts an analysis plan (from \code{\link{pipeline_load_analysis_plan}})
#' and user-selected PLS options into a configured \code{\link{pls_spec}}
#' ready for \code{\link{run}()}.
#'
#' @param plan An analysis plan as returned by
#'   \code{\link{pipeline_load_analysis_plan}}.
#' @param pls_options A named list of PLS configuration. Recognised elements:
#'   \describe{
#'     \item{method}{PLS method name or integer (e.g., \code{"task"},
#'       \code{"behavior"}, \code{1L}).}
#'     \item{nperm}{Number of permutations (integer).}
#'     \item{nboot}{Number of bootstrap samples (integer).}
#'     \item{nsplit}{Number of split-half iterations (integer).}
#'     \item{clim}{Confidence level (numeric, 0–100).}
#'     \item{meancentering}{Mean-centering type (integer or string).}
#'     \item{cormode}{Correlation mode (integer or string).}
#'     \item{boot_type}{Bootstrap type (\code{"strat"} or \code{"nonstrat"}).}
#'     \item{is_struct}{Logical; structure PLS flag.}
#'     \item{behavior_data}{Optional behaviour matrix (rows = observations).}
#'     \item{behavior_measures}{Optional column names for \code{behavior_data}.}
#'     \item{block_conditions}{Optional block conditions for behaviour PLS.}
#'     \item{design_matrix}{Optional contrast/design matrix for non-rotated
#'       methods.}
#'     \item{design_labels}{Optional labels for \code{design_matrix} columns.}
#'   }
#'
#' @return A configured \code{\link{pls_spec}} object.
#'
#' @export
pipeline_build_pls_spec_from_ui <- function(plan, pls_options) {
  .validate_analysis_plan(plan)

  pls_manifest <- plan$manifest
  mask <- plan$mask_file
  if (is.null(mask) || !nzchar(as.character(mask)[1])) {
    stop("A mask file is required for map-based analysis", call. = FALSE)
  }

  pspec <- pls_spec()

  # Add subjects from manifest, branching on kind

  if (identical(plan$manifest_kind, "voxel_lag") || "lag" %in% names(pls_manifest)) {
    pspec <- add_subjects_manifest(
      pspec,
      manifest = pls_manifest[, c("group", "subject", "condition", "lag", "file"),
                              drop = FALSE],
      mask = mask,
      lag_col = "lag"
    )
  } else {
    pspec <- add_subjects_map_manifest(
      pspec,
      manifest = pls_manifest[, c("group", "subject", "condition", "file"),
                              drop = FALSE],
      mask = mask
    )
  }

  # ---- Optional behaviour data (already a matrix, not a file path) ---------

  if (!is.null(pls_options$behavior_data)) {
    measures <- pls_options$behavior_measures %||% colnames(pls_options$behavior_data)
    pspec <- add_behavior(
      pspec,
      data   = pls_options$behavior_data,
      measures = measures,
      block_conditions = pls_options$block_conditions
    )
  }

  # ---- Optional design matrix (for non-rotated methods) --------------------

  if (!is.null(pls_options$design_matrix)) {
    pspec <- add_design(
      pspec,
      contrasts = pls_options$design_matrix,
      labels    = pls_options$design_labels
    )
  }

  # ---- Map method name if integer ------------------------------------------

  method <- pls_options$method %||% "task"
  if (is.numeric(method)) {
    method <- pls_method_int_to_name(method)
  }

  # ---- Configure PLS parameters --------------------------------------------

  configure(
    pspec,
    method         = method,
    nperm          = as.integer(pls_options$nperm  %||% 0L),
    nboot          = as.integer(pls_options$nboot  %||% 0L),
    nsplit         = as.integer(pls_options$nsplit  %||% 0L),
    clim           = as.numeric(pls_options$clim   %||% 95),
    meancentering  = pls_options$meancentering,
    cormode        = pls_options$cormode,
    boot_type      = pls_options$boot_type,
    is_struct      = pls_options$is_struct
  )
}
