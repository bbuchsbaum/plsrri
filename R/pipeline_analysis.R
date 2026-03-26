#' Pipeline Analysis Adapters
#'
#' @description
#' Generic helpers that convert first-level outputs into reusable
#' analysis-plan objects, plus a PLS-specific adapter that consumes them.
#'
#' @name pipeline-analysis
NULL

.pipeline_read_firstlevel_manifests <- function(manifest_files, root = NULL) {
  if (!length(manifest_files)) {
    stop("No completed first-level map manifests were found", call. = FALSE)
  }
  out <- do.call(rbind, lapply(manifest_files, .pipeline_read_tsv,
                               root = root, path_cols = c("file", "mask_file")))
  .validate_firstlevel_manifest(out)
  out
}

.pipeline_resolve_analysis_mask <- function(manifest, fallback_mask = NULL) {
  masks <- unique(stats::na.omit(manifest$mask_file))
  if (length(masks) > 1L) {
    stop("Analysis manifest spans multiple mask files; harmonize masks before running multivariate analysis", call. = FALSE)
  }
  if (length(masks) == 1L) {
    return(masks[[1]])
  }
  fallback_mask
}

.pipeline_build_analysis_plan <- function(map_rows,
                                          input_spec,
                                          analysis = "pls",
                                          fallback_mask = NULL) {
  manifest_info <- .pipeline_build_pls_manifest(map_rows, input_spec)
  mask_file <- .pipeline_resolve_analysis_mask(manifest_info$manifest, fallback_mask = fallback_mask)
  .new_analysis_plan(
    analysis = analysis,
    manifest = manifest_info$manifest,
    manifest_kind = manifest_info$kind,
    mask_file = mask_file,
    input_type = input_spec$type,
    statistic = input_spec$statistic,
    basis_order = manifest_info$basis_order %||% NULL
  )
}

.pipeline_build_pls_spec_from_plan <- function(plan_obj, spec) {
  .validate_analysis_plan(plan_obj)

  pls_manifest <- plan_obj$manifest
  mask <- plan_obj$mask_file
  if (is.null(mask) || !nzchar(as.character(mask)[1])) {
    stop("A mask file is required for map-based multivariate analysis ingestion", call. = FALSE)
  }

  pspec <- pls_spec()
  if (identical(plan_obj$manifest_kind, "voxel_lag") || "lag" %in% names(pls_manifest)) {
    pspec <- pspec |>
      add_subjects_manifest(
        manifest = pls_manifest[, c("group", "subject", "condition", "lag", "file"), drop = FALSE],
        mask = mask,
        lag_col = "lag"
      )
  } else {
    pspec <- pspec |>
      add_subjects_map_manifest(
        manifest = pls_manifest[, c("group", "subject", "condition", "file"), drop = FALSE],
        mask = mask
      )
  }

  if (!is.null(spec$pls$behavior) && !is.null(spec$pls$behavior$file)) {
    behavior_mat <- .pipeline_align_behavior(
      pls_manifest[, c("group", "subject", "condition"), drop = FALSE],
      spec$pls$behavior
    )
    pspec <- add_behavior(
      pspec,
      data = behavior_mat,
      measures = colnames(behavior_mat),
      block_conditions = .pipeline_nested(spec, c("pls", "behavior", "block_conditions"), NULL)
    )
  }

  if (!is.null(.pipeline_nested(spec, c("pls", "design", "file"), NULL))) {
    design_file <- .pipeline_nested(spec, c("pls", "design", "file"), NULL)
    design_mat <- .pipeline_read_table_file(design_file)
    if (is.data.frame(design_mat)) design_mat <- as.matrix(design_mat)
    pspec <- add_design(pspec, design_mat, labels = colnames(design_mat))
  }

  configure(
    pspec,
    method = .pipeline_method_name(spec$pls$method),
    nperm = .pipeline_nested(spec, c("pls", "nperm"), 0L),
    nboot = .pipeline_nested(spec, c("pls", "nboot"), 0L),
    nsplit = .pipeline_nested(spec, c("pls", "nsplit"), 0L),
    clim = .pipeline_nested(spec, c("pls", "clim"), 95),
    meancentering = .pipeline_nested(spec, c("pls", "meancentering"), NULL),
    cormode = .pipeline_nested(spec, c("pls", "cormode"), NULL),
    boot_type = .pipeline_nested(spec, c("pls", "boot_type"), NULL),
    is_struct = .pipeline_nested(spec, c("pls", "is_struct"), NULL)
  )
}
