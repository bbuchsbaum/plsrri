#' Pipeline Data Contracts
#'
#' @description
#' Constructors and validators for staged `plscli` manifests.
#'
#' @name pipeline-contracts
NULL

.pipeline_require_cols <- function(df, required, what) {
  missing <- setdiff(required, names(df))
  if (length(missing) > 0) {
    stop(what, " is missing required columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  invisible(df)
}

.validate_discovery_manifest <- function(df) {
  .pipeline_require_cols(
    df,
    c("subject", "group", "task", "run", "scan_file", "events_file", "mask_file", "n_volumes", "tr"),
    "Discovery manifest"
  )
  if (nrow(df) == 0) stop("Discovery manifest must have at least one row", call. = FALSE)
  invisible(df)
}

.validate_firstlevel_plan <- function(df) {
  .pipeline_require_cols(
    df,
    c("subject", "group", "task", "work_id", "scan_count", "work_dir", "fit_file", "manifest_file"),
    "First-level plan"
  )
  if (anyDuplicated(df$work_id)) stop("First-level plan contains duplicate work_id values", call. = FALSE)
  invisible(df)
}

.validate_firstlevel_manifest <- function(df) {
  .pipeline_require_cols(
    df,
    c("work_id", "subject", "group", "task", "type", "label", "statistic", "file", "mask_file"),
    "First-level manifest"
  )
  if (nrow(df) == 0) stop("First-level manifest must have at least one row", call. = FALSE)
  invisible(df)
}

.validate_pls_manifest <- function(df, kind = c("auto", "voxel_map", "voxel_lag")) {
  kind <- match.arg(kind)
  base_cols <- c("group", "subject", "condition", "file")
  .pipeline_require_cols(df, base_cols, "PLS manifest")
  if (kind == "voxel_lag" || (kind == "auto" && "lag" %in% names(df))) {
    .pipeline_require_cols(df, "lag", "PLS lag manifest")
  }
  if (nrow(df) == 0) stop("PLS manifest must have at least one row", call. = FALSE)
  invisible(df)
}

.validate_analysis_plan <- function(x) {
  required <- c("analysis", "manifest", "manifest_kind", "mask_file", "input_type", "statistic")
  missing <- setdiff(required, names(x))
  if (length(missing) > 0) {
    stop("Analysis plan is missing required fields: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  if (!is.data.frame(x$manifest)) {
    stop("Analysis plan manifest must be a data.frame", call. = FALSE)
  }
  .validate_pls_manifest(x$manifest, kind = x$manifest_kind %||% "auto")
  invisible(x)
}

.new_analysis_plan <- function(analysis,
                               manifest,
                               manifest_kind,
                               mask_file,
                               input_type,
                               statistic,
                               basis_order = NULL) {
  out <- list(
    analysis = as.character(analysis)[1],
    manifest = manifest,
    manifest_kind = as.character(manifest_kind)[1],
    mask_file = if (is.null(mask_file)) NULL else as.character(mask_file)[1],
    input_type = as.character(input_type)[1],
    statistic = as.character(statistic)[1],
    basis_order = if (is.null(basis_order)) NULL else as.character(basis_order)
  )
  .validate_analysis_plan(out)
  out
}

.new_firstlevel_manifest_row <- function(work_id,
                                         subject,
                                         group,
                                         task,
                                         type,
                                         label,
                                         statistic,
                                         file,
                                         mask_file,
                                         condition = NA_character_,
                                         basis_label = NA_character_,
                                         basis_index = NA_integer_) {
  data.frame(
    work_id = as.character(work_id),
    subject = as.character(subject),
    group = as.character(group),
    task = as.character(task),
    type = as.character(type),
    label = as.character(label),
    statistic = as.character(statistic),
    file = as.character(file),
    mask_file = as.character(mask_file),
    condition = as.character(condition),
    basis_label = as.character(basis_label),
    basis_index = as.integer(basis_index),
    stringsAsFactors = FALSE
  )
}

.new_firstlevel_plan_row <- function(subject,
                                     group,
                                     task,
                                     work_id,
                                     scan_count,
                                     work_dir,
                                     fit_file,
                                     manifest_file) {
  data.frame(
    subject = as.character(subject),
    group = as.character(group),
    task = as.character(task),
    work_id = as.character(work_id),
    scan_count = as.integer(scan_count),
    work_dir = as.character(work_dir),
    fit_file = as.character(fit_file),
    manifest_file = as.character(manifest_file),
    stringsAsFactors = FALSE
  )
}
