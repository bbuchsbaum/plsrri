#' Pipeline PLS Stages
#'
#' @description
#' PLS-specific adapters and execution stages for `plscli`.
#'
#' @name pipeline-pls
NULL

.pipeline_align_behavior <- function(brain_manifest, behavior_spec) {
  behavior_file <- behavior_spec$file
  beh <- .pipeline_read_table_file(behavior_file)
  if (!is.data.frame(beh)) stop("Behavior file must parse to a data.frame", call. = FALSE)

  subject_col <- behavior_spec$subject_col %||% "subject"
  condition_col <- behavior_spec$condition_col %||% "condition"
  group_col <- behavior_spec$group_col %||% NULL
  measures <- .pipeline_as_chr(behavior_spec$measures)

  req <- c(subject_col, condition_col)
  missing_cols <- setdiff(req, names(beh))
  if (length(missing_cols) > 0) {
    stop("Behavior file is missing required columns: ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  if (!length(measures)) {
    drop_cols <- c(subject_col, condition_col, group_col)
    measures <- setdiff(names(beh), drop_cols)
  }
  if (!length(measures)) stop("No behavior measure columns were resolved", call. = FALSE)

  key_fun <- function(subject, condition, group = NULL) {
    if (is.null(group) || !nzchar(group)) {
      paste(subject, condition, sep = "\t")
    } else {
      paste(group, subject, condition, sep = "\t")
    }
  }

  beh_subject <- as.character(beh[[subject_col]])
  beh_condition <- as.character(beh[[condition_col]])
  beh_group <- if (!is.null(group_col) && group_col %in% names(beh)) as.character(beh[[group_col]]) else NULL
  use_group <- !is.null(group_col) && group_col %in% names(beh)
  beh_key <- if (use_group) {
    key_fun(beh_subject, beh_condition, beh_group)
  } else {
    key_fun(beh_subject, beh_condition, NULL)
  }
  if (anyDuplicated(beh_key)) {
    dup <- unique(beh_key[duplicated(beh_key)])
    stop("Behavior file contains duplicate observation keys (showing up to 5): ",
         paste(utils::head(dup, 5), collapse = ", "), call. = FALSE)
  }

  brain_key <- if (use_group) {
    key_fun(brain_manifest$subject, brain_manifest$condition, brain_manifest$group)
  } else {
    key_fun(brain_manifest$subject, brain_manifest$condition, NULL)
  }
  idx <- match(brain_key, beh_key)
  if (anyNA(idx)) {
    miss <- unique(brain_key[is.na(idx)])
    stop("Behavior file is missing rows for some PLS observations (showing up to 5): ",
         paste(utils::head(miss, 5), collapse = ", "), call. = FALSE)
  }

  as.matrix(beh[idx, measures, drop = FALSE])
}

.pipeline_method_name <- function(x) {
  x <- tolower(as.character(x)[1])
  switch(
    x,
    task = "task",
    task_nonrotated = "task_nonrotated",
    behavior = "behavior",
    behavior_nonrotated = "behavior_nonrotated",
    multiblock = "multiblock",
    multiblock_nonrotated = "multiblock_nonrotated",
    stop("Unsupported pls.method: ", x, call. = FALSE)
  )
}

.pipeline_parse_basis_manifest <- function(map_rows, input_spec) {
  if (all(c("condition", "basis_index") %in% names(map_rows)) &&
      any(!is.na(map_rows$condition)) &&
      any(!is.na(map_rows$basis_index))) {
    out <- data.frame(
      group = map_rows$group,
      subject = map_rows$subject,
      condition = ifelse(is.na(map_rows$condition), map_rows$label, map_rows$condition),
      lag = as.integer(map_rows$basis_index - 1L),
      file = map_rows$file,
      type = map_rows$type,
      statistic = map_rows$statistic,
      label = map_rows$label,
      task = map_rows$task,
      mask_file = map_rows$mask_file,
      stringsAsFactors = FALSE
    )
    return(list(
      kind = "voxel_lag",
      manifest = out,
      basis_order = .pipeline_as_chr(input_spec$basis_order)
    ))
  }

  pattern <- input_spec$basis_pattern %||% NULL
  if (is.null(pattern) || !nzchar(as.character(pattern)[1])) {
    out <- map_rows[, c("group", "subject", "condition", "file", "type", "statistic", "label", "task", "mask_file"), drop = FALSE]
    return(list(kind = "voxel_map", manifest = out))
  }

  condition_group <- as.integer(input_spec$condition_group %||% 1L)
  basis_group <- as.integer(input_spec$basis_group %||% 2L)

  pieces <- regexec(pattern, map_rows$label, perl = TRUE)
  matches <- regmatches(map_rows$label, pieces)
  ok <- vapply(matches, function(x) length(x) > max(condition_group, basis_group), logical(1))
  if (!all(ok)) {
    bad <- unique(map_rows$label[!ok])
    stop("basis_pattern did not match some first-level labels (showing up to 5): ",
         paste(utils::head(bad, 5), collapse = ", "), call. = FALSE)
  }

  parsed_condition <- vapply(matches, function(x) x[[condition_group + 1L]], character(1))
  basis_label <- vapply(matches, function(x) x[[basis_group + 1L]], character(1))

  basis_order <- .pipeline_as_chr(input_spec$basis_order)
  if (!length(basis_order)) {
    basis_order <- unique(basis_label)
  }
  lag_index <- match(basis_label, basis_order)
  if (anyNA(lag_index)) {
    stop("Could not resolve lag order for parsed basis labels", call. = FALSE)
  }

  out <- data.frame(
    group = map_rows$group,
    subject = map_rows$subject,
    condition = parsed_condition,
    lag = as.integer(lag_index - 1L),
    file = map_rows$file,
    type = map_rows$type,
    statistic = map_rows$statistic,
    label = map_rows$label,
    task = map_rows$task,
    mask_file = map_rows$mask_file,
    stringsAsFactors = FALSE
  )

  list(kind = "voxel_lag", manifest = out, basis_order = basis_order)
}

.pipeline_pls_summary <- function(result) {
  out <- data.frame(
    lv = seq_along(result$s),
    singular_value = as.numeric(result$s),
    variance_explained = as.numeric((result$s^2) / sum(result$s^2)),
    stringsAsFactors = FALSE
  )
  if (!is.null(result$perm_result) && !is.null(result$perm_result$sprob)) {
    out$p_value <- as.numeric(result$perm_result$sprob)
  }
  out
}

#' Build a PLS Observation Manifest from First-Level Outputs
#'
#' @param spec Path to a YAML file or a pipeline specification list.
#'
#' @return The PLS manifest as a data.frame.
#' @export
pipeline_pls_plan <- function(spec) {
  spec <- validate_pipeline_spec(spec)
  paths <- .pipeline_artifact_paths(spec)
  if (!file.exists(paths$firstlevel_plan_tsv)) {
    pipeline_firstlevel_plan(spec)
  }

  plan <- .pipeline_read_tsv(paths$firstlevel_plan_tsv,
                            root = paths$root, path_cols = c("work_dir", "fit_file", "manifest_file"))
  manifest_files <- plan$manifest_file[file.exists(plan$manifest_file)]
  map_rows <- .pipeline_read_firstlevel_manifests(manifest_files, root = paths$root)
  analysis_plan <- .pipeline_build_analysis_plan(
    map_rows,
    spec$pls$input,
    analysis = "pls",
    fallback_mask = .pipeline_nested(spec, c("dataset", "mask"), NULL)
  )
  pls_manifest <- analysis_plan$manifest
  .pipeline_write_tsv(pls_manifest, paths$pls_manifest_tsv,
                      root = paths$root, path_cols = c("file"))

  plan_obj <- c(
    analysis_plan[c("analysis", "manifest_kind", "mask_file", "input_type", "statistic", "basis_order")],
    list(manifest_path = paths$pls_manifest_tsv)
  )
  .pipeline_ensure_parent(paths$pls_plan_rds)
  saveRDS(plan_obj, paths$pls_plan_rds)
  pls_manifest
}

#' Run PLS from a Planned Manifest
#'
#' @param spec Path to a YAML file or a pipeline specification list.
#'
#' @return A `pls_result` object.
#' @export
pipeline_pls_run <- function(spec) {
  spec <- validate_pipeline_spec(spec)
  paths <- .pipeline_artifact_paths(spec)
  if (!file.exists(paths$pls_plan_rds) || !file.exists(paths$pls_manifest_tsv)) {
    pipeline_pls_plan(spec)
  }

  plan_obj <- readRDS(paths$pls_plan_rds)
  pls_manifest <- .pipeline_read_tsv(paths$pls_manifest_tsv,
                                    root = paths$root, path_cols = c("file"))
  plan_obj$manifest <- pls_manifest
  .validate_analysis_plan(plan_obj)
  if (!identical(plan_obj$analysis, "pls")) {
    stop("pipeline_pls_run() requires an analysis plan with analysis = 'pls'", call. = FALSE)
  }

  pspec <- .pipeline_build_pls_spec_from_plan(
    plan_obj = plan_obj,
    spec = spec
  )

  result <- run(pspec, progress = FALSE)
  .pipeline_ensure_parent(paths$pls_result_rds)
  saveRDS(result, paths$pls_result_rds)
  .pipeline_write_tsv(.pipeline_pls_summary(result), paths$pls_summary_tsv)
  result
}
