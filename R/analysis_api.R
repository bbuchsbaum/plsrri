#' High-Level Analysis API
#'
#' @description
#' Public convenience wrappers for the staged non-GUI analysis workflow.
#' These functions expose a small, stable surface over the pipeline and
#' builder layers:
#'
#' - \code{prepare_firstlevel()}: validate, discover, plan, and optionally run
#'   first-level estimation from a pipeline specification.
#' - \code{prepare_pls()}: build a runnable \code{\link{pls_spec}} from a saved
#'   pipeline, artifact root, or analysis plan.
#' - \code{run_pls()}: run a prepared \code{\link{pls_spec}} or dispatch from
#'   saved pipeline/artifact inputs.
#' - \code{render_pls_report()}: render a Quarto report from a result or saved
#'   artifacts.
#'
#' @name analysis-api
NULL

.analysis_default_pls_options <- function() {
  list(
    method = "task",
    nperm = 0L,
    nboot = 0L,
    nsplit = 0L,
    clim = 95
  )
}

.analysis_is_plan <- function(x) {
  is.list(x) &&
    !is.null(x$analysis) &&
    !is.null(x$manifest_kind) &&
    !is.null(x$manifest)
}

.analysis_is_pipeline_spec_input <- function(x) {
  if (inherits(x, "plsrri_firstlevel_prep")) {
    return(TRUE)
  }
  if (is.character(x) && length(x) == 1L && nzchar(x)) {
    ext <- tolower(tools::file_ext(x))
    return(ext %in% c("yml", "yaml"))
  }
  if (!is.list(x) || .analysis_is_plan(x)) {
    return(FALSE)
  }
  any(c("dataset", "design", "first_level", "pls", "outputs") %in% names(x))
}

.analysis_merge_pls_options <- function(base, overrides = NULL) {
  utils::modifyList(base %||% list(), overrides %||% list(), keep.null = TRUE)
}

.analysis_plan_from_pipeline_spec <- function(spec) {
  spec_obj <- validate_pipeline_spec(spec)
  paths <- .pipeline_artifact_paths(spec_obj)
  if (!file.exists(paths$pls_plan_rds) || !file.exists(paths$pls_manifest_tsv)) {
    pipeline_pls_plan(spec_obj)
  }

  plan_obj <- readRDS(paths$pls_plan_rds)
  plan_obj$manifest <- .pipeline_read_tsv(
    paths$pls_manifest_tsv,
    root = paths$root,
    path_cols = c("file")
  )
  .validate_analysis_plan(plan_obj)
  list(
    spec = spec_obj,
    artifact_root = paths$root,
    plan = plan_obj
  )
}

.analysis_pls_options_from_spec <- function(plan, spec) {
  opts <- list(
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

  if (!is.null(spec$pls$behavior) && !is.null(spec$pls$behavior$file)) {
    behavior_mat <- .pipeline_align_behavior(
      plan$manifest[, c("group", "subject", "condition"), drop = FALSE],
      spec$pls$behavior
    )
    opts$behavior_data <- behavior_mat
    opts$behavior_measures <- colnames(behavior_mat)
    opts$block_conditions <- .pipeline_nested(spec, c("pls", "behavior", "block_conditions"), NULL)
  }

  design_file <- .pipeline_nested(spec, c("pls", "design", "file"), NULL)
  if (!is.null(design_file)) {
    design_mat <- .pipeline_read_table_file(design_file)
    if (is.data.frame(design_mat)) {
      design_mat <- as.matrix(design_mat)
    }
    opts$design_matrix <- design_mat
    opts$design_labels <- colnames(design_mat)
  }

  opts
}

.new_firstlevel_preparation <- function(spec,
                                        artifact_root,
                                        validation,
                                        discovery,
                                        work_plan,
                                        firstlevel_manifest = NULL,
                                        summary = NULL,
                                        work_id = NULL) {
  structure(
    list(
      spec = spec,
      artifact_root = artifact_root,
      validation = validation,
      discovery = discovery,
      work_plan = work_plan,
      firstlevel_manifest = firstlevel_manifest,
      summary = summary,
      work_id = work_id
    ),
    class = "plsrri_firstlevel_prep"
  )
}

#' @export
print.plsrri_firstlevel_prep <- function(x, ...) {
  cat("<plsrri_firstlevel_prep>\n")
  cat("  artifact_root:", x$artifact_root, "\n")
  cat("  work_units:", nrow(x$work_plan %||% data.frame()), "\n")
  if (!is.null(x$firstlevel_manifest)) {
    cat("  outputs:", nrow(x$firstlevel_manifest), "map(s)\n")
  } else {
    cat("  outputs: planned only\n")
  }
  invisible(x)
}

#' Prepare First-Level Artifacts
#'
#' @param spec Path to a pipeline YAML file or a specification list.
#' @param work_id Optional work-unit identifier for array/HPC execution.
#' @param run Logical; if \code{TRUE} (default), execute first-level estimation
#'   after planning. If \code{FALSE}, only validate, discover, and write the
#'   work plan.
#' @param summarize Logical; if \code{TRUE} (default), refresh
#'   \code{pipeline_summary.tsv} after preparation.
#'
#' @return A \code{plsrri_firstlevel_prep} object containing the validated
#'   specification, artifact root, discovery manifest, work plan, and optional
#'   first-level output manifest.
#' @export
prepare_firstlevel <- function(spec,
                               work_id = NULL,
                               run = TRUE,
                               summarize = TRUE) {
  spec_obj <- validate_pipeline_spec(spec)
  paths <- .pipeline_artifact_paths(spec_obj)

  validation <- pipeline_validate(spec_obj)
  discovery <- pipeline_discover(spec_obj)
  work_plan <- pipeline_firstlevel_plan(spec_obj)
  firstlevel_manifest <- if (isTRUE(run)) {
    pipeline_firstlevel_run(spec_obj, work_id = work_id)
  } else {
    NULL
  }
  summary <- if (isTRUE(summarize)) pipeline_summarize(spec_obj) else NULL

  .new_firstlevel_preparation(
    spec = spec_obj,
    artifact_root = paths$root,
    validation = validation,
    discovery = discovery,
    work_plan = work_plan,
    firstlevel_manifest = firstlevel_manifest,
    summary = summary,
    work_id = work_id
  )
}

#' Prepare a Runnable PLS Specification
#'
#' @param x One of:
#'   \itemize{
#'     \item a pipeline YAML file or specification list
#'     \item a \code{plsrri_firstlevel_prep} object
#'     \item an attached artifact root directory
#'     \item an analysis plan from \code{\link{pipeline_load_analysis_plan}}
#'   }
#' @param pls_options Optional named list of PLS overrides. For pipeline-spec
#'   inputs these override the saved PLS configuration. For artifact-root or
#'   analysis-plan inputs they define the PLS configuration.
#' @param input_type Optional first-level output type for artifact-root inputs.
#' @param statistic Optional first-level statistic for artifact-root inputs.
#' @param remap Optional alternative root for resolving moved relative paths
#'   when \code{x} is an artifact root.
#'
#' @return A configured \code{\link{pls_spec}} object.
#' @export
prepare_pls <- function(x,
                        pls_options = NULL,
                        input_type = NULL,
                        statistic = NULL,
                        remap = NULL) {
  if (inherits(x, "pls_spec")) {
    return(x)
  }

  if (.analysis_is_pipeline_spec_input(x)) {
    spec_obj <- if (inherits(x, "plsrri_firstlevel_prep")) x$spec else validate_pipeline_spec(x)
    plan_info <- .analysis_plan_from_pipeline_spec(spec_obj)
    base_opts <- .analysis_pls_options_from_spec(plan_info$plan, spec_obj)
    opts <- .analysis_merge_pls_options(base_opts, pls_options)
    return(pipeline_build_pls_spec_from_ui(plan_info$plan, opts))
  }

  if (.analysis_is_plan(x)) {
    return(pipeline_build_pls_spec_from_ui(
      x,
      .analysis_merge_pls_options(.analysis_default_pls_options(), pls_options)
    ))
  }

  if (is.character(x) && length(x) == 1L && dir.exists(x)) {
    plan <- pipeline_load_analysis_plan(
      root = x,
      input_type = input_type,
      statistic = statistic,
      remap = remap
    )
    return(pipeline_build_pls_spec_from_ui(
      plan,
      .analysis_merge_pls_options(.analysis_default_pls_options(), pls_options)
    ))
  }

  stop(
    "x must be a pipeline spec, first-level preparation object, artifact root, analysis plan, or pls_spec",
    call. = FALSE
  )
}

#' Run PLS from a Prepared or Saved Input
#'
#' @param x A \code{\link{pls_spec}}, pipeline YAML/list, first-level
#'   preparation object, artifact root, or analysis plan.
#' @param pls_options Optional PLS option overrides passed to
#'   \code{\link{prepare_pls}} when \code{x} is not already a
#'   \code{\link{pls_spec}}.
#' @param input_type Optional first-level output type for artifact-root inputs.
#' @param statistic Optional first-level statistic for artifact-root inputs.
#' @param remap Optional alternative root for resolving moved relative paths.
#' @param progress Logical; passed to \code{\link{run}} when running an
#'   in-memory \code{\link{pls_spec}}.
#' @param ... Additional arguments forwarded to \code{\link{run}}.
#'
#' @return A \code{\link{pls_result}} object.
#' @export
run_pls <- function(x,
                    pls_options = NULL,
                    input_type = NULL,
                    statistic = NULL,
                    remap = NULL,
                    progress = FALSE,
                    ...) {
  if (inherits(x, "pls_spec")) {
    return(run(x, progress = progress, ...))
  }

  if (is.null(pls_options) && .analysis_is_pipeline_spec_input(x)) {
    spec_obj <- if (inherits(x, "plsrri_firstlevel_prep")) x$spec else validate_pipeline_spec(x)
    return(pipeline_pls_run(spec_obj))
  }

  pspec <- prepare_pls(
    x,
    pls_options = pls_options,
    input_type = input_type,
    statistic = statistic,
    remap = remap
  )
  run(pspec, progress = progress, ...)
}

#' Render a PLS Report
#'
#' @inheritParams render_report
#'
#' @return Path to the rendered report, invisibly.
#' @export
render_pls_report <- function(x,
                              output_file = NULL,
                              output_format = "html",
                              title = "PLS Analysis Report",
                              author = Sys.info()[["user"]],
                              template = NULL,
                              include_brain = TRUE,
                              bsr_threshold = 3,
                              p_threshold = 0.05,
                              open = FALSE,
                              ...) {
  render_report(
    x,
    output_file = output_file,
    output_format = output_format,
    title = title,
    author = author,
    template = template,
    include_brain = include_brain,
    bsr_threshold = bsr_threshold,
    p_threshold = p_threshold,
    open = open,
    ...
  )
}
