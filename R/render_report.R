#' Quarto Report Generation for PLS
#'
#' @description
#' Functions for generating Quarto reports from in-memory PLS results or from
#' saved `plscli` artifact directories.
#'
#' @name pls-reports
NULL

.report_template_path <- function(template = NULL) {
  if (!is.null(template)) {
    return(normalizePath(template, winslash = "/", mustWork = TRUE))
  }

  built_in <- system.file("quarto", "pls_report.qmd", package = "plsrri")
  if (nzchar(built_in)) {
    return(built_in)
  }

  .create_temp_template()
}

.report_stage_template <- function(template_path, target_qmd) {
  file.copy(template_path, target_qmd, overwrite = TRUE)
  template_dir <- dirname(template_path)
  scss <- file.path(template_dir, "custom.scss")
  if (file.exists(scss)) {
    file.copy(scss, file.path(dirname(target_qmd), "custom.scss"), overwrite = TRUE)
  }
  invisible(target_qmd)
}

.report_detect_input <- function(x) {
  if (inherits(x, "pls_result")) {
    return(list(
      kind = "result",
      result = x,
      artifact_root = NULL,
      spec_path = NULL,
      paths = NULL
    ))
  }

  if (!is.character(x) || length(x) != 1L || !nzchar(x)) {
    stop("x must be a pls_result, pipeline spec path, artifact root, or pls_result.rds path", call. = FALSE)
  }

  path <- normalizePath(path.expand(x), winslash = "/", mustWork = FALSE)
  if (!file.exists(path) && !dir.exists(path)) {
    stop("Report input does not exist: ", x, call. = FALSE)
  }

  if (dir.exists(path)) {
    result_path <- file.path(path, "pls", "pls_result.rds")
    if (!file.exists(result_path)) {
      stop("Artifact root does not contain pls/pls_result.rds: ", path, call. = FALSE)
    }
    return(list(
      kind = "artifact_root",
      result = readRDS(result_path),
      artifact_root = path,
      spec_path = NULL,
      paths = .report_paths_from_root(path)
    ))
  }

  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("yml", "yaml")) {
    spec <- read_pipeline_spec(path)
    paths <- .pipeline_artifact_paths(spec)
    if (!file.exists(paths$pls_result_rds)) {
      stop("Pipeline spec resolves to an output root without a completed pls_result.rds", call. = FALSE)
    }
    return(list(
      kind = "pipeline_spec",
      result = readRDS(paths$pls_result_rds),
      artifact_root = paths$root,
      spec_path = spec$.spec_path %||% path,
      paths = paths
    ))
  }

  if (ext == "rds") {
    obj <- readRDS(path)
    if (!inherits(obj, "pls_result")) {
      stop("RDS input must contain a pls_result object", call. = FALSE)
    }
    artifact_root <- .report_guess_root_from_result_path(path)
    return(list(
      kind = "result_rds",
      result = obj,
      artifact_root = artifact_root,
      spec_path = NULL,
      paths = if (is.null(artifact_root)) NULL else .report_paths_from_root(artifact_root)
    ))
  }

  stop("Unsupported report input: ", x, call. = FALSE)
}

.report_paths_from_root <- function(root) {
  root <- normalizePath(root, winslash = "/", mustWork = FALSE)
  list(
    root = root,
    validation_tsv = file.path(root, "validation.tsv"),
    discovery_tsv = file.path(root, "discovery", "study_manifest.tsv"),
    firstlevel_plan_tsv = file.path(root, "firstlevel", "work_plan.tsv"),
    firstlevel_dir = file.path(root, "firstlevel", "work"),
    pls_plan_rds = file.path(root, "pls", "pls_plan.rds"),
    pls_manifest_tsv = file.path(root, "pls", "pls_manifest.tsv"),
    pls_result_rds = file.path(root, "pls", "pls_result.rds"),
    pls_summary_tsv = file.path(root, "pls", "pls_summary.tsv"),
    pipeline_summary_tsv = file.path(root, "pipeline_summary.tsv"),
    report_dir = file.path(root, "reports")
  )
}

.report_guess_root_from_result_path <- function(path) {
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  parent <- dirname(path)
  if (basename(parent) == "pls") {
    return(dirname(parent))
  }
  NULL
}

.report_default_output_file <- function(context, output_format) {
  ext <- switch(
    output_format,
    html = "html",
    pdf = "pdf",
    docx = "docx",
    output_format
  )

  if (!is.null(context$artifact_root)) {
    report_dir <- file.path(context$artifact_root, "reports")
    dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)
    return(file.path(report_dir, paste0("pls_report.", ext)))
  }

  file.path(getwd(), paste0("pls_report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".", ext))
}

.report_context_payload <- function(context, title, author, include_brain, bsr_threshold, p_threshold) {
  result <- context$result
  paths <- context$paths

  validation <- if (!is.null(paths) && file.exists(paths$validation_tsv)) {
    .pipeline_read_tsv(paths$validation_tsv)
  } else NULL
  discovery <- if (!is.null(paths) && file.exists(paths$discovery_tsv)) {
    .pipeline_read_tsv(paths$discovery_tsv, root = paths$root, path_cols = c("scan_file", "events_file", "mask_file"))
  } else NULL
  pipeline_summary <- if (!is.null(paths) && file.exists(paths$pipeline_summary_tsv)) {
    .pipeline_read_tsv(paths$pipeline_summary_tsv, root = paths$root, path_cols = c("file"))
  } else NULL
  pls_summary <- if (!is.null(paths) && file.exists(paths$pls_summary_tsv)) {
    .pipeline_read_tsv(paths$pls_summary_tsv)
  } else NULL
  pls_manifest <- if (!is.null(paths) && file.exists(paths$pls_manifest_tsv)) {
    .pipeline_read_tsv(paths$pls_manifest_tsv, root = paths$root, path_cols = c("file"))
  } else NULL

  list(
    metadata = list(
      title = title,
      author = author,
      artifact_root = context$artifact_root,
      spec_path = context$spec_path,
      input_kind = context$kind,
      generated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
      package_version = as.character(utils::packageVersion("plsrri")),
      include_brain = isTRUE(include_brain) && !is.null(result$mask),
      bsr_threshold = bsr_threshold,
      p_threshold = p_threshold
    ),
    validation = validation,
    discovery = discovery,
    pipeline_summary = pipeline_summary,
    pls_summary = pls_summary,
    pls_manifest = pls_manifest
  )
}

.report_render_common <- function(context,
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
  if (!requireNamespace("quarto", quietly = TRUE)) {
    stop("Package 'quarto' is required for report generation. Install it with install.packages('quarto')", call. = FALSE)
  }

  output_format <- match.arg(output_format, c("html", "pdf", "docx"))
  template_path <- .report_template_path(template)
  output_file <- output_file %||% .report_default_output_file(context, output_format)
  output_file <- normalizePath(path.expand(output_file), winslash = "/", mustWork = FALSE)
  dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)

  temp_dir <- tempfile("pls_report_")
  dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  result_file <- file.path(temp_dir, "pls_result.rds")
  context_file <- file.path(temp_dir, "report_context.rds")
  report_qmd <- file.path(temp_dir, "report.qmd")

  saveRDS(context$result, result_file)
  saveRDS(
    .report_context_payload(
      context = context,
      title = title,
      author = author,
      include_brain = include_brain,
      bsr_threshold = bsr_threshold,
      p_threshold = p_threshold
    ),
    context_file
  )
  .report_stage_template(template_path, report_qmd)

  quarto::quarto_render(
    input = report_qmd,
    output_file = basename(output_file),
    output_format = output_format,
    execute_params = list(
      result_file = result_file,
      context_file = context_file,
      title = title,
      author = author,
      include_brain = isTRUE(include_brain) && !is.null(context$result$mask),
      bsr_threshold = bsr_threshold,
      p_threshold = p_threshold,
      method = context$result$method,
      n_lv = length(context$result$s),
      has_perm = !is.null(context$result$perm_result),
      has_boot = !is.null(context$result$boot_result)
    ),
    ...
  )

  rendered <- file.path(temp_dir, basename(output_file))
  if (!file.exists(rendered)) {
    stop("Quarto did not produce the expected output file: ", rendered, call. = FALSE)
  }
  file.copy(rendered, output_file, overwrite = TRUE)

  if (isTRUE(open) && interactive()) {
    utils::browseURL(output_file)
  }

  cli::cli_alert_success("Report generated: {output_file}")
  invisible(output_file)
}

#' Render PLS Report
#'
#' @description
#' Generate a Quarto report from either a `pls_result` object or a saved
#' `plscli` artifact location. Character inputs may be:
#' - a pipeline YAML specification
#' - an artifact root containing `pls/pls_result.rds`
#' - a `pls_result.rds` path
#'
#' @param x A `pls_result` object or a path to pipeline/reportable artifacts.
#' @param output_file Output file path (default: artifact-root `reports/` or a timestamped local file)
#' @param output_format Output format: `"html"` (default), `"pdf"`, or `"docx"`
#' @param title Report title
#' @param author Report author
#' @param template Custom Quarto template path (NULL = built-in)
#' @param include_brain Include brain map visualizations when a mask is available
#' @param bsr_threshold BSR threshold for reliability summaries
#' @param p_threshold P-value threshold for significance summaries
#' @param open Open the report after rendering when interactive
#' @param ... Additional arguments passed to `quarto::quarto_render()`
#'
#' @return Path to rendered report, invisibly.
#' @export
render_report <- function(x,
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
  UseMethod("render_report")
}

#' @export
render_report.pls_result <- function(x,
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
  .report_render_common(
    context = .report_detect_input(x),
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

#' @export
render_report.character <- function(x,
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
  .report_render_common(
    context = .report_detect_input(x),
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

#' Render a Report from `plscli` Artifacts
#'
#' @param spec Path to a pipeline YAML file or a validated pipeline spec list.
#' @param output_file Optional output file path.
#' @param output_format Output format: `"html"`, `"pdf"`, or `"docx"`.
#' @param title Report title.
#' @param author Report author.
#' @param open Open the rendered report if interactive.
#' @param ... Additional arguments passed to [render_report()].
#'
#' @return Path to rendered report, invisibly.
#' @export
pipeline_render_report <- function(spec,
                                   output_file = NULL,
                                   output_format = "html",
                                   title = "PLS Pipeline Report",
                                   author = Sys.info()[["user"]],
                                   open = FALSE,
                                   ...) {
  spec_obj <- read_pipeline_spec(spec)
  render_report(
    spec_obj$.spec_path %||% .pipeline_output_root(spec_obj),
    output_file = output_file,
    output_format = output_format,
    title = title,
    author = author,
    open = open,
    ...
  )
}

#' Create Temporary Report Template
#'
#' @keywords internal
.create_temp_template <- function() {
  template <- tempfile(fileext = ".qmd")
  built_in <- system.file("quarto", "pls_report.qmd", package = "plsrri")
  if (nzchar(built_in)) {
    file.copy(built_in, template, overwrite = TRUE)
    return(template)
  }
  stop("Built-in Quarto report template is not available", call. = FALSE)
}

#' Write Results to File
#'
#' @description
#' Exports PLS results to various file formats.
#'
#' @param x A `pls_result` object
#' @param file Output file path
#' @param format Export format: "rds", "csv", "mat"
#' @param what What to export: "all", "salience", "bsr", "scores"
#' @param lv LV indices to export (NULL = all)
#'
#' @return Path to exported file (invisibly)
#' @export
write_results <- function(x, file, format = "rds", what = "all", lv = NULL) {
  UseMethod("write_results")
}

#' @export
write_results.pls_result <- function(x, file, format = "rds", what = "all", lv = NULL) {

  format <- match.arg(format, c("rds", "csv", "mat"))

  if (format == "rds") {
    if (what == "all") {
      saveRDS(x, file)
    } else {
      data <- switch(what,
        salience = salience(x, lv = lv),
        bsr = bsr(x, lv = lv),
        scores = scores(x, lv = lv)
      )
      saveRDS(data, file)
    }
  } else if (format == "csv") {
    data <- switch(what,
      salience = salience(x, lv = lv),
      bsr = bsr(x, lv = lv),
      scores = scores(x, lv = lv),
      all = {
        list(
          salience = salience(x),
          scores = scores(x)
        )
      }
    )

    if (is.list(data) && !is.matrix(data)) {
      for (nm in names(data)) {
        out_file <- sub("\\.csv$", paste0("_", nm, ".csv"), file)
        utils::write.csv(data[[nm]], out_file, row.names = FALSE)
      }
    } else {
      utils::write.csv(data, file, row.names = FALSE)
    }
  } else if (format == "mat") {
    if (!requireNamespace("R.matlab", quietly = TRUE)) {
      stop("Package R.matlab required for .mat export")
    }

    mat_list <- list(
      con = file,
      u = x$u,
      s = x$s,
      v = x$v,
      usc = x$usc,
      vsc = x$vsc
    )

    if (!is.null(x$perm_result)) {
      mat_list$sprob <- x$perm_result$sprob
    }

    if (!is.null(x$boot_result)) {
      mat_list$compare_u <- x$boot_result$compare_u
    }

    do.call(R.matlab::writeMat, mat_list)
  }

  cli::cli_alert_success("Results exported to: {file}")
  invisible(file)
}
