#' Pipeline Specification Helpers
#'
#' @description
#' Read, validate, and scaffold YAML specifications for `plscli`.
#'
#' @name pipeline-spec
NULL

.pipeline_nested <- function(x, path, default = NULL) {
  cur <- x
  for (key in path) {
    if (!is.list(cur) || is.null(cur[[key]])) return(default)
    cur <- cur[[key]]
  }
  cur
}

.pipeline_spec_dir <- function(spec) {
  if (!is.null(spec$.spec_dir)) return(spec$.spec_dir)
  getwd()
}

.pipeline_output_root <- function(spec) {
  root <- .pipeline_nested(spec, c("outputs", "root"), NULL)
  if (is.null(root) || !nzchar(as.character(root)[1])) {
    root <- file.path(.pipeline_spec_dir(spec), "plscli-out")
  }
  normalizePath(path.expand(root), winslash = "/", mustWork = FALSE)
}

.pipeline_artifact_paths <- function(spec) {
  root <- .pipeline_output_root(spec)
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
    pipeline_summary_tsv = file.path(root, "pipeline_summary.tsv")
  )
}

.pipeline_ensure_parent <- function(path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  invisible(path)
}

.pipeline_write_tsv <- function(x, path, root = NULL, path_cols = NULL) {
  .pipeline_ensure_parent(path)
  if (!is.null(root) && length(path_cols)) {
    x <- .pipeline_relativize_df(x, root, path_cols)
  }
  utils::write.table(x, file = path, sep = "\t", quote = FALSE, row.names = FALSE, col.names = TRUE)
  invisible(path)
}

.pipeline_read_tsv <- function(path, root = NULL, path_cols = NULL) {
  df <- utils::read.delim(path, sep = "\t", stringsAsFactors = FALSE, check.names = FALSE)
  if (!is.null(root) && length(path_cols)) {
    df <- .pipeline_resolve_df(df, root, path_cols)
  }
  df
}

.pipeline_read_table_file <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext == "csv") return(utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE))
  if (ext %in% c("tsv", "txt")) return(utils::read.delim(path, sep = "\t", stringsAsFactors = FALSE, check.names = FALSE))
  if (ext == "rds") return(readRDS(path))
  stop("Unsupported tabular file extension: .", ext, call. = FALSE)
}

.pipeline_as_chr <- function(x) {
  if (is.null(x)) return(character(0))
  as.character(unlist(x, recursive = TRUE, use.names = FALSE))
}

.pipeline_set_defaults <- function(spec) {
  if (is.null(spec$dataset)) spec$dataset <- list()
  if (is.null(spec$design)) spec$design <- list()
  if (is.null(spec$first_level)) spec$first_level <- list()
  if (is.null(spec$first_level$output)) spec$first_level$output <- list()
  if (is.null(spec$pls)) spec$pls <- list()
  if (is.null(spec$pls$input)) spec$pls$input <- list()
  if (is.null(spec$execution)) spec$execution <- list()
  if (is.null(spec$outputs)) spec$outputs <- list()

  spec$design$block <- spec$design$block %||% "~ run"
  spec$first_level$strategy <- spec$first_level$strategy %||% "runwise"
  spec$first_level$nchunks <- as.integer(spec$first_level$nchunks %||% 1L)
  spec$first_level$progress <- isTRUE(spec$first_level$progress)
  spec$first_level$save_fit <- !identical(spec$first_level$save_fit, FALSE)
  spec$first_level$output$type <- spec$first_level$output$type %||% "estimates"
  spec$first_level$output$statistics <- .pipeline_as_chr(spec$first_level$output$statistics %||% "estimate")
  spec$pls$method <- spec$pls$method %||% "task"
  spec$pls$input$type <- spec$pls$input$type %||% spec$first_level$output$type
  spec$pls$input$statistic <- spec$pls$input$statistic %||% spec$first_level$output$statistics[[1]]
  spec$execution$mode <- spec$execution$mode %||% "local"
  spec$execution$parallelism <- as.integer(spec$execution$parallelism %||% 1L)

  spec
}

#' Read a `plscli` Pipeline Specification
#'
#' @param x Path to a YAML file or an already-loaded list.
#'
#' @return A normalized pipeline specification list.
#' @export
read_pipeline_spec <- function(x) {
  if (is.list(x)) {
    spec <- x
    spec$.spec_path <- spec$.spec_path %||% NA_character_
    spec$.spec_dir <- spec$.spec_dir %||% getwd()
    return(.pipeline_set_defaults(spec))
  }

  if (!is.character(x) || length(x) != 1L) {
    stop("x must be a YAML path or a specification list", call. = FALSE)
  }
  if (!file.exists(x)) stop("Specification file does not exist: ", x, call. = FALSE)
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("Package 'yaml' is required for plscli specifications", call. = FALSE)
  }

  spec <- yaml::read_yaml(x)
  if (!is.list(spec)) stop("Pipeline specification must parse to a list", call. = FALSE)

  spec$.spec_path <- normalizePath(x, winslash = "/", mustWork = TRUE)
  spec$.spec_dir <- dirname(spec$.spec_path)
  .pipeline_set_defaults(spec)
}

#' Validate a `plscli` Pipeline Specification
#'
#' @param x Path to a YAML file or a specification list.
#'
#' @return The validated specification with defaults applied.
#' @export
validate_pipeline_spec <- function(x) {
  spec <- read_pipeline_spec(x)
  errors <- character(0)

  bids_dir <- .pipeline_nested(spec, c("dataset", "bids_dir"), NULL)
  if (is.null(bids_dir) || !nzchar(as.character(bids_dir)[1])) {
    errors <- c(errors, "dataset.bids_dir is required")
  } else {
    bids_dir <- normalizePath(path.expand(bids_dir), winslash = "/", mustWork = FALSE)
    spec$dataset$bids_dir <- bids_dir
    if (!dir.exists(bids_dir)) errors <- c(errors, sprintf("dataset.bids_dir does not exist: %s", bids_dir))
  }

  tasks <- .pipeline_as_chr(.pipeline_nested(spec, c("dataset", "task"), character(0)))
  if (!length(tasks)) {
    errors <- c(errors, "dataset.task must contain at least one task label")
  } else {
    spec$dataset$task <- tasks
  }

  if (is.null(.pipeline_nested(spec, c("design", "formula"), NULL))) {
    errors <- c(errors, "design.formula is required")
  }

  output_type <- .pipeline_nested(spec, c("first_level", "output", "type"), "estimates")
  if (!output_type %in% c("estimates", "contrasts", "F")) {
    errors <- c(errors, "first_level.output.type must be one of: estimates, contrasts, F")
  }

  fl_basis_pattern <- .pipeline_nested(spec, c("first_level", "output", "basis_pattern"), NULL)
  fl_basis_order <- .pipeline_as_chr(.pipeline_nested(spec, c("first_level", "output", "basis_order"), character(0)))
  if (!is.null(fl_basis_pattern) && !nzchar(as.character(fl_basis_pattern)[1])) {
    errors <- c(errors, "first_level.output.basis_pattern must be a non-empty regex when provided")
  }
  if (length(fl_basis_order) > 0) {
    spec$first_level$output$basis_order <- fl_basis_order
  }

  pls_method <- .pipeline_nested(spec, c("pls", "method"), "task")
  if (!tolower(as.character(pls_method)[1]) %in% c(
    "task", "task_nonrotated",
    "behavior", "behavior_nonrotated",
    "multiblock", "multiblock_nonrotated"
  )) {
    errors <- c(errors, "pls.method must be a supported plsrri method name")
  }

  exec_mode <- .pipeline_nested(spec, c("execution", "mode"), "local")
  if (!exec_mode %in% c("local", "array")) {
    errors <- c(errors, "execution.mode must be 'local' or 'array'")
  }

  pls_basis_pattern <- .pipeline_nested(spec, c("pls", "input", "basis_pattern"), NULL)
  pls_basis_order <- .pipeline_as_chr(.pipeline_nested(spec, c("pls", "input", "basis_order"), character(0)))
  if (!is.null(pls_basis_pattern) && !nzchar(as.character(pls_basis_pattern)[1])) {
    errors <- c(errors, "pls.input.basis_pattern must be a non-empty regex when provided")
  }
  if (!is.null(pls_basis_pattern)) {
    for (nm in c("condition_group", "basis_group")) {
      val <- .pipeline_nested(spec, c("pls", "input", nm), NULL)
      if (is.null(val) || is.na(suppressWarnings(as.integer(val))) || as.integer(val) < 1L) {
        errors <- c(errors, sprintf("pls.input.%s must be a positive integer when basis_pattern is provided", nm))
      }
    }
  }
  if (length(pls_basis_order) > 0) {
    spec$pls$input$basis_order <- pls_basis_order
  }

  behavior_file <- .pipeline_nested(spec, c("pls", "behavior", "file"), NULL)
  if (!is.null(behavior_file)) {
    behavior_path <- normalizePath(path.expand(behavior_file), winslash = "/", mustWork = FALSE)
    spec$pls$behavior$file <- behavior_path
    if (!file.exists(behavior_path)) {
      errors <- c(errors, sprintf("pls.behavior.file does not exist: %s", behavior_path))
    }
  }

  if (length(errors) > 0) {
    stop(paste(c("Pipeline specification validation failed:", errors), collapse = "\n  - "), call. = FALSE)
  }

  spec
}

#' Write an Example `plscli` YAML Specification
#'
#' @param path Output YAML path.
#'
#' @return The written path, invisibly.
#' @export
write_pipeline_template <- function(path) {
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("Package 'yaml' is required to write a pipeline template", call. = FALSE)
  }

  spec <- list(
    dataset = list(
      bids_dir = "/path/to/bids",
      task = "stroop",
      space = "MNI152NLin2009cAsym",
      group_column = "group"
    ),
    design = list(
      formula = "onset ~ hrf(condition, basis = 'spmg1')",
      block = "~ run"
    ),
    first_level = list(
      strategy = "runwise",
      nchunks = 1L,
      output = list(
        type = "estimates",
        statistics = c("estimate"),
        labels = NULL
      )
    ),
    pls = list(
      method = "task",
      input = list(
        type = "estimates",
        statistic = "estimate"
      ),
      nperm = 0L,
      nboot = 0L
    ),
    execution = list(
      mode = "local",
      parallelism = 1L
    ),
    outputs = list(
      root = "plscli-out"
    )
  )

  .pipeline_ensure_parent(path)
  writeLines(yaml::as.yaml(spec), path)
  invisible(path)
}
