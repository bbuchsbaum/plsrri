# Prepared-analysis helpers for Shiny pipeline workflows.

prepared_analysis_method_label <- function(method) {
  pls_method_label(method)
}

summarize_pls_spec_for_review <- function(spec) {
  if (is.null(spec)) return(NULL)
  list(
    n_groups = length(spec$datamat_lst),
    n_subjects = sum(vapply(spec$num_subj_lst, function(x) {
      if (length(x) == 1L) as.integer(x) else sum(as.integer(x))
    }, integer(1))),
    n_observations = sum(vapply(spec$datamat_lst, nrow, integer(1))),
    n_conditions = if (!is.null(spec$num_cond)) as.integer(spec$num_cond) else NA_integer_,
    n_features = if (length(spec$datamat_lst) > 0) ncol(spec$datamat_lst[[1]]) else NA_integer_,
    method_label = prepared_analysis_method_label(spec$method),
    nperm = as.integer(spec$num_perm %||% 0L),
    nboot = as.integer(spec$num_boot %||% 0L)
  )
}

summarize_attach_for_review <- function(summary_info, spec = NULL) {
  if (!is.null(spec)) {
    out <- summarize_pls_spec_for_review(spec)
  } else {
    out <- list(
      n_groups = summary_info$n_groups %||% NA_integer_,
      n_subjects = summary_info$n_subjects %||% NA_integer_,
      n_observations = summary_info$n_subjects %||% NA_integer_,
      n_conditions = summary_info$n_conditions %||% NA_integer_,
      n_features = NA_integer_,
      method_label = "Prepared First-Level Outputs",
      nperm = NA_integer_,
      nboot = NA_integer_
    )
  }
  out$labels <- summary_info$labels %||% character(0)
  out$n_subjects <- out$n_subjects %||% (summary_info$n_subjects %||% NA_integer_)
  out$n_work_units <- summary_info$n_work_units %||% NA_integer_
  out$n_completed <- summary_info$n_completed %||% NA_integer_
  out$has_basis <- isTRUE(summary_info$has_basis)
  out$n_lags <- summary_info$n_lags %||% NA_integer_
  out
}

validate_prepared_analysis <- function(x) {
  if (!is.list(x)) stop("prepared_analysis must be a list", call. = FALSE)

  source <- as.character(x$analysis_source %||% "")[1]
  mode <- as.character(x$analyze_mode %||% "")[1]

  if (!source %in% c("attach", "direct", "bids_in_app")) {
    stop("prepared_analysis$analysis_source must be one of: attach, direct, bids_in_app", call. = FALSE)
  }
  if (!mode %in% c("pls_only", "end_to_end", "firstlevel_only")) {
    stop("prepared_analysis$analyze_mode must be one of: pls_only, end_to_end, firstlevel_only", call. = FALSE)
  }

  if (identical(mode, "pls_only") && is.null(x$spec) && is.null(x$analysis_plan)) {
    stop("prepared_analysis in pls_only mode must include a pls_spec or analysis_plan", call. = FALSE)
  }
  if (mode %in% c("end_to_end", "firstlevel_only") && is.null(x$pipeline_spec)) {
    stop("prepared_analysis in pipeline mode must include pipeline_spec", call. = FALSE)
  }

  invisible(x)
}

validate_prepared_analysis_for_run <- function(x) {
  validate_prepared_analysis(x)

  mode <- as.character(x$analyze_mode %||% "")[1]
  if (identical(mode, "pls_only") && is.null(x$spec)) {
    stop("prepared_analysis in pls_only mode must include a runnable pls_spec", call. = FALSE)
  }

  invisible(x)
}

is_valid_prepared_analysis <- function(x) {
  tryCatch({
    validate_prepared_analysis(x)
    TRUE
  }, error = function(e) FALSE)
}

is_runnable_prepared_analysis <- function(x) {
  tryCatch({
    validate_prepared_analysis_for_run(x)
    TRUE
  }, error = function(e) FALSE)
}

new_prepared_analysis <- function(analysis_source,
                                  analyze_mode,
                                  spec = NULL,
                                  pipeline_spec = NULL,
                                  pipeline_root = NULL,
                                  analysis_plan = NULL,
                                  pls_options = NULL,
                                  pls_input = NULL,
                                  summary = NULL,
                                  firstlevel_plan = NULL,
                                  firstlevel_manifest = NULL) {
  out <- list(
    analysis_source = as.character(analysis_source)[1],
    analyze_mode = as.character(analyze_mode)[1],
    spec = spec,
    pipeline_spec = pipeline_spec,
    pipeline_root = if (is.null(pipeline_root)) NULL else as.character(pipeline_root)[1],
    analysis_plan = analysis_plan,
    pls_options = pls_options,
    pls_input = pls_input,
    summary = summary,
    firstlevel_plan = firstlevel_plan,
    firstlevel_manifest = firstlevel_manifest
  )
  class(out) <- c("prepared_analysis", class(out))
  validate_prepared_analysis(out)
  out
}

build_prepared_analysis_from_attach <- function(root,
                                                attach_info,
                                                plan,
                                                spec,
                                                pls_options = NULL) {
  new_prepared_analysis(
    analysis_source = "attach",
    analyze_mode = "pls_only",
    spec = spec,
    pipeline_root = root,
    analysis_plan = plan,
    pls_options = pls_options,
    pls_input = list(type = plan$input_type, statistic = plan$statistic),
    summary = summarize_attach_for_review(attach_info$summary, spec = spec),
    firstlevel_plan = attach_info$firstlevel_plan,
    firstlevel_manifest = attach_info$firstlevel_manifest
  )
}

build_prepared_analysis_from_spec <- function(spec, source = "direct") {
  new_prepared_analysis(
    analysis_source = source,
    analyze_mode = "pls_only",
    spec = spec,
    summary = summarize_pls_spec_for_review(spec)
  )
}

.parse_csv_chr <- function(x) {
  x <- as.character(x)[1]
  if (is.na(x) || !nzchar(trimws(x))) return(character(0))
  vals <- trimws(strsplit(x, ",", fixed = TRUE)[[1]])
  vals[nzchar(vals)]
}

build_bids_pipeline_spec_from_setup <- function(bids_path,
                                                task,
                                                space = NULL,
                                                participants_df = NULL,
                                                group_col = NULL,
                                                group_values = character(0),
                                                design_formula,
                                                output_root = NULL,
                                                output_type = "estimates",
                                                output_statistic = "estimate",
                                                basis_pattern = NULL,
                                                basis_order = character(0),
                                                method = "task",
                                                nperm = 0L,
                                                nboot = 0L,
                                                nsplit = 0L,
                                                clim = 95,
                                                meancentering = NULL,
                                                cormode = NULL,
                                                boot_type = NULL,
                                                is_struct = NULL,
                                                analyze_mode = NULL) {
  group_col <- as.character(group_col)[1]
  if (is.na(group_col) || !nzchar(group_col) || identical(group_col, "all")) {
    group_col <- NULL
  }

  subjects <- NULL
  if (!is.null(group_col) && length(group_values) > 0 && !is.null(participants_df) &&
      is.data.frame(participants_df) && all(c("participant_id", group_col) %in% names(participants_df))) {
    keep <- as.character(participants_df[[group_col]]) %in% as.character(group_values)
    subjects <- gsub("^sub-", "", as.character(participants_df$participant_id[keep]))
    subjects <- subjects[nzchar(subjects)]
  }

  basis_pattern <- as.character(basis_pattern)[1]
  if (is.na(basis_pattern) || !nzchar(trimws(basis_pattern))) basis_pattern <- NULL
  basis_order <- as.character(basis_order)
  basis_order <- basis_order[nzchar(basis_order)]

  if (is.null(output_root) || !nzchar(as.character(output_root)[1])) {
    output_root <- file.path(tempdir(), sprintf("plsrri-shiny-%s", format(Sys.time(), "%Y%m%d-%H%M%S")))
  }

  spec <- list(
    dataset = list(
      bids_dir = bids_path,
      task = task,
      space = space,
      group_column = group_col,
      subjects = subjects
    ),
    design = list(
      formula = design_formula,
      block = "~ run"
    ),
    first_level = list(
      strategy = "runwise",
      nchunks = 1L,
      progress = FALSE,
      save_fit = FALSE,
      output = list(
        type = output_type,
        statistics = output_statistic
      )
    ),
    pls = list(
      method = method,
      input = list(
        type = output_type,
        statistic = output_statistic
      ),
      nperm = as.integer(nperm),
      nboot = as.integer(nboot),
      nsplit = as.integer(nsplit),
      clim = as.numeric(clim),
      meancentering = meancentering,
      cormode = cormode,
      boot_type = boot_type,
      is_struct = is_struct
    ),
    outputs = list(
      root = output_root
    )
  )

  analyze_mode <- as.character(analyze_mode)[1]
  if (!is.na(analyze_mode) && nzchar(analyze_mode)) {
    spec$ui <- list(analyze_mode = analyze_mode)
  }

  if (!is.null(basis_pattern)) {
    spec$first_level$output$basis_pattern <- basis_pattern
    spec$first_level$output$condition_group <- 1L
    spec$first_level$output$basis_group <- 2L
    spec$pls$input$basis_pattern <- basis_pattern
    spec$pls$input$condition_group <- 1L
    spec$pls$input$basis_group <- 2L
    if (length(basis_order)) {
      spec$first_level$output$basis_order <- basis_order
      spec$pls$input$basis_order <- basis_order
    }
  }

  plsrri::validate_pipeline_spec(spec)
}

.pipeline_ui_method_key <- function(method) {
  key <- tolower(as.character(method)[1] %||% "task")
  switch(
    key,
    task = "task",
    task_nonrotated = "task",
    behavior = "behavior",
    behavior_nonrotated = "behavior",
    multiblock = "multiblock",
    multiblock_nonrotated = "multiblock",
    "task"
  )
}

pipeline_spec_to_setup_values <- function(spec) {
  spec <- plsrri::read_pipeline_spec(spec)

  task_vals <- plsrri:::.pipeline_as_chr(spec$dataset$task)
  basis_pattern <- spec$pls$input$basis_pattern %||% spec$first_level$output$basis_pattern %||% ""
  basis_order <- plsrri:::.pipeline_as_chr(spec$pls$input$basis_order %||% spec$first_level$output$basis_order %||% character(0))
  analyze_mode <- as.character(plsrri:::.pipeline_nested(spec, c("ui", "analyze_mode"), "end_to_end"))[1]

  list(
    analysis_source = "direct",
    data_source = "bids",
    bids_use_pipeline = TRUE,
    bids_task = if (length(task_vals)) task_vals[[1]] else "",
    bids_space = spec$dataset$space %||% "",
    bids_group_col = as.character(spec$dataset$group_column %||% "all")[1],
    bids_group_values = character(0),
    bids_design_formula = spec$design$formula %||% "",
    bids_pipeline_output_type = spec$first_level$output$type %||% "estimates",
    bids_pipeline_statistic = plsrri:::.pipeline_as_chr(spec$pls$input$statistic %||% spec$first_level$output$statistics %||% "estimate")[[1]],
    bids_basis_pattern = as.character(basis_pattern)[1],
    bids_basis_order = paste(basis_order, collapse = ","),
    bids_analyze_mode = if (analyze_mode %in% c("end_to_end", "firstlevel_only")) analyze_mode else "end_to_end",
    method = .pipeline_ui_method_key(spec$pls$method),
    num_perm = as.integer(spec$pls$nperm %||% 0L),
    num_boot = as.integer(spec$pls$nboot %||% 0L),
    num_split = as.integer(spec$pls$nsplit %||% 0L),
    confidence = as.numeric(spec$pls$clim %||% 95),
    meancentering = as.character(spec$pls$meancentering %||% 0L),
    cormode = as.character(spec$pls$cormode %||% 0L),
    boot_type = as.character(spec$pls$boot_type %||% "strat"),
    bids_path = spec$dataset$bids_dir,
    bids_output_root = plsrri:::.pipeline_output_root(spec),
    pipeline_spec = spec
  )
}

build_prepared_analysis_from_bids_pipeline <- function(pipeline_spec,
                                                       pls_options,
                                                       summary = NULL,
                                                       analyze_mode = "end_to_end") {
  validate_mode <- as.character(analyze_mode)[1]
  if (!validate_mode %in% c("end_to_end", "firstlevel_only")) {
    stop("BIDS pipeline mode must be end_to_end or firstlevel_only", call. = FALSE)
  }

  root <- plsrri:::.pipeline_output_root(pipeline_spec)
  new_prepared_analysis(
    analysis_source = "bids_in_app",
    analyze_mode = validate_mode,
    pipeline_spec = pipeline_spec,
    pipeline_root = root,
    pls_options = pls_options,
    pls_input = list(
      type = pipeline_spec$pls$input$type,
      statistic = pipeline_spec$pls$input$statistic
    ),
    summary = summary
  )
}

collect_pls_options_from_setup <- function(input, local_rv) {
  method <- as.character(input$method)[1]
  opts <- list(
    method = method,
    nperm = as.integer(input$num_perm),
    nboot = as.integer(input$num_boot),
    nsplit = as.integer(input$num_split),
    clim = as.numeric(input$confidence),
    meancentering = as.integer(input$meancentering),
    boot_type = input$boot_type
  )

  if (identical(method, "behavior") || identical(method, "multiblock")) {
    opts$behavior_data <- local_rv$behav_data
    opts$behavior_measures <- colnames(local_rv$behav_data)
    opts$cormode <- as.integer(input$cormode)
  }

  opts
}

prepared_analysis_runtime <- function() {
  list(
    run_pls = function(spec) {
      plsrri::run(spec, progress = TRUE)
    },
    pipeline_validate = plsrri::pipeline_validate,
    pipeline_discover = plsrri::pipeline_discover,
    pipeline_firstlevel_plan = plsrri::pipeline_firstlevel_plan,
    pipeline_firstlevel_run = plsrri::pipeline_firstlevel_run,
    pipeline_attach_summary = plsrri::pipeline_attach_summary,
    pipeline_load_analysis_plan = plsrri::pipeline_load_analysis_plan,
    pipeline_build_pls_spec_from_ui = plsrri::pipeline_build_pls_spec_from_ui
  )
}

run_prepared_analysis <- function(prepared) {
  validate_prepared_analysis_for_run(prepared)
  runtime <- prepared_analysis_runtime()

  if (identical(prepared$analyze_mode, "pls_only")) {
    result <- runtime$run_pls(prepared$spec)
    return(list(
      result = result,
      spec = prepared$spec,
      prepared_analysis = prepared,
      pipeline_root = prepared$pipeline_root
    ))
  }

  pipeline_spec <- prepared$pipeline_spec
  runtime$pipeline_validate(pipeline_spec)
  runtime$pipeline_discover(pipeline_spec)
  runtime$pipeline_firstlevel_plan(pipeline_spec)
  runtime$pipeline_firstlevel_run(pipeline_spec)

  attach_info <- runtime$pipeline_attach_summary(prepared$pipeline_root)
  plan <- runtime$pipeline_load_analysis_plan(
    prepared$pipeline_root,
    input_type = prepared$pls_input$type,
    statistic = prepared$pls_input$statistic
  )

  if (identical(prepared$analyze_mode, "firstlevel_only")) {
    next_prepared <- new_prepared_analysis(
      analysis_source = "attach",
      analyze_mode = "firstlevel_only",
      pipeline_spec = prepared$pipeline_spec,
      pipeline_root = prepared$pipeline_root,
      analysis_plan = plan,
      pls_options = prepared$pls_options,
      pls_input = prepared$pls_input,
      summary = summarize_attach_for_review(attach_info$summary, spec = NULL),
      firstlevel_plan = attach_info$firstlevel_plan,
      firstlevel_manifest = attach_info$firstlevel_manifest
    )
    return(list(
      result = NULL,
      spec = NULL,
      prepared_analysis = next_prepared,
      pipeline_root = prepared$pipeline_root
    ))
  }

  spec <- runtime$pipeline_build_pls_spec_from_ui(plan, prepared$pls_options)
  result <- runtime$run_pls(spec)
  next_prepared <- build_prepared_analysis_from_attach(
    root = prepared$pipeline_root,
    attach_info = attach_info,
    plan = plan,
    spec = spec,
    pls_options = prepared$pls_options
  )

  list(
    result = result,
    spec = spec,
    prepared_analysis = next_prepared,
    pipeline_root = prepared$pipeline_root
  )
}

prepared_analysis_review_summary <- function(prepared) {
  if (is.null(prepared)) return(NULL)
  validate_prepared_analysis(prepared)
  summary <- prepared$summary %||% list()
  summary$source_label <- switch(
    prepared$analysis_source,
    attach = "Attached first-level outputs",
    bids_in_app = "BIDS workstation pipeline",
    direct = "Direct data input",
    "Unknown source"
  )
  summary$mode_label <- switch(
    prepared$analyze_mode,
    pls_only = "PLS only",
    end_to_end = "First-level + PLS",
    firstlevel_only = "First-level only",
    prepared$analyze_mode
  )
  summary
}

prepared_analysis_pipeline_spec <- function(prepared) {
  validate_prepared_analysis(prepared)
  prepared$pipeline_spec
}

.drop_pipeline_internal_fields <- function(x) {
  if (!is.list(x)) return(x)
  x[setdiff(names(x), c(".spec_path", ".spec_dir"))]
}

pipeline_spec_yaml_text <- function(spec) {
  if (is.null(spec)) stop("A pipeline specification is required", call. = FALSE)
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("Package 'yaml' is required to export pipeline specifications", call. = FALSE)
  }
  yaml::as.yaml(.drop_pipeline_internal_fields(spec))
}

.r_cli_string <- function(x) {
  paste0("'", gsub("(['\\\\])", "\\\\\\1", as.character(x), perl = TRUE), "'")
}

.plscli_rscript_command <- function(command, spec_path, work_id = NULL) {
  args <- c(command, "--spec", spec_path)
  if (!is.null(work_id)) {
    args <- c(args, "--work-id", work_id)
  }
  sprintf(
    "Rscript -e \"plsrri::plscli_main(c(%s))\"",
    paste(vapply(args, .r_cli_string, character(1)), collapse = ", ")
  )
}

prepared_analysis_cli_commands <- function(prepared, spec_path = "analysis.yml") {
  spec <- prepared_analysis_pipeline_spec(prepared)
  if (is.null(spec)) {
    stop("This prepared analysis has no pipeline specification to export", call. = FALSE)
  }

  mode <- prepared$analyze_mode
  local_cmd <- switch(
    mode,
    pls_only = .plscli_rscript_command("pls-run", spec_path),
    end_to_end = .plscli_rscript_command("run", spec_path),
    firstlevel_only = .plscli_rscript_command("firstlevel-run", spec_path),
    .plscli_rscript_command("run", spec_path)
  )

  staged_cmds <- switch(
    mode,
    pls_only = c(
      .plscli_rscript_command("pls-plan", spec_path),
      .plscli_rscript_command("pls-run", spec_path)
    ),
    firstlevel_only = c(
      .plscli_rscript_command("validate", spec_path),
      .plscli_rscript_command("firstlevel-plan", spec_path),
      paste(
        "for WORK_ID in <work ids>; do",
        .plscli_rscript_command("firstlevel-run", spec_path, work_id = "$WORK_ID"),
        "; done"
      ),
      .plscli_rscript_command("summarize", spec_path),
      paste(
        "# SLURM array example:",
        .plscli_rscript_command("firstlevel-run", spec_path, work_id = "$SLURM_ARRAY_TASK_ID")
      )
    ),
    end_to_end = c(
      .plscli_rscript_command("validate", spec_path),
      .plscli_rscript_command("firstlevel-plan", spec_path),
      paste(
        "for WORK_ID in <work ids>; do",
        .plscli_rscript_command("firstlevel-run", spec_path, work_id = "$WORK_ID"),
        "; done"
      ),
      .plscli_rscript_command("pls-plan", spec_path),
      .plscli_rscript_command("pls-run", spec_path),
      .plscli_rscript_command("summarize", spec_path),
      paste(
        "# SLURM array example:",
        .plscli_rscript_command("firstlevel-run", spec_path, work_id = "$SLURM_ARRAY_TASK_ID")
      )
    ),
    character(0)
  )

  list(
    local = local_cmd,
    staged = staged_cmds
  )
}
