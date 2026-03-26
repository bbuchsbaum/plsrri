testthat::skip_if_not_installed("shiny")

module_path_prepared <- {
  pkg_path <- system.file("shiny/R", package = "plsrri")
  if (pkg_path == "") {
    if (file.exists("inst/shiny/R")) {
      "inst/shiny/R"
    } else if (file.exists("../../inst/shiny/R")) {
      "../../inst/shiny/R"
    } else {
      file.path(getwd(), "inst/shiny/R")
    }
  } else {
    pkg_path
  }
}

source(file.path(module_path_prepared, "fct_prepared_analysis.R"), local = TRUE)

.make_minimal_pls_spec <- function() {
  pls_spec() |>
    add_subjects(list(matrix(rnorm(12), nrow = 4)), groups = 2) |>
    add_conditions(2) |>
    configure(method = "task", nperm = 0, nboot = 0)
}

# ---------------------------------------------------------------------------
# Method label regression tests
# ---------------------------------------------------------------------------

test_that("prepared_analysis_method_label returns correct labels for integers 1-6", {
  expect_equal(prepared_analysis_method_label(1L), "Mean-Centering Task PLS")
  expect_equal(prepared_analysis_method_label(2L), "Non-Rotated Task PLS")
  expect_equal(prepared_analysis_method_label(3L), "Regular Behavior PLS")
  expect_equal(prepared_analysis_method_label(4L), "Regular Multiblock PLS")
  expect_equal(prepared_analysis_method_label(5L), "Non-Rotated Behavior PLS")
  expect_equal(prepared_analysis_method_label(6L), "Non-Rotated Multiblock PLS")
})

test_that("prepared_analysis_method_label returns correct labels for string names", {
  expect_equal(prepared_analysis_method_label("task"), "Mean-Centering Task PLS")
  expect_equal(prepared_analysis_method_label("multiblock"), "Regular Multiblock PLS")
  expect_equal(prepared_analysis_method_label("behavior_nonrotated"), "Non-Rotated Behavior PLS")
})

# ---------------------------------------------------------------------------

test_that("new_prepared_analysis validates pls_only objects", {
  spec <- .make_minimal_pls_spec()

  prepared <- build_prepared_analysis_from_spec(spec, source = "direct")

  expect_s3_class(prepared, "prepared_analysis")
  expect_equal(prepared$analyze_mode, "pls_only")
  expect_equal(prepared$analysis_source, "direct")
})

test_that("is_valid_prepared_analysis distinguishes valid from invalid objects", {
  expect_true(is_valid_prepared_analysis(build_prepared_analysis_from_spec(.make_minimal_pls_spec())))
  expect_false(is_valid_prepared_analysis(list(analysis_source = "attach")))
})

test_that("attach prepared analyses can be valid before they are runnable", {
  prepared <- new_prepared_analysis(
    analysis_source = "attach",
    analyze_mode = "pls_only",
    pipeline_root = tempfile("attach-root-"),
    analysis_plan = list(input_type = "estimates", statistic = "estimate"),
    pls_input = list(type = "estimates", statistic = "estimate"),
    summary = list(n_subjects = 2L)
  )

  expect_true(is_valid_prepared_analysis(prepared))
  expect_false(is_runnable_prepared_analysis(prepared))
})

test_that("build_bids_pipeline_spec_from_setup produces a validated pipeline spec", {
  tmp <- tempfile("bids-ui-")
  dir.create(tmp, recursive = TRUE)
  file.create(file.path(tmp, "dataset_description.json"))

  spec <- build_bids_pipeline_spec_from_setup(
    bids_path = tmp,
    task = "stroop",
    space = "MNI152NLin2009cAsym",
    design_formula = "onset ~ hrf(condition, basis = 'spmg1')",
    output_root = file.path(tmp, "out"),
    method = "task",
    nperm = 10L,
    nboot = 5L
  )

  expect_equal(spec$dataset$bids_dir, normalizePath(tmp, winslash = "/", mustWork = FALSE))
  expect_equal(spec$design$formula, "onset ~ hrf(condition, basis = 'spmg1')")
  expect_equal(spec$pls$method, "task")
  expect_equal(spec$outputs$root, normalizePath(file.path(tmp, "out"), winslash = "/", mustWork = FALSE))
})

test_that("build_prepared_analysis_from_bids_pipeline stores pipeline mode", {
  tmp <- tempfile("bids-ui-out-")
  prepared <- build_prepared_analysis_from_bids_pipeline(
    pipeline_spec = list(
      dataset = list(bids_dir = tempdir(), task = "stroop"),
      design = list(formula = "onset ~ hrf(condition, basis = 'spmg1')"),
      first_level = list(output = list(type = "estimates", statistics = "estimate")),
      pls = list(
        method = "task",
        input = list(type = "estimates", statistic = "estimate"),
        nperm = 10L,
        nboot = 5L
      ),
      outputs = list(root = tmp)
    ),
    pls_options = list(method = "task", nperm = 10L, nboot = 5L),
    summary = list(n_groups = 1L, n_observations = 12L, n_conditions = 2L, n_features = 100L),
    analyze_mode = "end_to_end"
  )

  expect_s3_class(prepared, "prepared_analysis")
  expect_equal(prepared$analysis_source, "bids_in_app")
  expect_equal(prepared$analyze_mode, "end_to_end")
  expect_equal(prepared$pipeline_root, normalizePath(tmp, winslash = "/", mustWork = FALSE))
})

test_that("run_prepared_analysis handles pls_only mode", {
  skip_if_not_installed("neuroim2")

  spec <- pls_spec() |>
    add_subjects(list(matrix(rnorm(24), nrow = 8)), groups = 4) |>
    add_conditions(2) |>
    configure(method = "task", nperm = 0, nboot = 0)

  prepared <- build_prepared_analysis_from_spec(spec, source = "direct")
  out <- run_prepared_analysis(prepared)

  expect_s3_class(out$result, "pls_result")
  expect_s3_class(out$prepared_analysis, "prepared_analysis")
  expect_s3_class(out$spec, "pls_spec")
})

test_that("run_prepared_analysis handles firstlevel_only mode with staged runtime", {
  calls <- character(0)
  pipeline_root <- tempfile("pipeline-firstlevel-")

  prepared <- build_prepared_analysis_from_bids_pipeline(
    pipeline_spec = list(
      dataset = list(bids_dir = tempdir(), task = "stroop"),
      design = list(formula = "onset ~ hrf(condition, basis = 'spmg1')"),
      first_level = list(output = list(type = "estimates", statistics = "estimate")),
      pls = list(
        method = "task",
        input = list(type = "estimates", statistic = "estimate"),
        nperm = 10L,
        nboot = 5L
      ),
      outputs = list(root = pipeline_root)
    ),
    pls_options = list(method = "task", nperm = 10L, nboot = 5L),
    analyze_mode = "firstlevel_only"
  )

  firstlevel_plan <- data.frame(
    work_id = "w0001",
    subject = "01",
    manifest_file = file.path(pipeline_root, "firstlevel", "work", "w0001", "maps.tsv"),
    stringsAsFactors = FALSE
  )
  firstlevel_manifest <- data.frame(
    subject = "01",
    group = "all",
    condition = "face",
    file = file.path(pipeline_root, "firstlevel", "work", "w0001", "face.nii.gz"),
    stringsAsFactors = FALSE
  )
  attach_info <- list(
    valid = TRUE,
    errors = character(0),
    summary = list(
      n_groups = 1L,
      n_subjects = 1L,
      n_conditions = 1L,
      n_work_units = 1L,
      n_completed = 1L,
      labels = "face"
    ),
    firstlevel_plan = firstlevel_plan,
    firstlevel_manifest = firstlevel_manifest
  )
  plan <- list(input_type = "estimates", statistic = "estimate")

  runtime_env <- environment(run_prepared_analysis)
  old_runtime <- get("prepared_analysis_runtime", envir = runtime_env)
  assign("prepared_analysis_runtime", function() {
    list(
      run_pls = function(spec) stop("run_pls should not be called in firstlevel_only mode"),
      pipeline_validate = function(spec) calls <<- c(calls, "validate"),
      pipeline_discover = function(spec) calls <<- c(calls, "discover"),
      pipeline_firstlevel_plan = function(spec) calls <<- c(calls, "firstlevel_plan"),
      pipeline_firstlevel_run = function(spec) calls <<- c(calls, "firstlevel_run"),
      pipeline_attach_summary = function(root) {
        calls <<- c(calls, "attach_summary")
        attach_info
      },
      pipeline_load_analysis_plan = function(root, input_type, statistic) {
        calls <<- c(calls, sprintf("load_plan:%s:%s", input_type, statistic))
        plan
      },
      pipeline_build_pls_spec_from_ui = function(plan, pls_options) {
        stop("pipeline_build_pls_spec_from_ui should not be called in firstlevel_only mode")
      }
    )
  }, envir = runtime_env)
  on.exit(assign("prepared_analysis_runtime", old_runtime, envir = runtime_env), add = TRUE)

  out <- run_prepared_analysis(prepared)

  expect_null(out$result)
  expect_null(out$spec)
  expect_s3_class(out$prepared_analysis, "prepared_analysis")
  expect_equal(out$prepared_analysis$analysis_source, "attach")
  expect_equal(out$prepared_analysis$analyze_mode, "firstlevel_only")
  expect_equal(out$prepared_analysis$pipeline_root, pipeline_root)
  expect_equal(out$prepared_analysis$summary$n_completed, 1L)
  expect_equal(out$prepared_analysis$firstlevel_plan$work_id, "w0001")
  expect_equal(
    calls,
    c("validate", "discover", "firstlevel_plan", "firstlevel_run", "attach_summary", "load_plan:estimates:estimate")
  )
})

test_that("run_prepared_analysis handles end_to_end mode with staged runtime", {
  calls <- character(0)
  pipeline_root <- tempfile("pipeline-endtoend-")
  mock_spec <- .make_minimal_pls_spec()
  mock_result <- structure(list(ok = TRUE), class = "pls_result")

  prepared <- build_prepared_analysis_from_bids_pipeline(
    pipeline_spec = list(
      dataset = list(bids_dir = tempdir(), task = "stroop"),
      design = list(formula = "onset ~ hrf(condition, basis = 'spmg1')"),
      first_level = list(output = list(type = "estimates", statistics = "estimate")),
      pls = list(
        method = "task",
        input = list(type = "estimates", statistic = "estimate"),
        nperm = 10L,
        nboot = 5L
      ),
      outputs = list(root = pipeline_root)
    ),
    pls_options = list(method = "task", nperm = 10L, nboot = 5L),
    analyze_mode = "end_to_end"
  )

  firstlevel_plan <- data.frame(
    work_id = "w0001",
    subject = "01",
    manifest_file = file.path(pipeline_root, "firstlevel", "work", "w0001", "maps.tsv"),
    stringsAsFactors = FALSE
  )
  firstlevel_manifest <- data.frame(
    subject = "01",
    group = "all",
    condition = "face",
    file = file.path(pipeline_root, "firstlevel", "work", "w0001", "face.nii.gz"),
    stringsAsFactors = FALSE
  )
  attach_info <- list(
    valid = TRUE,
    errors = character(0),
    summary = list(
      n_groups = 1L,
      n_subjects = 2L,
      n_conditions = 2L,
      n_work_units = 2L,
      n_completed = 2L,
      labels = c("face", "scene")
    ),
    firstlevel_plan = firstlevel_plan,
    firstlevel_manifest = firstlevel_manifest
  )
  plan <- list(input_type = "estimates", statistic = "estimate")

  runtime_env <- environment(run_prepared_analysis)
  old_runtime <- get("prepared_analysis_runtime", envir = runtime_env)
  assign("prepared_analysis_runtime", function() {
    list(
      run_pls = function(spec) {
        calls <<- c(calls, "run_pls")
        expect_equal(spec, mock_spec)
        mock_result
      },
      pipeline_validate = function(spec) calls <<- c(calls, "validate"),
      pipeline_discover = function(spec) calls <<- c(calls, "discover"),
      pipeline_firstlevel_plan = function(spec) calls <<- c(calls, "firstlevel_plan"),
      pipeline_firstlevel_run = function(spec) calls <<- c(calls, "firstlevel_run"),
      pipeline_attach_summary = function(root) {
        calls <<- c(calls, "attach_summary")
        attach_info
      },
      pipeline_load_analysis_plan = function(root, input_type, statistic) {
        calls <<- c(calls, sprintf("load_plan:%s:%s", input_type, statistic))
        plan
      },
      pipeline_build_pls_spec_from_ui = function(plan_obj, pls_options) {
        calls <<- c(calls, sprintf("build_spec:%s", pls_options$method))
        mock_spec
      }
    )
  }, envir = runtime_env)
  on.exit(assign("prepared_analysis_runtime", old_runtime, envir = runtime_env), add = TRUE)

  out <- run_prepared_analysis(prepared)

  expect_s3_class(out$prepared_analysis, "prepared_analysis")
  expect_identical(out$result, mock_result)
  expect_equal(out$spec, mock_spec)
  expect_equal(out$prepared_analysis$analysis_source, "attach")
  expect_equal(out$prepared_analysis$analyze_mode, "pls_only")
  expect_equal(out$prepared_analysis$summary$n_subjects, 2L)
  expect_equal(
    calls,
    c(
      "validate",
      "discover",
      "firstlevel_plan",
      "firstlevel_run",
      "attach_summary",
      "load_plan:estimates:estimate",
      "build_spec:task",
      "run_pls"
    )
  )
})

test_that("pipeline_spec_yaml_text drops internal fields", {
  spec <- list(
    dataset = list(bids_dir = "/tmp/bids", task = "stroop"),
    outputs = list(root = "/tmp/out"),
    .spec_path = "ignore-me",
    .spec_dir = "ignore-me-too"
  )

  yaml_txt <- pipeline_spec_yaml_text(spec)

  expect_match(yaml_txt, "dataset:")
  expect_false(grepl(".spec_path", yaml_txt, fixed = TRUE))
  expect_false(grepl(".spec_dir", yaml_txt, fixed = TRUE))
})

test_that("prepared_analysis_cli_commands reflects analyze mode", {
  prepared <- build_prepared_analysis_from_bids_pipeline(
    pipeline_spec = list(
      dataset = list(bids_dir = tempdir(), task = "stroop"),
      design = list(formula = "onset ~ hrf(condition, basis = 'spmg1')"),
      first_level = list(output = list(type = "estimates", statistics = "estimate")),
      pls = list(
        method = "task",
        input = list(type = "estimates", statistic = "estimate"),
        nperm = 10L,
        nboot = 5L
      ),
      outputs = list(root = file.path(tempdir(), "plsrri-out"))
    ),
    pls_options = list(method = "task", nperm = 10L, nboot = 5L),
    analyze_mode = "firstlevel_only"
  )

  commands <- prepared_analysis_cli_commands(prepared, spec_path = "analysis.yml")

  expect_match(commands$local, "firstlevel-run")
  expect_true(any(grepl("firstlevel-plan", commands$staged, fixed = TRUE)))
  expect_false(any(grepl("pls-run", commands$staged, fixed = TRUE)))
  expect_true(any(grepl("SLURM_ARRAY_TASK_ID", commands$staged, fixed = TRUE)))
  expect_true(any(grepl("--work-id", commands$staged, fixed = TRUE)))
})
