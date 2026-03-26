testthat::skip_if_not_installed("shiny")
testthat::skip_if_not_installed("shinyjs")
testthat::skip_if_not_installed("shinyFiles")
testthat::skip_if_not_installed("R6")

module_path_export <- {
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

source(file.path(module_path_export, "shiny_sources.R"), local = TRUE)
plsrri_source_shiny_files(module_path_export, local = TRUE)

.make_export_spec <- function() {
  pls_spec() |>
    add_subjects(list(matrix(rnorm(12), nrow = 4)), groups = 2) |>
    add_conditions(2) |>
    configure(method = "task", nperm = 10, nboot = 5)
}

.make_pipeline_spec <- function(analyze_mode = "end_to_end") {
  build_bids_pipeline_spec_from_setup(
    bids_path = tempdir(),
    task = "stroop",
    space = "MNI152NLin2009cAsym",
    design_formula = "onset ~ hrf(condition, basis = 'fir')",
    output_root = file.path(tempdir(), "plsrri-yaml-out"),
    output_type = "estimates",
    output_statistic = "estimate",
    basis_pattern = "^(.*)_t([0-9]+)$",
    basis_order = c("0", "1", "2"),
    method = "multiblock_nonrotated",
    nperm = 25L,
    nboot = 10L,
    nsplit = 3L,
    clim = 90,
    meancentering = 1L,
    cormode = 2L,
    boot_type = "nonstrat",
    analyze_mode = analyze_mode
  )
}

make_test_state_rv_export <- function() {
  shiny::reactiveValues(
    step = 1L,
    max_step = 1L,
    spec = NULL,
    result = NULL,
    analysis_status = "ready"
  )
}

with_recent_dir <- function(code) {
  dir <- tempfile("recent-pipeline-")
  dir.create(dir, recursive = TRUE)
  old <- options(plsrri.shiny.recent_dir = dir)
  on.exit(options(old), add = TRUE)
  force(code)
}

test_that("setup_current_prepared_for_export suppresses non-exportable attach state", {
  prepared <- new_prepared_analysis(
    analysis_source = "attach",
    analyze_mode = "pls_only",
    pipeline_root = tempfile("attach-root-"),
    analysis_plan = list(input_type = "estimates", statistic = "estimate"),
    pls_input = list(type = "estimates", statistic = "estimate"),
    summary = list(n_subjects = 2L)
  )

  input <- list(data_source = "manual")
  local_rv <- list(
    analysis_source = "attach",
    prepared_analysis = prepared
  )

  expect_null(setup_current_prepared_for_export(input, local_rv))
})

test_that("setup_current_prepared_for_export returns pipeline-backed attach state unchanged", {
  prepared <- new_prepared_analysis(
    analysis_source = "attach",
    analyze_mode = "firstlevel_only",
    pipeline_spec = list(outputs = list(root = tempfile("out-"))),
    pipeline_root = tempfile("attach-root-"),
    analysis_plan = list(input_type = "estimates", statistic = "estimate"),
    pls_input = list(type = "estimates", statistic = "estimate"),
    summary = list(n_subjects = 2L)
  )

  input <- list(data_source = "manual")
  local_rv <- list(
    analysis_source = "attach",
    prepared_analysis = prepared
  )

  out <- setup_current_prepared_for_export(input, local_rv)

  expect_identical(out, prepared)
})

test_that("setup_current_prepared_for_export builds workstation pipeline exports from setup inputs", {
  bids_root <- tempfile("bids-export-")
  dir.create(bids_root, recursive = TRUE)
  writeLines('{"Name":"toy","BIDSVersion":"1.8.0"}', file.path(bids_root, "dataset_description.json"))

  input <- list(
    data_source = "bids",
    bids_use_pipeline = TRUE,
    bids_task = "stroop",
    bids_space = "MNI152NLin2009cAsym",
    bids_group_col = "all",
    bids_group_values = character(0),
    bids_design_formula = "onset ~ hrf(condition, basis = 'spmg1')",
    bids_pipeline_output_type = "estimates",
    bids_pipeline_statistic = "estimate",
    bids_basis_pattern = "",
    bids_basis_order = "",
    bids_analyze_mode = "end_to_end",
    method = "task",
    num_perm = 25L,
    num_boot = 10L,
    num_split = 0L,
    confidence = 95,
    meancentering = 0L,
    boot_type = "strat"
  )

  local_rv <- list(
    analysis_source = "direct",
    bids_path = bids_root,
    bids_part_df = NULL,
    bids_output_root = file.path(bids_root, "out"),
    bids_spec = .make_export_spec(),
    behav_data = NULL
  )

  out <- setup_current_prepared_for_export(input, local_rv)

  expect_s3_class(out, "prepared_analysis")
  expect_equal(out$analysis_source, "bids_in_app")
  expect_equal(out$analyze_mode, "end_to_end")
  expect_equal(out$pipeline_spec$dataset$bids_dir, normalizePath(bids_root, winslash = "/", mustWork = FALSE))
  expect_equal(out$pipeline_spec$pls$nperm, 25L)
})

test_that("setup_current_prepared_for_export returns NULL for non-pipeline direct setups", {
  input <- list(
    data_source = "manual",
    bids_use_pipeline = FALSE
  )
  local_rv <- list(
    analysis_source = "direct",
    bids_path = NULL,
    bids_part_df = NULL,
    bids_output_root = NULL,
    bids_spec = NULL,
    behav_data = NULL
  )

  expect_null(setup_current_prepared_for_export(input, local_rv))
})

test_that("pipeline_spec_to_setup_values recovers exported pipeline UI fields", {
  spec <- .make_pipeline_spec(analyze_mode = "firstlevel_only")
  vals <- pipeline_spec_to_setup_values(spec)

  expect_equal(vals$data_source, "bids")
  expect_true(vals$bids_use_pipeline)
  expect_equal(vals$bids_task, "stroop")
  expect_equal(vals$bids_space, "MNI152NLin2009cAsym")
  expect_equal(vals$bids_design_formula, "onset ~ hrf(condition, basis = 'fir')")
  expect_equal(vals$bids_basis_pattern, "^(.*)_t([0-9]+)$")
  expect_equal(vals$bids_basis_order, "0,1,2")
  expect_equal(vals$bids_analyze_mode, "firstlevel_only")
  expect_equal(vals$method, "multiblock")
  expect_equal(vals$num_perm, 25L)
  expect_equal(vals$num_boot, 10L)
  expect_equal(vals$num_split, 3L)
  expect_equal(vals$confidence, 90)
  expect_equal(vals$boot_type, "nonstrat")
})

test_that("setup_export_yaml_content writes YAML including UI analyze mode", {
  spec <- .make_pipeline_spec(analyze_mode = "firstlevel_only")
  prepared <- build_prepared_analysis_from_bids_pipeline(
    pipeline_spec = spec,
    pls_options = list(method = "task"),
    analyze_mode = "firstlevel_only"
  )
  out <- tempfile(fileext = ".yml")

  setup_export_yaml_content(prepared, out)
  txt <- paste(readLines(out, warn = FALSE), collapse = "\n")

  expect_match(txt, "ui:")
  expect_match(txt, "analyze_mode: firstlevel_only")
})

test_that("setup_cli_modal_body includes staged array guidance", {
  spec <- .make_pipeline_spec(analyze_mode = "end_to_end")
  prepared <- build_prepared_analysis_from_bids_pipeline(
    pipeline_spec = spec,
    pls_options = list(method = "task"),
    analyze_mode = "end_to_end"
  )

  body <- setup_cli_modal_body(prepared, spec_path = "analysis.yml")

  expect_match(body, "Local run:")
  expect_match(body, "Staged / HPC-friendly commands:")
  expect_match(body, "SLURM_ARRAY_TASK_ID")
  expect_match(body, "--work-id")
})

test_that("setup_apply_pipeline_yaml populates local state from imported YAML", {
  shiny::testServer(setup_server, args = list(state_rv = make_test_state_rv_export()), {
    spec <- .make_pipeline_spec(analyze_mode = "firstlevel_only")

    setup_apply_pipeline_yaml(session, local_rv, spec)

    expect_equal(local_rv$analysis_source, "direct")
    expect_equal(local_rv$bids_path, normalizePath(tempdir(), winslash = "/", mustWork = FALSE))
    expect_equal(local_rv$bids_output_root, normalizePath(file.path(tempdir(), "plsrri-yaml-out"), winslash = "/", mustWork = FALSE))
    expect_false(is.null(local_rv$pipeline_spec_loaded))
    expect_s3_class(local_rv$prepared_analysis, "prepared_analysis")
    expect_equal(local_rv$prepared_analysis$analysis_source, "bids_in_app")
    expect_equal(local_rv$prepared_analysis$analyze_mode, "firstlevel_only")
    expect_true(is.null(local_rv$pipeline_yaml_source) || is.na(local_rv$pipeline_yaml_source))
    expect_false(is.null(local_rv$pipeline_yaml_loaded_at))
  })
})

test_that("setup_read_pipeline_yaml_spec parses shinyFiles YAML selection", {
  spec <- .make_pipeline_spec(analyze_mode = "firstlevel_only")
  yaml_path <- tempfile("pipeline-ui-", tmpdir = ".", fileext = ".yml")
  on.exit(unlink(yaml_path), add = TRUE)
  writeLines(pipeline_spec_yaml_text(spec), yaml_path)

  out <- setup_read_pipeline_yaml_spec(
    file_info = list(root = "wd", files = list(basename(yaml_path))),
    roots = c(home = "~", wd = ".")
  )

  expect_equal(out$dataset$bids_dir, normalizePath(tempdir(), winslash = "/", mustWork = FALSE))
  expect_equal(out$design$formula, "onset ~ hrf(condition, basis = 'fir')")
  expect_equal(out$ui$analyze_mode, "firstlevel_only")
})

test_that("setup export CLI button opens modal with staged commands", {
  shiny::testServer(setup_server, args = list(state_rv = make_test_state_rv_export()), {
    local_rv$analysis_source <- "direct"
    local_rv$bids_path <- tempdir()
    local_rv$bids_output_root <- file.path(tempdir(), "plsrri-yaml-out")
    local_rv$bids_spec <- .make_export_spec()

    session$setInputs(
      analysis_source = "direct",
      data_source = "bids",
      bids_use_pipeline = TRUE,
      bids_task = "stroop",
      bids_space = "MNI152NLin2009cAsym",
      bids_group_col = "all",
      bids_group_values = character(0),
      bids_design_formula = "onset ~ hrf(condition, basis = 'fir')",
      bids_pipeline_output_type = "estimates",
      bids_pipeline_statistic = "estimate",
      bids_basis_pattern = "^(.*)_t([0-9]+)$",
      bids_basis_order = "0,1,2",
      bids_analyze_mode = "end_to_end",
      method = "task",
      num_perm = 25L,
      num_boot = 10L,
      num_split = 0L,
      confidence = 95,
      meancentering = "0",
      boot_type = "strat"
    )

    modal_env <- environment(setup_register_export_handlers)
    old_show_modal <- get("showModal", envir = modal_env, inherits = TRUE)
    captured <- NULL
    assign("showModal", function(ui, session = getDefaultReactiveDomain()) {
      captured <<- ui
      invisible(NULL)
    }, envir = modal_env)
    on.exit(assign("showModal", old_show_modal, envir = modal_env), add = TRUE)

    session$setInputs(btn_show_cli = 1)

    expect_false(is.null(captured))
    modal_txt <- paste(unlist(captured), collapse = " ")
    expect_match(modal_txt, "CLI Commands")
    expect_match(modal_txt, "SLURM_ARRAY_TASK_ID")
    expect_match(modal_txt, "pls-run")
  })
})

test_that("recent pipeline registry records and reloads snapshots", {
  with_recent_dir({
    spec <- .make_pipeline_spec(analyze_mode = "firstlevel_only")

    entry <- setup_record_recent_pipeline_spec(spec, source = "import")
    index <- setup_read_recent_pipeline_index()
    choices <- setup_recent_pipeline_choices(index)
    loaded <- setup_load_recent_pipeline_spec(entry$key)

    expect_true(file.exists(entry$file))
    expect_equal(nrow(index), 1L)
    expect_true(entry$key %in% index$key)
    expect_true(length(choices) == 1L)
    expect_equal(loaded$dataset$bids_dir, normalizePath(tempdir(), winslash = "/", mustWork = FALSE))
    expect_equal(loaded$ui$analyze_mode, "firstlevel_only")
  })
})

test_that("setup_export_yaml_content also records recent snapshot", {
  with_recent_dir({
    spec <- .make_pipeline_spec(analyze_mode = "end_to_end")
    prepared <- build_prepared_analysis_from_bids_pipeline(
      pipeline_spec = spec,
      pls_options = list(method = "task"),
      analyze_mode = "end_to_end"
    )
    out <- tempfile(fileext = ".yml")

    setup_export_yaml_content(prepared, out)
    index <- setup_read_recent_pipeline_index()

    expect_true(file.exists(out))
    expect_equal(nrow(index), 1L)
    expect_equal(index$source[[1]], "export")
    expect_true(file.exists(index$file[[1]]))
  })
})

test_that("setup export download button writes pipeline YAML", {
  shiny::testServer(setup_server, args = list(state_rv = make_test_state_rv_export()), {
    local_rv$analysis_source <- "direct"
    local_rv$bids_path <- tempdir()
    local_rv$bids_output_root <- file.path(tempdir(), "plsrri-yaml-out")
    local_rv$bids_spec <- .make_export_spec()

    session$setInputs(
      analysis_source = "direct",
      data_source = "bids",
      bids_use_pipeline = TRUE,
      bids_task = "stroop",
      bids_space = "MNI152NLin2009cAsym",
      bids_group_col = "all",
      bids_group_values = character(0),
      bids_design_formula = "onset ~ hrf(condition, basis = 'fir')",
      bids_pipeline_output_type = "estimates",
      bids_pipeline_statistic = "estimate",
      bids_basis_pattern = "^(.*)_t([0-9]+)$",
      bids_basis_order = "0,1,2",
      bids_analyze_mode = "firstlevel_only",
      method = "task",
      num_perm = 25L,
      num_boot = 10L,
      num_split = 0L,
      confidence = 95,
      meancentering = "0",
      boot_type = "strat"
    )

    path <- output$download_yaml
    expect_true(file.exists(path))
    txt <- paste(readLines(path, warn = FALSE), collapse = "\n")
    expect_match(txt, "dataset:")
    expect_match(txt, "ui:")
    expect_match(txt, "analyze_mode: firstlevel_only")
  })
})
