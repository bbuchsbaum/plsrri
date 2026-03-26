testthat::skip_if_not_installed("yaml")

# --- Tests for lightweight helper functions (no file I/O required) ---

test_that(".analysis_is_pipeline_spec_input returns TRUE for yml paths", {
  expect_true(plsrri:::.analysis_is_pipeline_spec_input("spec.yml"))
  expect_true(plsrri:::.analysis_is_pipeline_spec_input("spec.yaml"))
  expect_true(plsrri:::.analysis_is_pipeline_spec_input("/absolute/path/spec.yml"))
})

test_that(".analysis_is_pipeline_spec_input returns FALSE for non-spec inputs", {
  expect_false(plsrri:::.analysis_is_pipeline_spec_input("result.rds"))
  expect_false(plsrri:::.analysis_is_pipeline_spec_input("data.csv"))
  expect_false(plsrri:::.analysis_is_pipeline_spec_input(""))
  expect_false(plsrri:::.analysis_is_pipeline_spec_input(42L))
  expect_false(plsrri:::.analysis_is_pipeline_spec_input(NULL))
})

test_that(".analysis_is_pipeline_spec_input returns TRUE for plsrri_firstlevel_prep", {
  prep <- structure(list(), class = "plsrri_firstlevel_prep")
  expect_true(plsrri:::.analysis_is_pipeline_spec_input(prep))
})

test_that(".analysis_is_pipeline_spec_input returns TRUE for pipeline spec list", {
  spec_list <- list(dataset = list(), design = list())
  expect_true(plsrri:::.analysis_is_pipeline_spec_input(spec_list))
})

test_that(".analysis_is_pipeline_spec_input returns FALSE for plain list without spec keys", {
  plain_list <- list(a = 1, b = 2)
  expect_false(plsrri:::.analysis_is_pipeline_spec_input(plain_list))
})

test_that(".analysis_is_pipeline_spec_input returns FALSE for an analysis plan list", {
  plan <- list(analysis = "pls", manifest_kind = "voxel_map", manifest = data.frame())
  expect_false(plsrri:::.analysis_is_pipeline_spec_input(plan))
})

test_that(".analysis_is_plan detects a valid plan list", {
  plan <- list(analysis = "pls", manifest_kind = "voxel_map", manifest = data.frame())
  expect_true(plsrri:::.analysis_is_plan(plan))
})

test_that(".analysis_is_plan returns FALSE for incomplete lists", {
  expect_false(plsrri:::.analysis_is_plan(list(analysis = "pls")))
  expect_false(plsrri:::.analysis_is_plan(list()))
  expect_false(plsrri:::.analysis_is_plan("not a list"))
})

test_that(".analysis_default_pls_options returns expected defaults", {
  opts <- plsrri:::.analysis_default_pls_options()
  expect_equal(opts$method, "task")
  expect_equal(opts$nperm, 0L)
  expect_equal(opts$nboot, 0L)
  expect_equal(opts$nsplit, 0L)
  expect_equal(opts$clim, 95)
})

test_that(".analysis_merge_pls_options overrides base with overrides", {
  base <- list(method = "task", nperm = 0L, nboot = 0L, clim = 95)
  overrides <- list(nperm = 100L, clim = 99)
  result <- plsrri:::.analysis_merge_pls_options(base, overrides)
  expect_equal(result$method, "task")
  expect_equal(result$nperm, 100L)
  expect_equal(result$clim, 99)
  expect_equal(result$nboot, 0L)
})

test_that(".analysis_merge_pls_options handles NULL base gracefully", {
  result <- plsrri:::.analysis_merge_pls_options(NULL, list(nperm = 5L))
  expect_equal(result$nperm, 5L)
})

test_that(".analysis_merge_pls_options handles NULL overrides gracefully", {
  base <- list(method = "task", nperm = 0L)
  result <- plsrri:::.analysis_merge_pls_options(base, NULL)
  expect_equal(result, base)
})

test_that("print.plsrri_firstlevel_prep prints without error", {
  prep <- structure(
    list(
      artifact_root = "/tmp/fake_root",
      work_plan = data.frame(id = 1:3),
      firstlevel_manifest = NULL
    ),
    class = "plsrri_firstlevel_prep"
  )
  out <- capture.output(print(prep))
  expect_true(any(grepl("plsrri_firstlevel_prep", out)))
  expect_true(any(grepl("artifact_root", out)))
  expect_true(any(grepl("planned only", out)))
})

test_that("print.plsrri_firstlevel_prep shows output count when manifest present", {
  prep <- structure(
    list(
      artifact_root = "/tmp/fake_root",
      work_plan = data.frame(id = 1:2),
      firstlevel_manifest = data.frame(file = c("a.nii", "b.nii", "c.nii"))
    ),
    class = "plsrri_firstlevel_prep"
  )
  out <- capture.output(print(prep))
  expect_true(any(grepl("3", out)))
  expect_true(any(grepl("map", out)))
})

# --- End of lightweight helper tests ---


.make_analysis_api_root <- function(write_nifti = FALSE) {
  tmp <- tempfile("analysis-api-")
  dir.create(tmp, recursive = TRUE)
  root <- file.path(tmp, "out")

  mask_file <- file.path(tmp, "mask.nii.gz")
  if (write_nifti) {
    mask_arr <- array(0, dim = c(2, 2, 2))
    mask_arr[1, 1, 1] <- 1
    mask_arr[2, 1, 1] <- 1
    RNifti::writeNifti(mask_arr, mask_file)
  } else {
    file.create(mask_file)
  }

  work_ids <- c("w0001", "w0002")
  subjects <- c("01", "02")
  labels <- c("face", "scene")

  for (i in seq_along(work_ids)) {
    work_dir <- file.path(root, "firstlevel", "work", work_ids[[i]])
    dir.create(work_dir, recursive = TRUE, showWarnings = FALSE)

    files <- vapply(labels, function(label) {
      out <- file.path(work_dir, paste0(label, ".nii.gz"))
      if (write_nifti) {
        RNifti::writeNifti(array(runif(8), dim = c(2, 2, 2)), out)
      } else {
        file.create(out)
      }
      paste0("firstlevel/work/", work_ids[[i]], "/", label, ".nii.gz")
    }, character(1))

    manifest <- data.frame(
      work_id = work_ids[[i]],
      subject = subjects[[i]],
      group = "all",
      task = "toy",
      type = "estimates",
      label = labels,
      statistic = "estimate",
      file = files,
      mask_file = mask_file,
      stringsAsFactors = FALSE
    )
    utils::write.table(
      manifest,
      file.path(work_dir, "maps.tsv"),
      sep = "\t",
      quote = FALSE,
      row.names = FALSE
    )
  }

  plan <- data.frame(
    subject = subjects,
    group = "all",
    task = "toy",
    work_id = work_ids,
    scan_count = 1L,
    work_dir = paste0("firstlevel/work/", work_ids),
    fit_file = file.path(paste0("firstlevel/work/", work_ids), "fit.rds"),
    manifest_file = file.path(paste0("firstlevel/work/", work_ids), "maps.tsv"),
    stringsAsFactors = FALSE
  )
  utils::write.table(
    plan,
    file.path(root, "firstlevel", "work_plan.tsv"),
    sep = "\t",
    quote = FALSE,
    row.names = FALSE
  )

  spec_path <- file.path(root, "pipeline.yml")
  yaml::write_yaml(
    list(
      dataset = list(
        bids_dir = root,
        task = "toy"
      ),
      design = list(
        formula = "onset ~ hrf(condition, basis = 'spmg1')"
      ),
      outputs = list(root = root),
      pls = list(
        method = "task",
        nperm = 10L,
        nboot = 5L
      )
    ),
    spec_path
  )

  list(root = root, spec = spec_path)
}

test_that("prepare_firstlevel orchestrates staged first-level workflow", {
  calls <- character(0)
  spec_obj <- list(
    dataset = list(bids_dir = tempdir(), task = "toy"),
    design = list(formula = "~ 1"),
    first_level = list(output = list(type = "estimates", statistics = "estimate")),
    pls = list(method = "task"),
    outputs = list(root = file.path(tempdir(), "out"))
  )

  local_mocked_bindings(
    validate_pipeline_spec = function(x) {
      calls <<- c(calls, "validate_pipeline_spec")
      x
    },
    .pipeline_artifact_paths = function(spec) {
      list(root = spec$outputs$root)
    },
    pipeline_validate = function(spec) {
      calls <<- c(calls, "pipeline_validate")
      data.frame(ok = TRUE)
    },
    pipeline_discover = function(spec) {
      calls <<- c(calls, "pipeline_discover")
      data.frame(subject = "01")
    },
    pipeline_firstlevel_plan = function(spec) {
      calls <<- c(calls, "pipeline_firstlevel_plan")
      data.frame(work_id = "w0001")
    },
    pipeline_firstlevel_run = function(spec, work_id = NULL) {
      calls <<- c(calls, paste0("pipeline_firstlevel_run:", work_id %||% "all"))
      data.frame(file = "maps.tsv")
    },
    pipeline_summarize = function(spec) {
      calls <<- c(calls, "pipeline_summarize")
      data.frame(stage = "firstlevel")
    },
    .package = "plsrri"
  )

  prep <- prepare_firstlevel(spec_obj, work_id = "w0007")

  expect_s3_class(prep, "plsrri_firstlevel_prep")
  expect_equal(prep$artifact_root, spec_obj$outputs$root)
  expect_equal(calls, c(
    "validate_pipeline_spec",
    "pipeline_validate",
    "pipeline_discover",
    "pipeline_firstlevel_plan",
    "pipeline_firstlevel_run:w0007",
    "pipeline_summarize"
  ))
})

test_that("prepare_pls builds a runnable spec from an artifact root", {
  skip_if_not_installed("RNifti")
  skip_if_not_installed("neuroim2")

  info <- .make_analysis_api_root(write_nifti = TRUE)
  spec <- prepare_pls(info$root, pls_options = list(method = "task", nperm = 0L, nboot = 0L))

  expect_s3_class(spec, "pls_spec")
  expect_equal(spec$num_cond, 2L)
  expect_equal(length(spec$datamat_lst), 1L)
  expect_equal(spec$num_perm, 0L)
  expect_equal(spec$num_boot, 0L)
})

test_that("prepare_pls uses saved pipeline configuration for spec inputs", {
  info <- .make_analysis_api_root(write_nifti = FALSE)
  calls <- list()

  local_mocked_bindings(
    validate_pipeline_spec = function(x) read_pipeline_spec(x),
    pipeline_pls_plan = function(spec) {
      calls$planned <<- TRUE
      invisible(NULL)
    },
    .analysis_plan_from_pipeline_spec = function(spec) {
      calls$loaded <<- TRUE
      list(
        spec = spec,
        artifact_root = spec$outputs$root,
        plan = list(
          analysis = "pls",
          manifest_kind = "voxel_map",
          manifest = data.frame(
            group = "all",
            subject = "01",
            condition = "face",
            file = tempfile(fileext = ".nii.gz"),
            stringsAsFactors = FALSE
          ),
          mask_file = tempfile(fileext = ".nii.gz"),
          input_type = "estimates",
          statistic = "estimate"
        )
      )
    },
    pipeline_build_pls_spec_from_ui = function(plan, pls_options) {
      calls$options <<- pls_options
      structure(list(ok = TRUE, num_perm = pls_options$nperm), class = "pls_spec")
    },
    .package = "plsrri"
  )

  pspec <- prepare_pls(info$spec)
  expect_s3_class(pspec, "pls_spec")
  expect_true(isTRUE(calls$loaded))
  expect_equal(calls$options$method, "task")
  expect_equal(calls$options$nperm, 10L)
  expect_equal(calls$options$nboot, 5L)
})

test_that("run_pls dispatches to pipeline_pls_run for spec inputs without overrides", {
  calls <- character(0)
  spec_obj <- list(
    dataset = list(bids_dir = tempdir(), task = "toy"),
    design = list(formula = "~ 1"),
    first_level = list(output = list(type = "estimates", statistics = "estimate")),
    pls = list(method = "task"),
    outputs = list(root = file.path(tempdir(), "out"))
  )

  local_mocked_bindings(
    validate_pipeline_spec = function(x) {
      calls <<- c(calls, "validate_pipeline_spec")
      x
    },
    pipeline_pls_run = function(spec) {
      calls <<- c(calls, "pipeline_pls_run")
      structure(list(ok = TRUE), class = "pls_result")
    },
    .package = "plsrri"
  )

  result <- run_pls(spec_obj)
  expect_s3_class(result, "pls_result")
  expect_equal(calls, c("validate_pipeline_spec", "pipeline_pls_run"))
})

test_that("run_pls runs prepared specs in memory", {
  calls <- character(0)
  pspec <- structure(list(), class = "pls_spec")

  local_mocked_bindings(
    run = function(x, progress = FALSE, ...) {
      calls <<- c(calls, sprintf("run:%s", progress))
      structure(list(ok = TRUE), class = "pls_result")
    },
    .package = "plsrri"
  )

  result <- run_pls(pspec, progress = TRUE)
  expect_s3_class(result, "pls_result")
  expect_equal(calls, "run:TRUE")
})

test_that("render_pls_report forwards to render_report", {
  seen <- NULL
  local_mocked_bindings(
    render_report = function(x, output_file = NULL, output_format = "html", title = NULL, author = NULL, template = NULL, include_brain = TRUE, bsr_threshold = 3, p_threshold = 0.05, open = FALSE, ...) {
      seen <<- list(
        x = x,
        output_file = output_file,
        output_format = output_format,
        title = title,
        author = author,
        open = open
      )
      "/tmp/rendered.html"
    },
    .package = "plsrri"
  )

  out <- render_pls_report(
    "artifact-root",
    output_file = "report.pdf",
    output_format = "pdf",
    title = "Pretty Report",
    author = "Tester",
    open = TRUE
  )

  expect_equal(out, "/tmp/rendered.html")
  expect_equal(seen$x, "artifact-root")
  expect_equal(seen$output_file, "report.pdf")
  expect_equal(seen$output_format, "pdf")
  expect_equal(seen$title, "Pretty Report")
  expect_equal(seen$author, "Tester")
  expect_true(seen$open)
})

test_that("public analysis API supports synthetic end-to-end workflow", {
  skip_if_not_installed("yaml")
  skip_if_not_installed("RNifti")
  skip_if_not_installed("neuroim2")
  skip_if_not_installed("quarto")
  skip_if_not(nzchar(Sys.which("quarto")), "Quarto CLI is not available")

  tmp <- tempfile("analysis-e2e-")
  dir.create(tmp, recursive = TRUE)
  bids_dir <- file.path(tmp, "bids")
  out_root <- file.path(tmp, "out")
  dir.create(bids_dir, recursive = TRUE)

  mask_arr <- array(0, dim = c(2, 2, 2))
  mask_arr[1, 1, 1] <- 1
  mask_arr[2, 1, 1] <- 1
  mask_file <- file.path(tmp, "mask.nii.gz")
  RNifti::writeNifti(mask_arr, mask_file)

  subjects <- c("01", "02")
  scans <- file.path(bids_dir, paste0("sub-", subjects, "_task-toy_bold.nii.gz"))
  events <- file.path(bids_dir, paste0("sub-", subjects, "_task-toy_events.tsv"))
  invisible(file.create(scans))
  for (event_file in events) {
    writeLines(c("onset\tduration\tcondition", "0\t1\tface", "2\t1\tscene"), event_file)
  }

  spec_path <- file.path(tmp, "pipeline.yml")
  yaml::write_yaml(
    list(
      dataset = list(
        bids_dir = bids_dir,
        task = "toy",
        mask = mask_file
      ),
      design = list(
        formula = "onset ~ hrf(condition, basis = 'spmg1')"
      ),
      first_level = list(
        output = list(
          type = "estimates",
          statistics = "estimate"
        )
      ),
      pls = list(
        method = "task",
        nperm = 0L,
        nboot = 0L,
        nsplit = 0L
      ),
      outputs = list(
        root = out_root
      )
    ),
    spec_path
  )

  make_discovery <- function(spec) {
    spec <- validate_pipeline_spec(spec)
    paths <- .pipeline_artifact_paths(spec)
    df <- data.frame(
      subject = subjects,
      group = "all",
      task = "toy",
      run = "1",
      scan_file = scans,
      events_file = events,
      mask_file = mask_file,
      n_volumes = 4L,
      tr = 2,
      stringsAsFactors = FALSE
    )
    .pipeline_write_tsv(
      df,
      paths$discovery_tsv,
      root = paths$root,
      path_cols = c("scan_file", "events_file", "mask_file")
    )
    df
  }

  make_firstlevel_outputs <- function(spec, work_id = NULL) {
    spec <- validate_pipeline_spec(spec)
    paths <- .pipeline_artifact_paths(spec)
    plan <- .pipeline_read_tsv(
      paths$firstlevel_plan_tsv,
      root = paths$root,
      path_cols = c("work_dir", "fit_file", "manifest_file")
    )
    if (!is.null(work_id)) {
      plan <- plan[plan$work_id %in% work_id, , drop = FALSE]
    }

    label_values <- list(
      "01" = c(face = 2.5, scene = -2.5),
      "02" = c(face = 2.0, scene = -2.0)
    )

    manifests <- lapply(seq_len(nrow(plan)), function(i) {
      work_row <- plan[i, , drop = FALSE]
      dir.create(work_row$work_dir, recursive = TRUE, showWarnings = FALSE)

      rows <- lapply(names(label_values[[work_row$subject]]), function(label) {
        value <- unname(label_values[[work_row$subject]][[label]])
        arr <- array(0, dim = c(2, 2, 2))
        arr[1, 1, 1] <- value
        arr[2, 1, 1] <- value + 0.5
        out_file <- file.path(work_row$work_dir, paste0(label, ".nii.gz"))
        RNifti::writeNifti(arr, out_file)
        .new_firstlevel_manifest_row(
          work_id = work_row$work_id,
          subject = work_row$subject,
          group = work_row$group,
          task = work_row$task,
          type = "estimates",
          label = label,
          statistic = "estimate",
          file = out_file,
          mask_file = mask_file,
          condition = label
        )
      })

      manifest <- do.call(rbind, rows)
      .pipeline_write_tsv(
        manifest,
        work_row$manifest_file,
        root = paths$root,
        path_cols = c("file", "mask_file")
      )
      manifest
    })

    do.call(rbind, manifests)
  }

  local_mocked_bindings(
    pipeline_discover = make_discovery,
    pipeline_firstlevel_run = make_firstlevel_outputs,
    .package = "plsrri"
  )

  prep <- prepare_firstlevel(spec_path)
  expect_s3_class(prep, "plsrri_firstlevel_prep")
  expect_true(file.exists(file.path(out_root, "pipeline_summary.tsv")))
  expect_true(file.exists(file.path(out_root, "firstlevel", "work_plan.tsv")))

  pspec <- prepare_pls(prep)
  expect_s3_class(pspec, "pls_spec")
  expect_equal(pspec$num_cond, 2L)

  result <- run_pls(spec_path)
  expect_s3_class(result, "pls_result")
  expect_true(file.exists(file.path(out_root, "pls", "pls_result.rds")))

  report_file <- file.path(tmp, "pls-report.html")
  rendered <- render_pls_report(spec_path, output_file = report_file, include_brain = FALSE)
  expect_equal(rendered, report_file)
  expect_true(file.exists(report_file))

  html <- paste(readLines(report_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_match(html, "Overview")
  expect_match(html, "Provenance")
  expect_match(html, "Input Inventory")
})
