testthat::skip_if_not_installed("yaml")

# --- Lightweight helper tests (no quarto required) ---

test_that(".report_template_path returns built-in path or creates temp template", {
  path <- plsrri:::.report_template_path(NULL)
  expect_type(path, "character")
  expect_true(nzchar(path))
  expect_true(grepl("\\.qmd$", path))
})

test_that(".report_template_path returns normalised custom path", {
  tmp <- tempfile(fileext = ".qmd")
  writeLines("---\ntitle: test\n---\n", tmp)
  on.exit(unlink(tmp))
  path <- plsrri:::.report_template_path(tmp)
  expect_equal(path, normalizePath(tmp, winslash = "/", mustWork = TRUE))
})

test_that(".report_guess_root_from_result_path extracts root from pls/ subdirectory", {
  tmp <- file.path(tempdir(), "my_root", "pls")
  dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
  rds_path <- file.path(tmp, "pls_result.rds")
  saveRDS(list(), rds_path)
  on.exit(unlink(file.path(tempdir(), "my_root"), recursive = TRUE))

  root <- plsrri:::.report_guess_root_from_result_path(rds_path)
  expect_equal(root, normalizePath(file.path(tempdir(), "my_root"), winslash = "/"))
})

test_that(".report_guess_root_from_result_path returns NULL when not in pls/ dir", {
  tmp <- tempdir()
  rds_path <- file.path(tmp, "pls_result.rds")
  saveRDS(list(), rds_path)
  on.exit(unlink(rds_path))

  root <- plsrri:::.report_guess_root_from_result_path(rds_path)
  expect_null(root)
})

test_that(".report_default_output_file falls back to cwd when no artifact root", {
  ctx <- list(artifact_root = NULL)
  out <- plsrri:::.report_default_output_file(ctx, "html")
  expect_true(grepl("pls_report_.*\\.html$", out))
  expect_equal(dirname(out), normalizePath(getwd(), winslash = "/"))
})

test_that(".report_default_output_file uses artifact root reports/ dir", {
  tmp <- tempdir()
  ctx <- list(artifact_root = tmp)
  out <- plsrri:::.report_default_output_file(ctx, "pdf")
  expect_true(grepl("reports/pls_report\\.pdf$", out))
  expect_true(dir.exists(dirname(out)))
})

test_that(".report_detect_input returns 'result' kind for pls_result", {
  result <- make_mock_pls_result()
  ctx <- plsrri:::.report_detect_input(result)
  expect_equal(ctx$kind, "result")
  expect_s3_class(ctx$result, "pls_result")
  expect_null(ctx$artifact_root)
})

test_that(".report_detect_input errors on non-string non-result input", {
  expect_error(plsrri:::.report_detect_input(42L), "pls_result")
  expect_error(plsrri:::.report_detect_input(list()), "pls_result")
})

test_that(".report_detect_input errors on nonexistent path", {
  expect_error(plsrri:::.report_detect_input("/nonexistent/path/spec.yml"), "does not exist")
})

test_that(".report_detect_input errors on unsupported extension", {
  tmp <- tempfile(fileext = ".txt")
  writeLines("hello", tmp)
  on.exit(unlink(tmp))
  expect_error(plsrri:::.report_detect_input(tmp), "Unsupported")
})

test_that(".report_detect_input errors for .rds that is not a pls_result", {
  tmp <- tempfile(fileext = ".rds")
  saveRDS(list(not_a_result = TRUE), tmp)
  on.exit(unlink(tmp))
  expect_error(plsrri:::.report_detect_input(tmp), "pls_result")
})

test_that("write_results saves rds file", {
  set.seed(42)
  result <- make_mock_pls_result(n_voxels = 20, n_lv = 2, n_obs = 6)
  tmp <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp))
  write_results(result, tmp, format = "rds")
  expect_true(file.exists(tmp))
  loaded <- readRDS(tmp)
  expect_s3_class(loaded, "pls_result")
})

test_that("write_results saves csv files for salience", {
  set.seed(42)
  result <- make_mock_pls_result(n_voxels = 20, n_lv = 2, n_obs = 6)
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  write_results(result, tmp, format = "csv", what = "salience")
  expect_true(file.exists(tmp))
  df <- utils::read.csv(tmp)
  expect_equal(nrow(df), 20L)
})

test_that("write_results with format='csv' and what='all' writes multiple files", {
  set.seed(42)
  result <- make_mock_pls_result(n_voxels = 20, n_lv = 2, n_obs = 6)
  tmp <- tempfile(fileext = ".csv")
  on.exit({
    unlink(tmp)
    unlink(sub("\\.csv$", "_salience.csv", tmp))
    unlink(sub("\\.csv$", "_scores.csv", tmp))
  })
  write_results(result, tmp, format = "csv", what = "all")
  expect_true(file.exists(sub("\\.csv$", "_salience.csv", tmp)))
  expect_true(file.exists(sub("\\.csv$", "_scores.csv", tmp)))
})

test_that("write_results errors on bad format", {
  set.seed(42)
  result <- make_mock_pls_result(n_voxels = 10, n_lv = 2, n_obs = 4)
  expect_error(write_results(result, tempfile(), format = "xlsx"))
})

# --- End of lightweight helper tests ---

make_report_artifact_root <- function(root) {
  dir.create(file.path(root, "pls"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(root, "discovery"), recursive = TRUE, showWarnings = FALSE)

  result <- make_mock_pls_result(
    n_voxels = 120,
    n_lv = 3,
    n_obs = 12,
    include_boot = TRUE,
    include_perm = TRUE
  )
  result$num_subj_lst <- list(c(2L, 1L, 2L), c(3L, 2L, 1L))
  saveRDS(result, file.path(root, "pls", "pls_result.rds"))

  scan_file <- file.path(root, "sub-01_task-demo_bold.nii.gz")
  file.create(scan_file)

  .pipeline_write_tsv(
    data.frame(
      subject = "sub-01",
      task = "demo",
      scan_file = scan_file,
      stringsAsFactors = FALSE
    ),
    file.path(root, "discovery", "study_manifest.tsv"),
    root = root,
    path_cols = "scan_file"
  )

  .pipeline_write_tsv(
    data.frame(
      stage = c("discover", "pls-run"),
      status = c("completed", "completed"),
      file = c("discovery/study_manifest.tsv", "pls/pls_result.rds"),
      stringsAsFactors = FALSE
    ),
    file.path(root, "pipeline_summary.tsv"),
    root = root,
    path_cols = "file"
  )

  .pipeline_write_tsv(
    data.frame(
      group = "Group 1",
      condition = "Cond 1",
      file = scan_file,
      stringsAsFactors = FALSE
    ),
    file.path(root, "pls", "pls_manifest.tsv"),
    root = root,
    path_cols = "file"
  )

  spec_path <- file.path(root, "pipeline.yml")
  yaml::write_yaml(
    list(
      dataset = list(
        bids_dir = root,
        task = "demo"
      ),
      design = list(
        formula = "onset ~ hrf(condition, basis = 'spmg1')"
      ),
      outputs = list(
        root = root
      )
    ),
    spec_path
  )

  list(root = root, spec = spec_path, result = result)
}

test_that("report input detection resolves artifact roots, specs, and result files", {
  tmp <- withr::local_tempdir()
  paths <- make_report_artifact_root(tmp)

  artifact_ctx <- .report_detect_input(paths$root)
  expect_equal(artifact_ctx$kind, "artifact_root")
  expect_s3_class(artifact_ctx$result, "pls_result")
  expect_equal(artifact_ctx$artifact_root, normalizePath(paths$root, winslash = "/", mustWork = FALSE))

  spec_ctx <- .report_detect_input(paths$spec)
  expect_equal(spec_ctx$kind, "pipeline_spec")
  expect_equal(spec_ctx$artifact_root, artifact_ctx$artifact_root)
  expect_match(spec_ctx$spec_path, "pipeline[.]yml$")

  result_ctx <- .report_detect_input(file.path(paths$root, "pls", "pls_result.rds"))
  expect_equal(result_ctx$kind, "result_rds")
  expect_equal(result_ctx$artifact_root, artifact_ctx$artifact_root)
})

test_that("report context payload resolves relative artifact paths", {
  tmp <- withr::local_tempdir()
  paths <- make_report_artifact_root(tmp)
  ctx <- .report_detect_input(paths$root)

  payload <- .report_context_payload(
    context = ctx,
    title = "Report",
    author = "Tester",
    include_brain = FALSE,
    bsr_threshold = 3,
    p_threshold = 0.05
  )

  expect_equal(payload$metadata$title, "Report")
  expect_equal(payload$metadata$artifact_root, normalizePath(paths$root, winslash = "/", mustWork = FALSE))
  expect_true(file.exists(payload$discovery$scan_file[[1]]))
  expect_true(file.exists(payload$pipeline_summary$file[[1]]))
  expect_true(file.exists(payload$pls_manifest$file[[1]]))
})

test_that("default report output targets artifact reports directory", {
  tmp <- withr::local_tempdir()
  paths <- make_report_artifact_root(tmp)
  ctx <- .report_detect_input(paths$root)

  out <- .report_default_output_file(ctx, "html")
  expect_match(out, "reports/pls_report[.]html$")
  expect_true(dir.exists(dirname(out)))
})

test_that("pipeline_render_report forwards to render_report with spec path", {
  tmp <- withr::local_tempdir()
  paths <- make_report_artifact_root(tmp)

  seen <- NULL
  local_mocked_bindings(
    render_report = function(x, output_file = NULL, output_format = "html", title = NULL, author = NULL, open = FALSE, ...) {
      seen <<- list(
        x = x,
        output_file = output_file,
        output_format = output_format,
        title = title,
        author = author,
        open = open
      )
      "/tmp/mock-report.html"
    },
    .package = "plsrri"
  )

  out <- pipeline_render_report(
    paths$spec,
    output_format = "pdf",
    title = "Pipeline Report",
    author = "Unit Test",
    open = TRUE
  )

  expect_equal(out, "/tmp/mock-report.html")
  expect_equal(normalizePath(seen$x, winslash = "/", mustWork = FALSE), normalizePath(paths$spec, winslash = "/", mustWork = FALSE))
  expect_equal(seen$output_format, "pdf")
  expect_equal(seen$title, "Pipeline Report")
  expect_equal(seen$author, "Unit Test")
  expect_true(seen$open)
})

test_that("plscli report dispatches to render_report", {
  seen <- NULL
  local_mocked_bindings(
    render_report = function(x, output_file = NULL, output_format = "html", title = NULL, author = NULL, open = FALSE, ...) {
      seen <<- list(
        x = x,
        output_file = output_file,
        output_format = output_format,
        title = title,
        author = author,
        open = open
      )
      "/tmp/cli-report.html"
    },
    .package = "plsrri"
  )

  expect_invisible(
    plscli_main(c(
      "report",
      "--input", "/tmp/artifacts",
      "--format", "pdf",
      "--output", "/tmp/out.pdf",
      "--title", "CLI Report",
      "--author", "CLI Tester",
      "--open", "true"
    ))
  )

  expect_equal(seen$x, "/tmp/artifacts")
  expect_equal(seen$output_file, "/tmp/out.pdf")
  expect_equal(seen$output_format, "pdf")
  expect_equal(seen$title, "CLI Report")
  expect_equal(seen$author, "CLI Tester")
  expect_true(seen$open)
})

test_that("render_report can render html when quarto is available", {
  testthat::skip_if_not_installed("quarto")
  testthat::skip_if_not(nzchar(Sys.which("quarto")), "Quarto CLI is not available")

  out <- tempfile(fileext = ".html")
  render_report(
    make_mock_pls_result(include_boot = TRUE, include_perm = TRUE),
    output_file = out,
    output_format = "html",
    include_brain = FALSE
  )

  expect_true(file.exists(out))
  expect_gt(file.info(out)$size, 0)
})
