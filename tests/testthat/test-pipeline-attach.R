# Tests for the pipeline attach API (pipeline_attach.R)

# ---------------------------------------------------------------------------
# Helper: build a synthetic pipeline output root
# ---------------------------------------------------------------------------

.make_attach_root <- function(n_subjects = 2,
                              labels = c("face", "scene"),
                              use_relative_paths = TRUE,
                              write_nifti = FALSE) {
  tmp <- tempfile("attach-test-")
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

  subjects <- sprintf("%02d", seq_len(n_subjects))
  work_ids <- sprintf("w%04d", seq_len(n_subjects))

  for (i in seq_len(n_subjects)) {
    wdir <- file.path(root, "firstlevel", "work", work_ids[i])
    dir.create(wdir, recursive = TRUE, showWarnings = FALSE)

    files <- character(length(labels))
    for (j in seq_along(labels)) {
      f <- file.path(wdir, paste0(labels[j], ".nii.gz"))
      if (write_nifti) {
        arr <- array(runif(8), dim = c(2, 2, 2))
        RNifti::writeNifti(arr, f)
      } else {
        file.create(f)
      }
      files[j] <- if (use_relative_paths) {
        paste0("firstlevel/work/", work_ids[i], "/", labels[j], ".nii.gz")
      } else {
        f
      }
    }

    manifest <- data.frame(
      work_id = work_ids[i],
      subject = subjects[i],
      group = "all",
      task = "toy",
      type = "estimates",
      label = labels,
      statistic = "estimate",
      file = files,
      mask_file = mask_file,
      stringsAsFactors = FALSE
    )
    utils::write.table(manifest, file.path(wdir, "maps.tsv"),
                       sep = "\t", quote = FALSE, row.names = FALSE)
  }

  plan_work_dir <- if (use_relative_paths) {
    paste0("firstlevel/work/", work_ids)
  } else {
    file.path(root, "firstlevel", "work", work_ids)
  }

  plan <- data.frame(
    subject = subjects,
    group = "all",
    task = "toy",
    work_id = work_ids,
    scan_count = 1L,
    work_dir = plan_work_dir,
    fit_file = file.path(plan_work_dir, "fit.rds"),
    manifest_file = file.path(plan_work_dir, "maps.tsv"),
    stringsAsFactors = FALSE
  )
  utils::write.table(plan, file.path(root, "firstlevel", "work_plan.tsv"),
                     sep = "\t", quote = FALSE, row.names = FALSE)

  list(root = root, tmp = tmp, mask_file = mask_file,
       subjects = subjects, labels = labels)
}


# ---------------------------------------------------------------------------
# pipeline_attach_summary
# ---------------------------------------------------------------------------

test_that("pipeline_attach_summary validates a well-formed output root", {
  info <- .make_attach_root()
  result <- pipeline_attach_summary(info$root)

 expect_true(result$valid)
  expect_length(result$errors, 0)
  expect_equal(result$summary$n_subjects, 2L)
  expect_equal(result$summary$n_groups, 1L)
  expect_equal(result$summary$groups, "all")
  expect_equal(result$summary$types, "estimates")
  expect_equal(result$summary$statistics, "estimate")
  expect_equal(sort(result$summary$labels), c("face", "scene"))
})

test_that("pipeline_attach_summary resolves relative paths to absolute", {
  info <- .make_attach_root(use_relative_paths = TRUE)
  result <- pipeline_attach_summary(info$root)

  expect_true(result$valid)
  # All file paths in manifest should be absolute
  expect_true(all(startsWith(result$firstlevel_manifest$file, "/")))
  # Plan paths should also be absolute
  expect_true(all(startsWith(result$firstlevel_plan$work_dir, "/")))
  expect_true(all(startsWith(result$firstlevel_plan$manifest_file, "/")))
})

test_that("pipeline_attach_summary handles absolute paths (backward compat)", {
  info <- .make_attach_root(use_relative_paths = FALSE)
  result <- pipeline_attach_summary(info$root)

  expect_true(result$valid)
  expect_equal(result$summary$n_subjects, 2L)
  expect_true(all(startsWith(result$firstlevel_manifest$file, "/")))
})

test_that("pipeline_attach_summary rejects nonexistent root", {
  result <- pipeline_attach_summary("/nonexistent/path/xyz")
  expect_false(result$valid)
  expect_true(any(grepl("does not exist", result$errors)))
})

test_that("pipeline_attach_summary rejects root without work_plan.tsv", {
  tmp <- tempfile("attach-bad-")
  dir.create(file.path(tmp, "firstlevel", "work"), recursive = TRUE)
  result <- pipeline_attach_summary(tmp)

  expect_false(result$valid)
  expect_true(any(grepl("Missing firstlevel/work_plan.tsv", result$errors)))
})

test_that("pipeline_attach_summary rejects root without work/ directory", {
  tmp <- tempfile("attach-bad2-")
  dir.create(file.path(tmp, "firstlevel"), recursive = TRUE)
  utils::write.table(
    data.frame(x = 1), file.path(tmp, "firstlevel", "work_plan.tsv"),
    sep = "\t", quote = FALSE, row.names = FALSE
  )
  result <- pipeline_attach_summary(tmp)

  expect_false(result$valid)
  expect_true(any(grepl("Missing firstlevel/work", result$errors)))
})

test_that("pipeline_attach_summary supports remap for moved outputs", {
  info <- .make_attach_root(use_relative_paths = TRUE)

  # Move the root and use remap
  new_root <- tempfile("moved-")
  file.rename(info$root, new_root)

  result <- pipeline_attach_summary(new_root, remap = new_root)
  expect_true(result$valid)
  expect_equal(result$summary$n_subjects, 2L)
})


# ---------------------------------------------------------------------------
# pipeline_load_analysis_plan
# ---------------------------------------------------------------------------

test_that("pipeline_load_analysis_plan auto-detects type and statistic", {
  info <- .make_attach_root()
  plan <- pipeline_load_analysis_plan(info$root)

  expect_equal(plan$analysis, "pls")
  expect_equal(plan$input_type, "estimates")
  expect_equal(plan$statistic, "estimate")
  expect_true(is.data.frame(plan$manifest))
  expect_true(nrow(plan$manifest) > 0)
})

test_that("pipeline_load_analysis_plan respects explicit type/statistic", {
  info <- .make_attach_root()
  expect_error(
    pipeline_load_analysis_plan(info$root, input_type = "contrasts"),
    "No first-level outputs matched"
  )
})

test_that("pipeline_load_analysis_plan errors on bad root", {
  expect_error(
    pipeline_load_analysis_plan("/nonexistent/path"),
    "Cannot load analysis plan"
  )
})


# ---------------------------------------------------------------------------
# pipeline_build_pls_spec_from_ui
# ---------------------------------------------------------------------------

test_that("pipeline_build_pls_spec_from_ui builds a runnable spec", {
  skip_if_not_installed("RNifti")
  skip_if_not_installed("neuroim2")

  info <- .make_attach_root(write_nifti = TRUE)
  plan <- pipeline_load_analysis_plan(info$root)

  pls_opts <- list(method = "task", nperm = 0L, nboot = 0L)
  spec <- pipeline_build_pls_spec_from_ui(plan, pls_opts)

  expect_s3_class(spec, "pls_spec")
  expect_equal(spec$num_cond, 2L)
  expect_equal(length(spec$datamat_lst), 1L)  # 1 group
  expect_equal(spec$num_perm, 0L)
  expect_equal(spec$num_boot, 0L)
})

test_that("pipeline_build_pls_spec_from_ui accepts integer method", {
  skip_if_not_installed("RNifti")
  skip_if_not_installed("neuroim2")

  info <- .make_attach_root(write_nifti = TRUE)
  plan <- pipeline_load_analysis_plan(info$root)

  pls_opts <- list(method = 1L, nperm = 0L, nboot = 0L)
  spec <- pipeline_build_pls_spec_from_ui(plan, pls_opts)

  expect_s3_class(spec, "pls_spec")
  expect_equal(spec$method, 1L)
})

# ---------------------------------------------------------------------------
# Canonical method mapping regression tests
# ---------------------------------------------------------------------------

test_that("pls_method_int_to_name maps all 6 methods correctly", {
  expect_equal(pls_method_int_to_name(1L), "task")
  expect_equal(pls_method_int_to_name(2L), "task_nonrotated")
  expect_equal(pls_method_int_to_name(3L), "behavior")
  expect_equal(pls_method_int_to_name(4L), "multiblock")
  expect_equal(pls_method_int_to_name(5L), "behavior_nonrotated")
  expect_equal(pls_method_int_to_name(6L), "multiblock_nonrotated")
})

test_that("pls_method_int_to_name rejects invalid integers", {
  expect_error(pls_method_int_to_name(0L), "Unknown PLS method")
  expect_error(pls_method_int_to_name(7L), "Unknown PLS method")
})

test_that("pls_method_label returns correct labels for integers 1-6", {
  expect_equal(pls_method_label(1L), "Mean-Centering Task PLS")
  expect_equal(pls_method_label(2L), "Non-Rotated Task PLS")
  expect_equal(pls_method_label(3L), "Regular Behavior PLS")
  expect_equal(pls_method_label(4L), "Regular Multiblock PLS")

  expect_equal(pls_method_label(5L), "Non-Rotated Behavior PLS")
  expect_equal(pls_method_label(6L), "Non-Rotated Multiblock PLS")
})

test_that("pls_method_label returns correct labels for string names", {
  expect_equal(pls_method_label("task"), "Mean-Centering Task PLS")
  expect_equal(pls_method_label("behavior"), "Regular Behavior PLS")
  expect_equal(pls_method_label("multiblock"), "Regular Multiblock PLS")
  expect_equal(pls_method_label("behavior_nonrotated"), "Non-Rotated Behavior PLS")
  expect_equal(pls_method_label("multiblock_nonrotated"), "Non-Rotated Multiblock PLS")
})

test_that("pipeline_build_pls_spec_from_ui maps integer methods 1-6 correctly", {
  skip_if_not_installed("RNifti")
  skip_if_not_installed("neuroim2")

  info <- .make_attach_root(write_nifti = TRUE)
  plan <- pipeline_load_analysis_plan(info$root)

  # Method 1 = task (already tested above, included for completeness)
  expected <- c(
    "1" = 1L, "2" = 2L, "3" = 3L,
    "4" = 4L, "5" = 5L, "6" = 6L
  )

  for (m in seq_len(6L)) {
    pls_opts <- list(method = m, nperm = 0L, nboot = 0L)
    spec <- suppressWarnings(pipeline_build_pls_spec_from_ui(plan, pls_opts))
    expect_equal(spec$method, unname(expected[as.character(m)]),
                 label = sprintf("integer method %d", m))
  }
})

test_that("pipeline_build_pls_spec_from_ui end-to-end with run()", {
  skip_if_not_installed("RNifti")
  skip_if_not_installed("neuroim2")

  info <- .make_attach_root(write_nifti = TRUE)
  plan <- pipeline_load_analysis_plan(info$root)

  pls_opts <- list(method = "task", nperm = 0L, nboot = 0L, meancentering = 0L)
  spec <- pipeline_build_pls_spec_from_ui(plan, pls_opts)
  result <- run(spec)

  expect_s3_class(result, "pls_result")
  expect_equal(length(result$s), spec$num_cond)
})


# ---------------------------------------------------------------------------
# Relative path round-trip helpers
# ---------------------------------------------------------------------------

test_that("relativize and resolve are inverse operations for paths under root", {
  root <- "/tmp/plscli-out"
  paths <- c(
    "/tmp/plscli-out/firstlevel/work/w0001/map.nii.gz",
    "/tmp/plscli-out/pls/manifest.tsv"
  )

  rel <- plsrri:::.pipeline_relativize_path(paths, root)
  expect_false(any(startsWith(rel, "/")))

  abs <- plsrri:::.pipeline_resolve_path(rel, root)
  expect_equal(abs, paths)
})

test_that("relativize preserves external absolute paths", {
  root <- "/tmp/plscli-out"
  ext_path <- "/external/bids/mask.nii.gz"

  rel <- plsrri:::.pipeline_relativize_path(ext_path, root)
  expect_equal(rel, ext_path)
})

test_that("resolve preserves existing absolute paths", {
  root <- "/tmp/plscli-out"
  abs_path <- "/already/absolute.nii.gz"

  resolved <- plsrri:::.pipeline_resolve_path(abs_path, root)
  expect_equal(resolved, abs_path)
})

test_that("relativize and resolve handle NA values", {
  root <- "/tmp"
  expect_equal(plsrri:::.pipeline_relativize_path(NA_character_, root), NA_character_)
  expect_equal(plsrri:::.pipeline_resolve_path(NA_character_, root), NA_character_)
})
