test_that("SDAM first-level tutorial manifest and smoke fit are valid", {
  skip_if_not_installed("neuroim2")

  repo_root <- normalizePath(file.path("..", ".."), winslash = "/", mustWork = FALSE)
  if (!dir.exists(file.path(repo_root, "testdata"))) {
    repo_root <- normalizePath(".", winslash = "/", mustWork = FALSE)
  }
  testdata_root <- file.path(repo_root, "testdata")
  skip_if_not(dir.exists(testdata_root))

  example_env <- new.env(parent = globalenv())
  source(file.path(repo_root, "inst/examples/sdam_firstlevel_task_pls.R"), local = example_env)

  manifest <- example_env$build_sdam_manifest(testdata_root)
  expect_equal(nrow(manifest), 128L)
  expect_equal(levels(manifest$group), c("control", "sdam"))
  expect_equal(
    levels(manifest$condition),
    c("recog_low_mid", "recog_high_sem", "nback_low_mid", "nback_high_sem")
  )
  expect_true(all(file.exists(manifest$file)))

  condition_key <- example_env$sdam_condition_key()
  expect_equal(condition_key$condition, levels(manifest$condition))
  expect_equal(names(condition_key), c("condition", "task", "level"))

  design <- example_env$summarise_sdam_design(manifest)
  expect_equal(design$n_subjects, 32L)
  expect_equal(design$n_maps, 128L)
  expect_equal(design$group_counts$subjects, c(14L, 18L))

  mask <- example_env$derive_common_nonzero_mask(manifest, max_voxels = 32L)
  expect_equal(sum(as.array(mask) > 0), 32L)

  spec <- example_env$make_sdam_task_pls_spec(
    manifest = manifest,
    mask = mask,
    nperm = 2L,
    nboot = 3L
  )

  set.seed(example_env$SDAM_SEED)
  result <- run(spec, progress = FALSE)
  expect_s3_class(result, "pls_result")
  expect_equal(n_features(result), 32L)
  expect_equal(result$groups, c("control", "sdam"))
  expect_equal(result$conditions, levels(manifest$condition))

  design_scores <- as_design_scores(result, lv = 1, condition_key = condition_key)
  expect_equal(nrow(design_scores), 8L)
  expect_true(all(c("task", "level", "mean", "se") %in% names(design_scores)))

  design_contrasts <- as_design_contrasts(result, lv = 1, condition_key = condition_key)
  expect_equal(nrow(design_contrasts), 7L)
  expect_true(all(is.finite(design_contrasts$magnitude)))

  lv_summary <- example_env$summarise_sdam_result(result)
  expect_equal(nrow(lv_summary), length(result$s))
  expect_true(all(is.finite(lv_summary$variance_percent)))
  expect_equal(length(bsr(result, lv = 1)), 32L)

  bsr_vol <- example_env$sdam_bsr_neurovol(result, lv = 1, threshold = 3)
  expect_s4_class(bsr_vol, "NeuroVol")
  expect_equal(dim(bsr_vol), dim(mask))

  overlay_lim <- example_env$sdam_bsr_overlay_limits(result, lv = 1, threshold = 3)
  expect_equal(length(overlay_lim), 2L)
  expect_true(all(is.finite(overlay_lim)))
  expect_lt(overlay_lim[[1]], 0)
  expect_gt(overlay_lim[[2]], 0)
})
