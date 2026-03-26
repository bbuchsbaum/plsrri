# Optional MATLAB/Octave fixture tests for finite-data parameter sweeps.

test_that("MATLAB/Octave parameter fixtures reproduce reference results", {
  fixture_path <- testthat::test_path("fixtures", "matlab", "parameter_results.rds")

  if (!file.exists(fixture_path)) {
    testthat::skip("MATLAB parameter fixtures not present: tests/testthat/fixtures/matlab/parameter_results.rds")
  }

  fixtures <- readRDS(fixture_path)
  testthat::expect_true(is.list(fixtures))

  modes <- c(0L, 2L, 4L, 6L)

  xcor_fx <- fixtures$xcor
  for (i in seq_along(modes)) {
    mode <- modes[[i]]
    testthat::expect_equal(
      pls_xcor(xcor_fx$inputs$design, xcor_fx$inputs$datamat, cormode = mode),
      xcor_fx$expected[[i]],
      tolerance = 1e-10
    )
  }

  corr_fx <- fixtures$corr_maps
  for (i in seq_along(modes)) {
    mode <- modes[[i]]
    testthat::expect_equal(
      pls_corr_maps(corr_fx$inputs$behav, corr_fx$inputs$datamat, corr_fx$inputs$n_subj, corr_fx$inputs$num_cond, cormode = mode),
      corr_fx$expected_all[[i]],
      tolerance = 1e-10
    )
    testthat::expect_equal(
      pls_corr_maps_notall(corr_fx$inputs$behav, corr_fx$inputs$datamat, corr_fx$inputs$n_subj, bscan = corr_fx$inputs$bscan, cormode = mode),
      corr_fx$expected_sel[[i]],
      tolerance = 1e-10
    )
  }

  covcor3_fx <- fixtures$covcor_method3
  for (i in seq_along(modes)) {
    mode <- modes[[i]]
    res <- pls_get_covcor(
      method = covcor3_fx$inputs$method,
      stacked_datamat = covcor3_fx$inputs$stacked_datamat,
      stacked_behavdata = covcor3_fx$inputs$stacked_behavdata,
      num_groups = covcor3_fx$inputs$num_groups,
      num_subj_lst = covcor3_fx$inputs$num_subj_lst,
      num_cond = covcor3_fx$inputs$num_cond,
      bscan = covcor3_fx$inputs$bscan,
      meancentering_type = covcor3_fx$inputs$meancentering_type,
      cormode = mode
    )
    testthat::expect_equal(res$datamatsvd, covcor3_fx$expected[[i]]$datamatsvd, tolerance = 1e-10)
    testthat::expect_equal(res$datamatcorrs_lst[[1]], covcor3_fx$expected[[i]]$datamatcorrs_lst[[1]], tolerance = 1e-10)
    testthat::expect_equal(res$datamatcorrs_lst[[2]], covcor3_fx$expected[[i]]$datamatcorrs_lst[[2]], tolerance = 1e-10)
  }

  covcor1_fx <- fixtures$covcor_method1_meancentering
  for (mct in 0:3) {
    res <- pls_get_covcor(
      method = covcor1_fx$inputs$method,
      stacked_datamat = covcor1_fx$inputs$stacked_datamat,
      num_groups = covcor1_fx$inputs$num_groups,
      num_subj_lst = covcor1_fx$inputs$num_subj_lst,
      num_cond = covcor1_fx$inputs$num_cond,
      meancentering_type = as.integer(mct),
      cormode = covcor1_fx$inputs$cormode,
      compute_smeanmat = TRUE
    )
    exp <- covcor1_fx$expected[[mct + 1L]]
    testthat::expect_equal(res$datamatsvd, exp$datamatsvd, tolerance = 1e-10)
    testthat::expect_equal(res$stacked_smeanmat, exp$stacked_smeanmat, tolerance = 1e-10)
  }
})
