# Optional MATLAB/Octave fixture tests for missing-data helper paths.
#
# These fixtures are generated from the vendored MATLAB code under PLS/plscmd
# and focus on NaN-driven missnk behavior, not the known Inf divergence.

test_that("MATLAB/Octave missing-data fixtures reproduce reference results", {
  fixture_path <- testthat::test_path("fixtures", "matlab", "missing_results.rds")

  if (!file.exists(fixture_path)) {
    testthat::skip("MATLAB missing-data fixtures not present: tests/testthat/fixtures/matlab/missing_results.rds")
  }

  fixtures <- readRDS(fixture_path)
  testthat::expect_true(is.list(fixtures))

  xcor_fx <- fixtures$xcor
  testthat::expect_equal(
    pls_xcor(xcor_fx$inputs$design, xcor_fx$inputs$datamat, cormode = 0L),
    xcor_fx$expected$mode0,
    tolerance = 1e-10
  )
  testthat::expect_equal(
    pls_xcor(xcor_fx$inputs$design, xcor_fx$inputs$datamat, cormode = 2L),
    xcor_fx$expected$mode2,
    tolerance = 1e-10
  )
  testthat::expect_equal(
    pls_xcor(xcor_fx$inputs$design, xcor_fx$inputs$datamat, cormode = 4L),
    xcor_fx$expected$mode4,
    tolerance = 1e-10
  )
  testthat::expect_equal(
    pls_xcor(xcor_fx$inputs$design, xcor_fx$inputs$datamat, cormode = 6L),
    xcor_fx$expected$mode6,
    tolerance = 1e-10
  )

  corr_fx <- fixtures$corr_maps
  testthat::expect_equal(
    pls_corr_maps(corr_fx$inputs$behav, corr_fx$inputs$datamat, corr_fx$inputs$n_subj, corr_fx$inputs$num_cond, cormode = 0L),
    corr_fx$expected$all,
    tolerance = 1e-10
  )
  testthat::expect_equal(
    pls_corr_maps_notall(corr_fx$inputs$behav, corr_fx$inputs$datamat, corr_fx$inputs$n_subj, bscan = corr_fx$inputs$bscan, cormode = 0L),
    corr_fx$expected$sel,
    tolerance = 1e-10
  )

  covcor_fx <- fixtures$covcor_method3
  covcor_res <- pls_get_covcor(
    method = covcor_fx$inputs$method,
    stacked_datamat = covcor_fx$inputs$stacked_datamat,
    stacked_behavdata = covcor_fx$inputs$stacked_behavdata,
    num_groups = covcor_fx$inputs$num_groups,
    num_subj_lst = covcor_fx$inputs$num_subj_lst,
    num_cond = covcor_fx$inputs$num_cond,
    bscan = covcor_fx$inputs$bscan,
    meancentering_type = covcor_fx$inputs$meancentering_type,
    cormode = covcor_fx$inputs$cormode
  )
  testthat::expect_equal(covcor_res$datamatsvd, covcor_fx$expected$datamatsvd, tolerance = 1e-10)
})
