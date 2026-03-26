# Optional MATLAB/Octave fixture tests for full pls_analysis() runs.
#
# These fixtures are generated from the vendored MATLAB code under PLS/plscmd
# with fixed permutation/bootstrap orders so R can be compared deterministically
# without matching MATLAB's RNG stream.

test_that("MATLAB/Octave pls_analysis fixtures reproduce reference results", {
  fixture_path <- testthat::test_path("fixtures", "matlab", "analysis_results.rds")

  if (!file.exists(fixture_path)) {
    testthat::skip("MATLAB analysis fixtures not present: tests/testthat/fixtures/matlab/analysis_results.rds")
  }

  fixtures <- readRDS(fixture_path)
  testthat::expect_true(is.list(fixtures))

  for (nm in names(fixtures)) {
    fx <- fixtures[[nm]]
    args <- fx$args
    args$progress <- FALSE

    res <- do.call(pls_analysis, args)
    .parity_expect_result_matches_fixture(res, fx, info = nm)
  }
})
