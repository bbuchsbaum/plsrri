# Optional MATLAB fixture tests (skipped if fixtures are absent).
#
# These fixtures are intended to be generated from the vendored MATLAB code
# under `PLS/plscmd/` and checked into `tests/testthat/fixtures/matlab/`.
#
# The key idea: MATLAB's split-half code draws random permutations internally
# (rng-shuffle + randperm). To compare across languages without matching RNG
# implementations, fixtures should include the *explicit* outer permutation
# matrix (`outer_reorder`) and the *explicit* inner subject permutations
# (`inner_subject_perms`) used by MATLAB. R can then be run deterministically
# from those fixed resampling inputs.

test_that("MATLAB split-half fixtures reproduce reference results", {
  fixture_path <- testthat::test_path("fixtures", "matlab", "splithalf_result.rds")

  if (!file.exists(fixture_path)) {
    testthat::skip("MATLAB fixtures not present: tests/testthat/fixtures/matlab/splithalf_result.rds")
  }

  fixtures <- readRDS(fixture_path)
  testthat::expect_true(is.list(fixtures), info = "Fixture file must contain a list")

  for (fx in fixtures) {
    testthat::expect_true(is.list(fx))
    testthat::expect_true(is.list(fx$args))
    testthat::expect_true(is.list(fx$expected))

    args <- fx$args
    args$progress <- FALSE

    res <- do.call(pls_splithalf_test, args)

    for (nm in names(fx$expected)) {
      got <- res[[nm]]
      exp <- fx$expected[[nm]]

      if (is.numeric(got) && is.numeric(exp)) {
        # Cross-language numeric comparisons should allow tiny BLAS/LAPACK drift.
        testthat::expect_equal(got, exp, tolerance = 1e-10, info = paste0("Mismatch for field: ", nm))
      } else {
        testthat::expect_equal(got, exp, info = paste0("Mismatch for field: ", nm))
      }
    }
  }
})
