.attach_fixture_behavior <- function(spec, args) {
  if (is.null(args$stacked_behavdata)) {
    return(spec)
  }

  # Keep this helper independent of the installed package version when we
  # exercise fixture parity via `test_file()`. SSB fixtures use list-form
  # subject counts, and older installed exports may still assume balanced
  # scalar counts in `add_behavior()`.
  if (is.list(args$num_subj_lst)) {
    spec$stacked_behavdata <- args$stacked_behavdata
    if (!is.null(args$bscan)) {
      spec$bscan <- args$bscan
    }
    return(spec)
  }

  add_behavior(
    spec,
    args$stacked_behavdata,
    block_conditions = args$bscan
  )
}

build_fixture_spec <- function(args) {
  spec <- pls_spec() |>
    add_subjects(args$datamat_lst, groups = args$num_subj_lst) |>
    add_conditions(args$num_cond)

  spec <- .attach_fixture_behavior(spec, args)

  if (!is.null(args$stacked_designdata)) {
    if (ncol(args$stacked_designdata) == 1L) {
      spec$stacked_designdata <- args$stacked_designdata
    } else {
      spec <- add_design(spec, args$stacked_designdata)
    }
  }

  configure(
    spec,
    method = args$method,
    nperm = if (!is.null(args$num_perm)) args$num_perm else 0L,
    nboot = if (!is.null(args$num_boot)) args$num_boot else 0L,
    meancentering = args$meancentering_type,
    cormode = args$cormode,
    boot_type = args$boot_type,
    is_struct = args$is_struct
  )
}

test_that("builder path reproduces MATLAB/Octave-derived analysis fixtures", {
  fixture_path <- testthat::test_path("fixtures", "matlab", "analysis_results.rds")

  if (!file.exists(fixture_path)) {
    testthat::skip("MATLAB analysis fixtures not present: tests/testthat/fixtures/matlab/analysis_results.rds")
  }

  fixtures <- readRDS(fixture_path)
  testthat::expect_true(is.list(fixtures))

  run_arg_names <- c("permsamp", "bootsamp", "Tpermsamp", "Bpermsamp", "bootsamp_4beh")

  for (nm in names(fixtures)) {
    fx <- fixtures[[nm]]
    spec <- build_fixture_spec(fx$args)
    run_args <- fx$args[intersect(names(fx$args), run_arg_names)]
    run_args$progress <- FALSE

    res <- do.call(run, c(list(spec), run_args))

    .parity_expect_result_matches_fixture(res, fx, info = nm)
    testthat::expect_equal(res$method, fx$args$method, info = nm)
    testthat::expect_equal(res$num_subj_lst, fx$args$num_subj_lst, info = nm)
    testthat::expect_equal(res$num_cond, fx$args$num_cond, info = nm)
    testthat::expect_equal(
      res$conditions,
      paste0("condition_", seq_len(fx$args$num_cond)),
      info = nm
    )
  }
})
