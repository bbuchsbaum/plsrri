# Tests for held-out score projection abstractions

.make_projection_spec <- function(datamat,
                                  num_subj,
                                  num_cond,
                                  method,
                                  behav = NULL,
                                  design = NULL) {
  spec <- pls_spec()
  spec$datamat_lst <- list(datamat)
  spec$num_subj_lst <- as.integer(num_subj)
  spec$num_cond <- as.integer(num_cond)
  spec$method <- as.integer(method)

  if (!is.null(behav)) {
    spec$stacked_behavdata <- behav
  }
  if (!is.null(design)) {
    spec$stacked_designdata <- design
  }

  spec
}

test_that("task PLS projection reproduces fitted scores through the MvaMethod hook", {
  set.seed(101)

  datamat <- matrix(rnorm(24 * 15), 24, 15)
  spec <- .make_projection_spec(
    datamat = datamat,
    num_subj = 12,
    num_cond = 2,
    method = 1L
  )

  method <- get_method("pls_task")
  mva_res <- method$fit(spec, progress = FALSE)
  proj <- method$project_scores(mva_res, spec, type = "both")

  expect_equal(proj$feature, scores(mva_res, type = "feature"), tolerance = 1e-10)
  expect_equal(proj$design, scores(mva_res, type = "design"), tolerance = 1e-10)
})

test_that("non-rotated task projection uses fitted contrasts and does not require held-out design rows", {
  set.seed(102)

  datamat <- matrix(rnorm(36 * 12), 36, 12)
  fit_spec <- .make_projection_spec(
    datamat = datamat,
    num_subj = 12,
    num_cond = 3,
    method = 2L,
    design = matrix(c(1, -1, 0), ncol = 1)
  )

  mva_res <- get_method("pls_task_nonrotated")$fit(fit_spec, progress = FALSE)

  holdout_spec <- .make_projection_spec(
    datamat = datamat,
    num_subj = 12,
    num_cond = 3,
    method = 2L
  )

  proj <- project_scores(mva_res, holdout_spec, type = "both")

  expect_equal(proj$feature, scores(mva_res, type = "feature"), tolerance = 1e-10)
  expect_equal(proj$design, scores(mva_res, type = "design"), tolerance = 1e-10)
})

test_that("behavior PLS projection reproduces fitted scores and only requires behavior data for design scores", {
  set.seed(103)

  datamat <- matrix(rnorm(20 * 14), 20, 14)
  behav <- matrix(rnorm(20 * 2), 20, 2)
  spec <- .make_projection_spec(
    datamat = datamat,
    num_subj = 10,
    num_cond = 2,
    method = 3L,
    behav = behav
  )

  pls_res <- run(spec, progress = FALSE)
  proj <- project_scores(pls_res, spec, type = "both")

  expect_equal(proj$feature, scores(pls_res, type = "brain"), tolerance = 1e-10)
  expect_equal(proj$design, scores(pls_res, type = "design"), tolerance = 1e-10)

  feature_only_spec <- spec
  feature_only_spec$stacked_behavdata <- NULL

  expect_equal(
    project_scores(pls_res, feature_only_spec, type = "feature"),
    scores(pls_res, type = "brain"),
    tolerance = 1e-10
  )

  expect_error(
    project_scores(pls_res, feature_only_spec, type = "design"),
    "requires behavior data"
  )
})

test_that("multiblock projection fails with an explicit unsupported-method error", {
  set.seed(104)

  datamat <- matrix(rnorm(18 * 10), 18, 10)
  behav <- matrix(rnorm(18 * 2), 18, 2)
  spec <- .make_projection_spec(
    datamat = datamat,
    num_subj = 9,
    num_cond = 2,
    method = 4L,
    behav = behav
  )

  pls_res <- run(spec, progress = FALSE)

  expect_error(
    project_scores(pls_res, spec, type = "feature"),
    "not yet implemented for multiblock PLS methods"
  )
})
