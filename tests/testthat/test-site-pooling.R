test_that("site labels expand deterministically from subjects to observations", {
  meta <- plsrri:::.plsrri_observation_metadata(
    num_subj_lst = 3L,
    num_cond = 2L,
    groups = "all",
    conditions = c("c1", "c2")
  )

  expanded <- plsrri:::.plsrri_expand_site_labels(
    site = c("site_a", "site_b", "site_c"),
    obs_meta = meta
  )

  expect_equal(expanded, c("site_a", "site_b", "site_c", "site_a", "site_b", "site_c"))
})

test_that("observation-level site labels must be constant within subject", {
  meta <- plsrri:::.plsrri_observation_metadata(
    num_subj_lst = 2L,
    num_cond = 2L,
    groups = "all",
    conditions = c("c1", "c2")
  )

  expect_error(
    plsrri:::.plsrri_expand_site_labels(
      site = c("site_a", "site_b", "site_a", "site_a"),
      obs_meta = meta
    ),
    "constant within subject"
  )
})

test_that("site-specific subsetting preserves balanced counts and suppresses recursion", {
  spec <- pls_spec()
  spec$datamat_lst <- list(matrix(rnorm(12 * 5), nrow = 12, ncol = 5))
  spec$stacked_behavdata <- matrix(rnorm(12 * 2), nrow = 12, ncol = 2)
  spec$num_subj_lst <- 6L
  spec$num_cond <- 2L
  spec$method <- 3L

  meta <- plsrri:::.plsrri_observation_metadata(spec$num_subj_lst, spec$num_cond)
  site_obs <- plsrri:::.plsrri_expand_site_labels(c("a", "a", "b", "b", "c", "c"), meta)
  site_a <- plsrri:::.plsrri_subset_spec_by_site(spec, site_obs, keep_sites = "a")

  expect_equal(site_a$num_subj_lst, 2L)
  expect_equal(nrow(site_a$datamat_lst[[1]]), 4L)
  expect_true(isTRUE(site_a$.skip_site_diagnostics))
  expect_equal(unique(site_a$site), "a")
})

test_that("site_pooling_diagnostics works for direct behavior PLS", {
  set.seed(101)
  n_subj <- 9L
  n_cond <- 2L
  n_vox <- 20L
  n_behav <- 3L

  datamat <- matrix(rnorm(n_subj * n_cond * n_vox), nrow = n_subj * n_cond, ncol = n_vox)
  behav <- matrix(rnorm(n_subj * n_cond * n_behav), nrow = n_subj * n_cond, ncol = n_behav)
  site <- rep(c("site_a", "site_b", "site_c"), each = 3L)

  result <- behav_pls(
    datamat_lst = list(datamat),
    behav_data = behav,
    num_subj_lst = n_subj,
    num_cond = n_cond,
    nperm = 0,
    nboot = 0,
    site = site,
    progress = FALSE
  )

  expect_true(is.list(result$site_diagnostics))
  expect_setequal(result$site_diagnostics$sites, c("site_a", "site_b", "site_c"))
  expect_equal(
    unique(result$site_diagnostics$observation_scores$site),
    c("site_a", "site_b", "site_c")
  )
  expect_equal(
    nrow(result$site_diagnostics$site_score_correlations),
    length(unique(site)) * n_lv(result)
  )
  expect_equal(
    nrow(result$site_diagnostics$site_fit_similarity),
    length(unique(site)) * n_lv(result)
  )
  expect_equal(
    nrow(result$site_diagnostics$leave_one_site_out),
    length(unique(site)) * n_lv(result)
  )
})

test_that("site_pooling_diagnostics accepts observation-level site labels", {
  set.seed(102)
  n_subj <- 6L
  n_cond <- 2L
  n_vox <- 12L
  n_behav <- 2L

  datamat <- matrix(rnorm(n_subj * n_cond * n_vox), nrow = n_subj * n_cond, ncol = n_vox)
  behav <- matrix(rnorm(n_subj * n_cond * n_behav), nrow = n_subj * n_cond, ncol = n_behav)
  site_obs <- rep(c("site_a", "site_b", "site_c", "site_a", "site_b", "site_c"), times = n_cond)

  result <- behav_pls(
    datamat_lst = list(datamat),
    behav_data = behav,
    num_subj_lst = n_subj,
    num_cond = n_cond,
    nperm = 0,
    nboot = 0,
    progress = FALSE
  )

  spec_tmp <- pls_spec()
  spec_tmp$datamat_lst <- list(datamat)
  spec_tmp$stacked_behavdata <- behav
  spec_tmp$num_subj_lst <- n_subj
  spec_tmp$num_cond <- n_cond
  spec_tmp$method <- 3L

  diag <- site_pooling_diagnostics(result, site = site_obs, spec = spec_tmp)

  expect_equal(sort(unique(diag$observation_scores$site)), c("site_a", "site_b", "site_c"))
})

test_that("subject-level and observation-level site labels yield the same summaries", {
  set.seed(104)
  n_subj <- 6L
  n_cond <- 2L
  n_vox <- 10L
  n_behav <- 2L

  datamat <- matrix(rnorm(n_subj * n_cond * n_vox), nrow = n_subj * n_cond, ncol = n_vox)
  behav <- matrix(rnorm(n_subj * n_cond * n_behav), nrow = n_subj * n_cond, ncol = n_behav)
  site_subj <- c("site_a", "site_b", "site_c", "site_a", "site_b", "site_c")
  site_obs <- rep(site_subj, times = n_cond)

  result <- behav_pls(
    datamat_lst = list(datamat),
    behav_data = behav,
    num_subj_lst = n_subj,
    num_cond = n_cond,
    nperm = 0,
    nboot = 0,
    progress = FALSE
  )

  spec_tmp <- pls_spec()
  spec_tmp$datamat_lst <- list(datamat)
  spec_tmp$stacked_behavdata <- behav
  spec_tmp$num_subj_lst <- n_subj
  spec_tmp$num_cond <- n_cond
  spec_tmp$method <- 3L

  diag_subj <- site_pooling_diagnostics(result, site = site_subj, spec = spec_tmp)
  diag_obs <- site_pooling_diagnostics(result, site = site_obs, spec = spec_tmp)

  expect_equal(diag_subj$site_score_summary, diag_obs$site_score_summary, tolerance = 1e-12)
  expect_equal(diag_subj$site_score_correlations, diag_obs$site_score_correlations, tolerance = 1e-12)
})

test_that("builder behavior PLS attaches site diagnostics when site labels are provided", {
  set.seed(103)
  n_subj <- 9L
  n_cond <- 2L
  n_vox <- 10L
  n_behav <- 2L

  datamat <- matrix(rnorm(n_subj * n_cond * n_vox), nrow = n_subj * n_cond, ncol = n_vox)
  behav <- matrix(rnorm(n_subj * n_cond * n_behav), nrow = n_subj * n_cond, ncol = n_behav)
  site <- rep(c("site_a", "site_b", "site_c"), each = 3L)

  result <- pls_spec() |>
    add_subjects(list(datamat), groups = n_subj) |>
    add_conditions(n_cond) |>
    add_behavior(behav) |>
    add_site_labels(site) |>
    configure(method = "behavior", nperm = 0, nboot = 0) |>
    run(progress = FALSE)

  expect_true(is.list(result$site_diagnostics))
  expect_setequal(result$site_diagnostics$sites, c("site_a", "site_b", "site_c"))
})

test_that("site_pooling_diagnostics accepts mva_result inputs", {
  set.seed(105)
  n_subj <- 6L
  n_cond <- 2L
  n_vox <- 8L
  n_behav <- 2L

  datamat <- matrix(rnorm(n_subj * n_cond * n_vox), nrow = n_subj * n_cond, ncol = n_vox)
  behav <- matrix(rnorm(n_subj * n_cond * n_behav), nrow = n_subj * n_cond, ncol = n_behav)
  site <- c("site_a", "site_b", "site_c", "site_a", "site_b", "site_c")

  spec_tmp <- pls_spec()
  spec_tmp$datamat_lst <- list(datamat)
  spec_tmp$stacked_behavdata <- behav
  spec_tmp$num_subj_lst <- n_subj
  spec_tmp$num_cond <- n_cond
  spec_tmp$method <- 3L
  spec_tmp$site <- site

  result <- behav_pls(
    datamat_lst = list(datamat),
    behav_data = behav,
    num_subj_lst = n_subj,
    num_cond = n_cond,
    nperm = 0,
    nboot = 0,
    progress = FALSE
  )

  mva_res <- pls_result_to_mva_result(result, spec = spec_tmp)
  diag <- site_pooling_diagnostics(mva_res, progress = FALSE)

  expect_true(is.list(diag))
  expect_setequal(diag$sites, c("site_a", "site_b", "site_c"))
})
