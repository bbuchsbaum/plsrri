# Tests for within-subject seed PLS (ws-fcMRI)

# --- Helper: generate simulated trial-level data ---
make_ws_data <- function(n_subj = 10, n_trials = 24, n_vox = 50,
                         n_cond = 2, n_seeds = 1, signal_strength = 0,
                         seed = 123) {
  set.seed(seed)
  beta_lst <- vector("list", n_subj)
  seed_lst <- vector("list", n_subj)
  cond_lst <- vector("list", n_subj)

  trials_per_cond <- n_trials %/% n_cond

  for (i in seq_len(n_subj)) {
    set.seed(seed + i)  # Different data per subject
    betas <- matrix(rnorm(n_trials * n_vox), nrow = n_trials, ncol = n_vox)
    seeds <- matrix(rnorm(n_trials * n_seeds), nrow = n_trials, ncol = n_seeds)
    conds <- rep(seq_len(n_cond), each = trials_per_cond)
    # Handle remainder trials
    if (length(conds) < n_trials) {
      conds <- c(conds, rep(n_cond, n_trials - length(conds)))
    }

    # Inject signal: seed correlated with first 10 voxels in condition 1
    if (signal_strength > 0) {
      c1_idx <- which(conds == 1)
      for (s in seq_len(n_seeds)) {
        betas[c1_idx, 1:10] <- betas[c1_idx, 1:10] +
          signal_strength * seeds[c1_idx, s]
      }
    }

    beta_lst[[i]] <- betas
    seed_lst[[i]] <- if (n_seeds == 1) seeds[, 1] else seeds
    cond_lst[[i]] <- conds
  }

  list(beta_lst = beta_lst, seed_lst = seed_lst, cond_lst = cond_lst,
       n_subj = n_subj, n_trials = n_trials, n_vox = n_vox,
       n_cond = n_cond, n_seeds = n_seeds)
}


# ---- ws_seed_correlation tests ----

test_that("ws_seed_correlation returns correct dimensions", {
  d <- make_ws_data(n_subj = 8, n_trials = 20, n_vox = 40, n_cond = 2)
  ws <- ws_seed_correlation(d$beta_lst, d$seed_lst, d$cond_lst)

  expect_equal(nrow(ws$datamat), d$n_subj * d$n_cond)
  expect_equal(ncol(ws$datamat), d$n_vox)
  expect_equal(ws$num_cond, d$n_cond)
  expect_equal(ws$n_subjects, d$n_subj)
  expect_equal(ws$n_seeds, 1L)
  expect_equal(ws$n_voxels, d$n_vox)
})

test_that("ws_seed_correlation handles multiple seeds", {
  d <- make_ws_data(n_subj = 6, n_trials = 20, n_vox = 30, n_cond = 2, n_seeds = 3)
  ws <- ws_seed_correlation(d$beta_lst, d$seed_lst, d$cond_lst)

  expect_equal(nrow(ws$datamat), d$n_subj * d$n_cond)
  expect_equal(ncol(ws$datamat), d$n_vox * d$n_seeds)
  expect_equal(ws$n_seeds, 3L)
})

test_that("ws_seed_correlation produces finite values", {
  d <- make_ws_data(n_subj = 6, n_trials = 20, n_vox = 30, n_cond = 2)
  ws <- ws_seed_correlation(d$beta_lst, d$seed_lst, d$cond_lst)

  expect_true(all(is.finite(ws$datamat)))
})

test_that("ws_seed_correlation with fisher_z=FALSE returns raw correlations in [-1, 1]", {
  d <- make_ws_data(n_subj = 6, n_trials = 20, n_vox = 30, n_cond = 2)
  ws <- ws_seed_correlation(d$beta_lst, d$seed_lst, d$cond_lst, fisher_z = FALSE)

  expect_true(all(ws$datamat >= -1 & ws$datamat <= 1))
})

test_that("ws_seed_correlation fisher_z changes values", {
  d <- make_ws_data(n_subj = 6, n_trials = 20, n_vox = 30, n_cond = 2)
  ws_raw <- ws_seed_correlation(d$beta_lst, d$seed_lst, d$cond_lst, fisher_z = FALSE)
  ws_z <- ws_seed_correlation(d$beta_lst, d$seed_lst, d$cond_lst, fisher_z = TRUE)

  # z-transformed values should differ from raw (unless r = 0 exactly)
  expect_false(identical(ws_raw$datamat, ws_z$datamat))
  # z-transform should produce larger absolute values for large |r|
  expect_true(all(abs(ws_z$datamat) >= abs(ws_raw$datamat) - 1e-10))
})

test_that("ws_seed_correlation handles 3+ conditions", {
  d <- make_ws_data(n_subj = 6, n_trials = 30, n_vox = 20, n_cond = 3)
  ws <- ws_seed_correlation(d$beta_lst, d$seed_lst, d$cond_lst)

  expect_equal(nrow(ws$datamat), 6 * 3)
  expect_equal(ws$num_cond, 3)
  expect_equal(length(ws$cond_labels), 3)
})

test_that("ws_seed_correlation produces NA for too-few trials", {
  d <- make_ws_data(n_subj = 3, n_trials = 5, n_vox = 10, n_cond = 2)
  # With only 5 trials and 2 conditions, ~2-3 trials per condition
  ws <- ws_seed_correlation(d$beta_lst, d$seed_lst, d$cond_lst, min_trials = 4)

  # At least some rows should have NAs (not enough trials)
  expect_true(any(is.na(ws$datamat)))
})

test_that("ws_seed_correlation handles zero-variance seed gracefully", {
  d <- make_ws_data(n_subj = 4, n_trials = 20, n_vox = 15, n_cond = 2)
  # Set seed to constant for first subject, first condition
  conds <- d$cond_lst[[1]]
  c1_idx <- which(conds == 1)
  if (is.vector(d$seed_lst[[1]])) {
    d$seed_lst[[1]][c1_idx] <- 5.0
  } else {
    d$seed_lst[[1]][c1_idx, ] <- 5.0
  }

  ws <- ws_seed_correlation(d$beta_lst, d$seed_lst, d$cond_lst)

  # Zero-variance seed should produce r = 0, z = 0
  expect_true(all(ws$datamat[1, ] == 0))
})


# ---- Input validation tests ----

test_that("ws_seed_correlation errors on mismatched list lengths", {
  d <- make_ws_data(n_subj = 4, n_trials = 20, n_vox = 10, n_cond = 2)
  expect_error(
    ws_seed_correlation(d$beta_lst, d$seed_lst[1:3], d$cond_lst),
    "same length"
  )
  expect_error(
    ws_seed_correlation(d$beta_lst, d$seed_lst, d$cond_lst[1:3]),
    "same length"
  )
})

test_that("ws_seed_correlation errors on voxel count mismatch", {
  d <- make_ws_data(n_subj = 4, n_trials = 20, n_vox = 10, n_cond = 2)
  d$beta_lst[[2]] <- d$beta_lst[[2]][, 1:8]
  expect_error(
    ws_seed_correlation(d$beta_lst, d$seed_lst, d$cond_lst),
    "same voxel count"
  )
})

test_that("ws_seed_correlation errors on trial count mismatch between beta and seed", {
  d <- make_ws_data(n_subj = 4, n_trials = 20, n_vox = 10, n_cond = 2)
  d$seed_lst[[1]] <- d$seed_lst[[1]][1:15]
  expect_error(
    ws_seed_correlation(d$beta_lst, d$seed_lst, d$cond_lst),
    "must match trial count"
  )
})

test_that("ws_seed_correlation errors on condition length mismatch", {
  d <- make_ws_data(n_subj = 4, n_trials = 20, n_vox = 10, n_cond = 2)
  d$cond_lst[[1]] <- d$cond_lst[[1]][1:15]
  expect_error(
    ws_seed_correlation(d$beta_lst, d$seed_lst, d$cond_lst),
    "has length"
  )
})

test_that("ws_seed_correlation errors on min_trials < 2", {
  d <- make_ws_data(n_subj = 4, n_trials = 20, n_vox = 10, n_cond = 2)
  expect_error(
    ws_seed_correlation(d$beta_lst, d$seed_lst, d$cond_lst, min_trials = 1),
    "min_trials"
  )
})

test_that("ws_seed_correlation errors when subjects do not share condition sets", {
  d <- make_ws_data(n_subj = 4, n_trials = 20, n_vox = 10, n_cond = 2)
  d$cond_lst[[2]][d$cond_lst[[2]] == 2] <- 3

  expect_error(
    ws_seed_correlation(d$beta_lst, d$seed_lst, d$cond_lst),
    "same set of condition labels"
  )
})


# ---- ws_seed_pls integration tests ----

test_that("ws_seed_pls runs end-to-end with null data", {
  d <- make_ws_data(n_subj = 8, n_trials = 20, n_vox = 30, n_cond = 2)
  result <- ws_seed_pls(
    beta_lst = d$beta_lst,
    seed_lst = d$seed_lst,
    condition_lst = d$cond_lst,
    num_subj_lst = d$n_subj,
    nperm = 0, nboot = 0,
    progress = FALSE
  )

  expect_s3_class(result, "pls_result")
  expect_s3_class(result, "pls_task")
  expect_true(all(is.finite(result$s)))
  expect_equal(length(result$s), d$n_cond)
})

test_that("ws_seed_pls recovers injected signal", {
  # Verify the correlation step detects the injected condition-dependent

  # connectivity, and that PLS produces a meaningful decomposition.
  d <- make_ws_data(n_subj = 16, n_trials = 40, n_vox = 50,
                    n_cond = 2, signal_strength = 3.0)

  # Stage 1 check: within-subject correlations should show a large
  # condition difference for signal voxels (1-10) vs noise (11-50).
  ws <- ws_seed_correlation(d$beta_lst, d$seed_lst, d$cond_lst, fisher_z = FALSE)
  n_rows <- d$n_subj * d$n_cond
  c1_rows <- seq(1, n_rows, by = 2)
  c2_rows <- seq(2, n_rows, by = 2)
  cond_diff <- colMeans(ws$datamat[c1_rows, ]) - colMeans(ws$datamat[c2_rows, ])

  expect_gt(mean(abs(cond_diff[1:10])), mean(abs(cond_diff[11:50])) * 5)

  # Stage 2 check: PLS produces a non-trivial singular value.
  result <- ws_seed_pls(
    beta_lst = d$beta_lst,
    seed_lst = d$seed_lst,
    condition_lst = d$cond_lst,
    num_subj_lst = d$n_subj,
    fisher_z = FALSE,
    nperm = 0, nboot = 0,
    progress = FALSE
  )
  expect_s3_class(result, "pls_task")
  expect_gt(result$s[1], 0.1)
})

test_that("ws_seed_pls with nonrotated=TRUE returns non-rotated result", {
  d <- make_ws_data(n_subj = 8, n_trials = 20, n_vox = 30, n_cond = 2)
  result <- ws_seed_pls(
    beta_lst = d$beta_lst,
    seed_lst = d$seed_lst,
    condition_lst = d$cond_lst,
    num_subj_lst = d$n_subj,
    nonrotated = TRUE,
    nperm = 0, nboot = 0,
    progress = FALSE
  )

  expect_s3_class(result, "pls_task_nonrot")
})

test_that("ws_seed_pls supports multiple groups", {
  d <- make_ws_data(n_subj = 12, n_trials = 20, n_vox = 30, n_cond = 2)
  result <- ws_seed_pls(
    beta_lst = d$beta_lst,
    seed_lst = d$seed_lst,
    condition_lst = d$cond_lst,
    num_subj_lst = c(6L, 6L),
    nperm = 0, nboot = 0,
    progress = FALSE
  )

  expect_s3_class(result, "pls_task")
  expect_true(all(is.finite(result$s)))
})

test_that("ws_seed_pls with permutations and bootstraps", {
  d <- make_ws_data(n_subj = 8, n_trials = 20, n_vox = 30,
                    n_cond = 2, signal_strength = 1.5)
  result <- ws_seed_pls(
    beta_lst = d$beta_lst,
    seed_lst = d$seed_lst,
    condition_lst = d$cond_lst,
    num_subj_lst = d$n_subj,
    nperm = 20, nboot = 20,
    progress = FALSE
  )

  expect_s3_class(result, "pls_task")
  pvals <- significance(result)
  expect_true(all(is.finite(pvals)))
  expect_true(all(pvals >= 0 & pvals <= 1))

  bsr_vals <- bsr(result, lv = 1)
  expect_true(all(is.finite(bsr_vals)))
})

test_that("ws_seed_pls errors on num_subj_lst mismatch", {
  d <- make_ws_data(n_subj = 8, n_trials = 20, n_vox = 30, n_cond = 2)
  expect_error(
    ws_seed_pls(d$beta_lst, d$seed_lst, d$cond_lst,
                num_subj_lst = 10, nperm = 0, nboot = 0),
    "trial data contains 8 subjects"
  )
})

test_that("ws_seed_pls with multiple seeds", {
  d <- make_ws_data(n_subj = 8, n_trials = 20, n_vox = 30,
                    n_cond = 2, n_seeds = 2)
  result <- ws_seed_pls(
    beta_lst = d$beta_lst,
    seed_lst = d$seed_lst,
    condition_lst = d$cond_lst,
    num_subj_lst = d$n_subj,
    nperm = 0, nboot = 0,
    progress = FALSE
  )

  expect_s3_class(result, "pls_task")
  # With 2 seeds, columns should be n_vox * n_seeds = 60
  expect_equal(ncol(result$u), d$n_cond)
})


# ---- Builder API tests ----

test_that("builder API works with method='ws_seed'", {
  d <- make_ws_data(n_subj = 8, n_trials = 20, n_vox = 30, n_cond = 2)
  result <- pls_spec() |>
    add_trial_data(d$beta_lst, d$seed_lst, d$cond_lst) |>
    configure(method = "ws_seed", nperm = 0, nboot = 0) |>
    run(progress = FALSE)

  expect_s3_class(result, "pls_task")
  expect_true(all(is.finite(result$s)))
})

test_that("builder API warns when ws_seed has no trial data", {
  expect_message(
    pls_spec() |>
      configure(method = "ws_seed"),
    "trial-level data"
  )
})

test_that("builder API supports non-rotated ws-seed through task_nonrotated alias", {
  d <- make_ws_data(n_subj = 8, n_trials = 20, n_vox = 30, n_cond = 2)
  result <- pls_spec() |>
    add_trial_data(d$beta_lst, d$seed_lst, d$cond_lst) |>
    configure(method = "ws_seed_nonrotated", nperm = 0, nboot = 0) |>
    run(progress = FALSE)

  expect_s3_class(result, "pls_task_nonrot")
  expect_false(is.null(result$stacked_designdata))
})

test_that("builder API propagates bootstrap, split-half, and ws-seed metadata", {
  d <- make_ws_data(
    n_subj = 10, n_trials = 24, n_vox = 20, n_cond = 2,
    signal_strength = 1.5
  )

  result <- suppressWarnings(
    pls_spec() |>
      add_trial_data(d$beta_lst, d$seed_lst, d$cond_lst) |>
      add_conditions(c("cond_a", "cond_b")) |>
      configure(
        method = "ws_seed",
        nperm = 6,
        nboot = 6,
        nsplit = 2,
        clim = 90,
        boot_type = "strat",
        is_struct = TRUE
      ) |>
      run(progress = FALSE)
  )

  expect_equal(result$conditions, c("cond_a", "cond_b"))
  expect_equal(result$boot_result$clim, 90)
  expect_true(result$is_struct)
  expect_false(is.null(result$splithalf_result))
  expect_equal(result$ws_seed_info$n_seeds, 1L)
  expect_equal(result$ws_seed_info$n_voxels, d$n_vox)
  expect_equal(result$feature_layout$source, "ws_seed")
})

test_that("add_trial_data rejects conflicting builder structure", {
  d <- make_ws_data(n_subj = 8, n_trials = 20, n_vox = 30, n_cond = 2)

  expect_error(
    pls_spec() |>
      add_conditions(3) |>
      add_trial_data(d$beta_lst, d$seed_lst, d$cond_lst),
    "same length as the condition set implied"
  )

  expect_error(
    pls_spec() |>
      add_subjects(matrix(rnorm(20), nrow = 4), groups = 4) |>
      add_trial_data(d$beta_lst, d$seed_lst, d$cond_lst),
    "cannot be combined"
  )
})

test_that("process_trial_data derives SSB counts from sparse trial conditions", {
  d <- make_ws_data(n_subj = 5, n_trials = 20, n_vox = 12, n_cond = 2)
  groups <- c("A", "A", "A", "B", "B")

  d$cond_lst[[2]][] <- 1L
  d$cond_lst[[5]][] <- 1L

  spec <- pls_spec() |>
    add_trial_data(d$beta_lst, d$seed_lst, d$cond_lst, groups = groups)

  proc <- plsrri:::.process_trial_data(spec)

  expect_true(is.list(proc$num_subj_lst))
  expect_equal(proc$num_subj_lst, list(A = c(3L, 2L), B = c(2L, 1L)))
  expect_equal(proc$groups, c("A", "B"))
  expect_equal(nrow(proc$datamat_lst[[1]]), 5L)
  expect_equal(nrow(proc$datamat_lst[[2]]), 3L)
})

test_that("ws_seed_pls supports SSB when subject groups are supplied", {
  d <- make_ws_data(n_subj = 5, n_trials = 20, n_vox = 20, n_cond = 2, signal_strength = 1.5)
  groups <- c("A", "A", "A", "B", "B")

  d$cond_lst[[2]][] <- 1L
  d$cond_lst[[5]][] <- 1L

  result <- ws_seed_pls(
    beta_lst = d$beta_lst,
    seed_lst = d$seed_lst,
    condition_lst = d$cond_lst,
    num_subj_lst = list(A = c(3L, 2L), B = c(2L, 1L)),
    groups = groups,
    nperm = 0,
    nboot = 0,
    progress = FALSE
  )

  expect_s3_class(result, "pls_task")
  expect_true(is.list(result$num_subj_lst))
  expect_equal(result$num_subj_lst, list(A = c(3L, 2L), B = c(2L, 1L)))
  expect_equal(result$groups, c("A", "B"))
})

test_that("ws_seed_pls requires subject groups for SSB num_subj_lst", {
  d <- make_ws_data(n_subj = 5, n_trials = 20, n_vox = 20, n_cond = 2)
  d$cond_lst[[2]][] <- 1L
  d$cond_lst[[5]][] <- 1L

  expect_error(
    ws_seed_pls(
      beta_lst = d$beta_lst,
      seed_lst = d$seed_lst,
      condition_lst = d$cond_lst,
      num_subj_lst = list(c(3L, 2L), c(2L, 1L)),
      nperm = 0,
      nboot = 0,
      progress = FALSE
    ),
    "requires subject-level group assignments"
  )
})

test_that("ws_seed_pls rejects SSB counts that do not match observed trial data", {
  d <- make_ws_data(n_subj = 5, n_trials = 20, n_vox = 20, n_cond = 2)
  groups <- c("A", "A", "A", "B", "B")
  d$cond_lst[[2]][] <- 1L
  d$cond_lst[[5]][] <- 1L

  expect_error(
    ws_seed_pls(
      beta_lst = d$beta_lst,
      seed_lst = d$seed_lst,
      condition_lst = d$cond_lst,
      num_subj_lst = list(A = c(3L, 3L), B = c(2L, 1L)),
      groups = groups,
      nperm = 0,
      nboot = 0,
      progress = FALSE
    ),
    "does not match the SSB counts implied by trial data"
  )
})


# ---- Internal helper tests ----

test_that(".ws_cor_vec matches cor()", {
  set.seed(99)
  x <- rnorm(50)
  Y <- matrix(rnorm(50 * 10), 50, 10)

  r_ours <- plsrri:::.ws_cor_vec(x, Y)
  r_base <- as.numeric(cor(x, Y))

  expect_equal(r_ours, r_base, tolerance = 1e-12)
})

test_that(".fisher_z is correct", {
  r <- c(-0.9, -0.5, 0, 0.5, 0.9)
  z <- plsrri:::.fisher_z(r)
  z_expected <- atanh(r)

  expect_equal(z, z_expected, tolerance = 1e-6)
})

test_that(".fisher_z handles boundary values", {
  z <- plsrri:::.fisher_z(c(-1, 1))
  expect_true(all(is.finite(z)))
})
