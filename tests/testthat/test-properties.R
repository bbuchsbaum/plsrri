# Property-Based Tests for PLS
#
# These tests generate random inputs with varying parameters and verify
# that mathematical invariants always hold. This is inspired by QuickCheck-style
# property-based testing.
#
# Key invariants tested:
# - SVD properties (orthonormality, non-negativity, sorting)
# - Dimension consistency
# - Statistical bounds (p-values in [0,1], variance sums to 100%)
# - Reproducibility with fixed seeds
# - Accessor consistency

# -----------------------------------------------------------------------------
# Test Generators
# -----------------------------------------------------------------------------

#' Generate random PLS input data
#'
#' @param n_groups Number of groups (1-5)
#' @param n_subj_range Range for subjects per group
#' @param n_cond_range Range for number of conditions
#' @param n_features_range Range for number of features
generate_random_pls_data <- function(n_groups = NULL,
                                      n_subj_range = c(5, 20),
                                      n_cond_range = c(2, 5),
                                      n_features_range = c(20, 100)) {

  if (is.null(n_groups)) {
    n_groups <- sample(1:3, 1)
  }

  n_cond <- sample(n_cond_range[1]:n_cond_range[2], 1)
  n_features <- sample(n_features_range[1]:n_features_range[2], 1)

  # Generate subjects per group

  num_subj_lst <- sample(n_subj_range[1]:n_subj_range[2], n_groups, replace = TRUE)

  # Generate data matrices
  datamat_lst <- lapply(num_subj_lst, function(n_subj) {
    n_rows <- n_subj * n_cond
    matrix(rnorm(n_rows * n_features), nrow = n_rows, ncol = n_features)
  })

  list(
    datamat_lst = datamat_lst,
    num_subj_lst = num_subj_lst,
    n_cond = n_cond,
    n_features = n_features,
    n_groups = n_groups
  )
}

#' Generate random behavior data matching PLS input
generate_behavior_data <- function(pls_data, n_behav_range = c(1, 5)) {
  n_behav <- sample(n_behav_range[1]:n_behav_range[2], 1)
  total_rows <- sum(pls_data$num_subj_lst) * pls_data$n_cond

  matrix(rnorm(total_rows * n_behav), nrow = total_rows, ncol = n_behav)
}

#' Generate random design contrasts for non-rotated methods
generate_design_contrasts <- function(pls_data, method) {
  n_groups <- pls_data$n_groups
  n_cond <- pls_data$n_cond

  n_contrasts <- sample(1:min(3, n_cond - 1), 1)

  if (method == 2) {
    # Task design: n_groups * n_cond rows
    n_rows <- n_groups * n_cond
  } else if (method == 5) {
    # Behavior design: n_groups * n_cond * n_behav rows
    # Assume 2 behavior measures for simplicity
    n_rows <- n_groups * n_cond * 2
  } else if (method == 6) {
    # Multiblock design
    n_rows <- n_groups * n_cond + n_groups * n_cond * 2
  } else {
    stop("Invalid method for design contrasts")
  }

  matrix(rnorm(n_rows * n_contrasts), nrow = n_rows, ncol = n_contrasts)
}

# -----------------------------------------------------------------------------
# Property: SVD Singular Values are Non-Negative and Sorted
# -----------------------------------------------------------------------------

test_that("PROPERTY: singular values are always non-negative", {
  for (trial in 1:20) {
    set.seed(trial * 100)
    pls_data <- generate_random_pls_data()

    result <- pls_analysis(
      datamat_lst = pls_data$datamat_lst,
      num_subj_lst = pls_data$num_subj_lst,
      num_cond = pls_data$n_cond,
      method = 1,
      progress = FALSE
    )

    expect_true(
      all(result$s >= 0),
      label = sprintf("Trial %d: singular values non-negative", trial)
    )
  }
})

test_that("PROPERTY: singular values are sorted descending", {
  for (trial in 1:20) {
    set.seed(trial * 100)
    pls_data <- generate_random_pls_data()

    result <- pls_analysis(
      datamat_lst = pls_data$datamat_lst,
      num_subj_lst = pls_data$num_subj_lst,
      num_cond = pls_data$n_cond,
      method = 1,
      progress = FALSE
    )

    sorted_s <- sort(result$s, decreasing = TRUE)
    expect_equal(
      result$s, sorted_s,
      label = sprintf("Trial %d: singular values sorted", trial)
    )
  }
})

# -----------------------------------------------------------------------------
# Property: U and V Have Orthonormal Columns
# -----------------------------------------------------------------------------

test_that("PROPERTY: U matrix columns are orthonormal", {
  for (trial in 1:15) {
    set.seed(trial * 100)
    pls_data <- generate_random_pls_data()

    result <- pls_analysis(
      datamat_lst = pls_data$datamat_lst,
      num_subj_lst = pls_data$num_subj_lst,
      num_cond = pls_data$n_cond,
      method = 1,
      progress = FALSE
    )

    u <- result$u
    u_inner <- crossprod(u)

    # Diagonal should be 1 (unit norm)
    expect_equal(
      diag(u_inner), rep(1, ncol(u)),
      tolerance = 1e-10,
      label = sprintf("Trial %d: U columns unit norm", trial)
    )

    # Off-diagonal should be 0 (orthogonal)
    off_diag <- u_inner - diag(diag(u_inner))
    expect_true(
      all(abs(off_diag) < 1e-10),
      label = sprintf("Trial %d: U columns orthogonal", trial)
    )
  }
})

test_that("PROPERTY: V matrix columns are orthonormal", {
  for (trial in 1:15) {
    set.seed(trial * 100)
    pls_data <- generate_random_pls_data()

    result <- pls_analysis(
      datamat_lst = pls_data$datamat_lst,
      num_subj_lst = pls_data$num_subj_lst,
      num_cond = pls_data$n_cond,
      method = 1,
      progress = FALSE
    )

    v <- result$v
    v_inner <- crossprod(v)

    # Diagonal should be 1
    expect_equal(
      diag(v_inner), rep(1, ncol(v)),
      tolerance = 1e-10,
      label = sprintf("Trial %d: V columns unit norm", trial)
    )

    # Off-diagonal should be 0
    off_diag <- v_inner - diag(diag(v_inner))
    expect_true(
      all(abs(off_diag) < 1e-10),
      label = sprintf("Trial %d: V columns orthogonal", trial)
    )
  }
})

# -----------------------------------------------------------------------------
# Property: Variance Explained Sums to 100%
# -----------------------------------------------------------------------------

test_that("PROPERTY: variance explained always sums to 100%", {
  for (trial in 1:20) {
    set.seed(trial * 100)
    pls_data <- generate_random_pls_data()

    result <- pls_analysis(
      datamat_lst = pls_data$datamat_lst,
      num_subj_lst = pls_data$num_subj_lst,
      num_cond = pls_data$n_cond,
      method = 1,
      progress = FALSE
    )

    var_exp <- (result$s^2) / sum(result$s^2) * 100

    expect_equal(
      sum(var_exp), 100,
      tolerance = 1e-10,
      label = sprintf("Trial %d: variance sums to 100", trial)
    )
  }
})

# -----------------------------------------------------------------------------
# Property: Dimension Consistency
# -----------------------------------------------------------------------------

test_that("PROPERTY: output dimensions are consistent with input", {
  for (trial in 1:20) {
    set.seed(trial * 100)
    pls_data <- generate_random_pls_data()

    total_obs <- sum(pls_data$num_subj_lst) * pls_data$n_cond
    expected_n_lv <- pls_data$n_groups * pls_data$n_cond

    result <- pls_analysis(
      datamat_lst = pls_data$datamat_lst,
      num_subj_lst = pls_data$num_subj_lst,
      num_cond = pls_data$n_cond,
      method = 1,
      progress = FALSE
    )

    # U should be n_features x n_lv
    expect_equal(
      nrow(result$u), pls_data$n_features,
      label = sprintf("Trial %d: U rows = n_features", trial)
    )
    expect_equal(
      ncol(result$u), expected_n_lv,
      label = sprintf("Trial %d: U cols = n_lv", trial)
    )

    # V should be (n_groups * n_cond) x n_lv
    expect_equal(
      nrow(result$v), expected_n_lv,
      label = sprintf("Trial %d: V rows = n_groups*n_cond", trial)
    )

    # Brain scores should be total_obs x n_lv
    expect_equal(
      nrow(result$usc), total_obs,
      label = sprintf("Trial %d: brain scores rows = total_obs", trial)
    )
    expect_equal(
      ncol(result$usc), expected_n_lv,
      label = sprintf("Trial %d: brain scores cols = n_lv", trial)
    )
  }
})

# -----------------------------------------------------------------------------
# Property: P-values are Bounded [0, 1]
# -----------------------------------------------------------------------------

test_that("PROPERTY: permutation p-values are always in [0, 1]", {
  for (trial in 1:10) {
    set.seed(trial * 100)
    pls_data <- generate_random_pls_data(
      n_subj_range = c(5, 10),  # Smaller for speed
      n_features_range = c(20, 40)
    )

    result <- pls_analysis(
      datamat_lst = pls_data$datamat_lst,
      num_subj_lst = pls_data$num_subj_lst,
      num_cond = pls_data$n_cond,
      method = 1,
      num_perm = 20,
      progress = FALSE
    )

    pvals <- result$perm_result$sprob

    expect_true(
      all(pvals >= 0),
      label = sprintf("Trial %d: p-values >= 0", trial)
    )
    expect_true(
      all(pvals <= 1),
      label = sprintf("Trial %d: p-values <= 1", trial)
    )
  }
})

# -----------------------------------------------------------------------------
# Property: Bootstrap Ratios are Finite (when SE > 0)
# -----------------------------------------------------------------------------

test_that("PROPERTY: bootstrap ratios are finite", {
  for (trial in 1:10) {
    set.seed(trial * 100)
    pls_data <- generate_random_pls_data(
      n_subj_range = c(8, 12),  # Need enough for bootstrap
      n_features_range = c(20, 40)
    )

    result <- pls_analysis(
      datamat_lst = pls_data$datamat_lst,
      num_subj_lst = pls_data$num_subj_lst,
      num_cond = pls_data$n_cond,
      method = 1,
      num_boot = 20,
      progress = FALSE
    )

    bsr_vals <- result$boot_result$compare_u

    expect_true(
      all(is.finite(bsr_vals)),
      label = sprintf("Trial %d: BSR values finite", trial)
    )
  }
})

# -----------------------------------------------------------------------------
# Property: Determinism with Fixed Seed
# -----------------------------------------------------------------------------

test_that("PROPERTY: same seed produces identical results", {
  for (trial in 1:10) {
    seed <- trial * 1000
    pls_data <- generate_random_pls_data()

    set.seed(seed)
    result1 <- pls_analysis(
      datamat_lst = pls_data$datamat_lst,
      num_subj_lst = pls_data$num_subj_lst,
      num_cond = pls_data$n_cond,
      method = 1,
      num_perm = 10,
      progress = FALSE
    )

    set.seed(seed)
    result2 <- pls_analysis(
      datamat_lst = pls_data$datamat_lst,
      num_subj_lst = pls_data$num_subj_lst,
      num_cond = pls_data$n_cond,
      method = 1,
      num_perm = 10,
      progress = FALSE
    )

    expect_equal(result1$s, result2$s,
                 label = sprintf("Trial %d: singular values reproducible", trial))
    expect_equal(result1$u, result2$u,
                 label = sprintf("Trial %d: U reproducible", trial))
    expect_equal(result1$perm_result$sprob, result2$perm_result$sprob,
                 label = sprintf("Trial %d: p-values reproducible", trial))
  }
})

# -----------------------------------------------------------------------------
# Property: Accessor Consistency
# -----------------------------------------------------------------------------

test_that("PROPERTY: salience() equals result$u", {
  for (trial in 1:15) {
    set.seed(trial * 100)
    pls_data <- generate_random_pls_data()

    result <- pls_analysis(
      datamat_lst = pls_data$datamat_lst,
      num_subj_lst = pls_data$num_subj_lst,
      num_cond = pls_data$n_cond,
      method = 1,
      progress = FALSE
    )

    sal_all <- salience(result)
    expect_equal(sal_all, result$u,
                 label = sprintf("Trial %d: salience() == result$u", trial))

    # Test single LV extraction
    sal_1 <- salience(result, lv = 1)
    expect_equal(as.numeric(sal_1), result$u[, 1],
                 label = sprintf("Trial %d: salience(lv=1) correct", trial))
  }
})

test_that("PROPERTY: scores() dimensions match input", {
  for (trial in 1:15) {
    set.seed(trial * 100)
    pls_data <- generate_random_pls_data()

    total_obs <- sum(pls_data$num_subj_lst) * pls_data$n_cond

    result <- pls_analysis(
      datamat_lst = pls_data$datamat_lst,
      num_subj_lst = pls_data$num_subj_lst,
      num_cond = pls_data$n_cond,
      method = 1,
      progress = FALSE
    )

    brain_sc <- scores(result, type = "brain")
    design_sc <- scores(result, type = "design")

    expect_equal(nrow(brain_sc), total_obs,
                 label = sprintf("Trial %d: brain scores rows", trial))
    expect_equal(nrow(design_sc), total_obs,
                 label = sprintf("Trial %d: design scores rows", trial))
  }
})

test_that("PROPERTY: bsr() equals boot_result$compare_u", {
  for (trial in 1:10) {
    set.seed(trial * 100)
    pls_data <- generate_random_pls_data(
      n_subj_range = c(8, 12),
      n_features_range = c(20, 40)
    )

    result <- pls_analysis(
      datamat_lst = pls_data$datamat_lst,
      num_subj_lst = pls_data$num_subj_lst,
      num_cond = pls_data$n_cond,
      method = 1,
      num_boot = 20,
      progress = FALSE
    )

    bsr_all <- bsr(result)
    expect_equal(as.matrix(bsr_all), result$boot_result$compare_u,
                 label = sprintf("Trial %d: bsr() == compare_u", trial))
  }
})

# -----------------------------------------------------------------------------
# Property: All Methods Produce Valid Output
# -----------------------------------------------------------------------------

test_that("PROPERTY: method 1 (task) always produces valid output", {
  for (trial in 1:15) {
    set.seed(trial * 100)
    pls_data <- generate_random_pls_data()

    result <- pls_analysis(
      datamat_lst = pls_data$datamat_lst,
      num_subj_lst = pls_data$num_subj_lst,
      num_cond = pls_data$n_cond,
      method = 1,
      progress = FALSE
    )

    expect_s3_class(result, "pls_result")
    expect_s3_class(result, "pls_task")
    expect_true(all(is.finite(result$s)))
    expect_true(all(is.finite(result$u)))
  }
})

test_that("PROPERTY: method 3 (behavior) always produces valid output", {
  for (trial in 1:15) {
    set.seed(trial * 100)
    pls_data <- generate_random_pls_data()
    behavdata <- generate_behavior_data(pls_data)

    result <- pls_analysis(
      datamat_lst = pls_data$datamat_lst,
      num_subj_lst = pls_data$num_subj_lst,
      num_cond = pls_data$n_cond,
      method = 3,
      stacked_behavdata = behavdata,
      progress = FALSE
    )

    expect_s3_class(result, "pls_result")
    expect_s3_class(result, "pls_behavior")
    expect_true(all(is.finite(result$s)))
    expect_true(!is.null(result$lvcorrs))
  }
})

test_that("PROPERTY: method 4 (multiblock) always produces valid output", {
  for (trial in 1:15) {
    set.seed(trial * 100)
    pls_data <- generate_random_pls_data()
    behavdata <- generate_behavior_data(pls_data)

    result <- pls_analysis(
      datamat_lst = pls_data$datamat_lst,
      num_subj_lst = pls_data$num_subj_lst,
      num_cond = pls_data$n_cond,
      method = 4,
      stacked_behavdata = behavdata,
      progress = FALSE
    )

    expect_s3_class(result, "pls_result")
    expect_s3_class(result, "pls_multiblock")
    expect_true(all(is.finite(result$s)))
  }
})

# -----------------------------------------------------------------------------
# Property: Numerical Stability
# -----------------------------------------------------------------------------

test_that("PROPERTY: handles varying data magnitudes", {
  magnitudes <- c(1e-6, 1e-3, 1, 1e3, 1e6)

  for (mag in magnitudes) {
    set.seed(42)
    pls_data <- generate_random_pls_data(
      n_groups = 1,
      n_subj_range = c(10, 10),
      n_cond_range = c(3, 3),
      n_features_range = c(30, 30)
    )

    # Scale data
    pls_data$datamat_lst[[1]] <- pls_data$datamat_lst[[1]] * mag

    result <- pls_analysis(
      datamat_lst = pls_data$datamat_lst,
      num_subj_lst = pls_data$num_subj_lst,
      num_cond = pls_data$n_cond,
      method = 1,
      progress = FALSE
    )

    expect_true(
      all(is.finite(result$s)),
      label = sprintf("Magnitude %g: singular values finite", mag)
    )
    expect_true(
      all(is.finite(result$u)),
      label = sprintf("Magnitude %g: U finite", mag)
    )
  }
})

test_that("PROPERTY: handles wide and tall data shapes", {
  shapes <- list(
    wide = list(n_subj = c(5, 5), n_cond = 2, n_feat = 200),   # p > n
    tall = list(n_subj = c(30, 30), n_cond = 4, n_feat = 20),  # n > p
    square = list(n_subj = c(10, 10), n_cond = 3, n_feat = 60) # n ≈ p
  )

  for (shape_name in names(shapes)) {
    set.seed(42)
    shape <- shapes[[shape_name]]

    datamat_lst <- lapply(shape$n_subj, function(n) {
      matrix(rnorm(n * shape$n_cond * shape$n_feat), n * shape$n_cond, shape$n_feat)
    })

    result <- pls_analysis(
      datamat_lst = datamat_lst,
      num_subj_lst = shape$n_subj,
      num_cond = shape$n_cond,
      method = 1,
      progress = FALSE
    )

    expect_s3_class(result, "pls_result")
    expect_true(
      all(is.finite(result$s)),
      label = sprintf("%s shape: finite singular values", shape_name)
    )
  }
})

# -----------------------------------------------------------------------------
# Property: Cross-Correlation Modes
# -----------------------------------------------------------------------------

test_that("PROPERTY: all correlation modes produce valid output", {
  cormodes <- c(0, 2, 4, 6)  # pearson, covariance, cosine, dot

  for (cormode in cormodes) {
    set.seed(42)
    pls_data <- generate_random_pls_data(
      n_groups = 1,
      n_subj_range = c(10, 10),
      n_cond_range = c(3, 3)
    )
    behavdata <- generate_behavior_data(pls_data)

    result <- pls_analysis(
      datamat_lst = pls_data$datamat_lst,
      num_subj_lst = pls_data$num_subj_lst,
      num_cond = pls_data$n_cond,
      method = 3,
      stacked_behavdata = behavdata,
      cormode = cormode,
      progress = FALSE
    )

    expect_s3_class(result, "pls_result")
    expect_true(
      all(is.finite(result$s)),
      label = sprintf("cormode %d: finite singular values", cormode)
    )
  }
})

# -----------------------------------------------------------------------------
# Property: Mean-Centering Types
# -----------------------------------------------------------------------------

test_that("PROPERTY: all mean-centering types produce valid output", {
  mc_types <- 0:3

  for (mc_type in mc_types) {
    set.seed(42)
    pls_data <- generate_random_pls_data(
      n_groups = 2,  # Need 2 groups for some centering types
      n_subj_range = c(8, 12),
      n_cond_range = c(3, 3)
    )

    result <- pls_analysis(
      datamat_lst = pls_data$datamat_lst,
      num_subj_lst = pls_data$num_subj_lst,
      num_cond = pls_data$n_cond,
      method = 1,
      meancentering_type = mc_type,
      progress = FALSE
    )

    expect_s3_class(result, "pls_result")
    expect_true(
      all(is.finite(result$s)),
      label = sprintf("mc_type %d: finite singular values", mc_type)
    )
  }
})

# -----------------------------------------------------------------------------
# Property: Monotonicity of Variance Explained
# -----------------------------------------------------------------------------

test_that("PROPERTY: variance explained is monotonically decreasing", {
  for (trial in 1:15) {
    set.seed(trial * 100)
    pls_data <- generate_random_pls_data()

    result <- pls_analysis(
      datamat_lst = pls_data$datamat_lst,
      num_subj_lst = pls_data$num_subj_lst,
      num_cond = pls_data$n_cond,
      method = 1,
      progress = FALSE
    )

    var_exp <- (result$s^2) / sum(result$s^2) * 100

    # Each LV should explain >= the next one
    for (i in seq_len(length(var_exp) - 1)) {
      expect_true(
        var_exp[i] >= var_exp[i + 1] - 1e-10,  # Small tolerance for floating point
        label = sprintf("Trial %d: LV%d >= LV%d variance", trial, i, i + 1)
      )
    }
  }
})
