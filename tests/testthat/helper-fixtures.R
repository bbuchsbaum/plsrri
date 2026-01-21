# Helper functions for creating mock pls_result fixtures
#
# These functions create realistic but synthetic pls_result objects
# for testing Shiny modules without running actual PLS analyses.

#' Create a minimal NeuroVol mask
#'
#' @param dim Dimensions of the mask volume (default: c(10, 10, 10))
#' @return A NeuroVol object (or array with class if neuroim2 unavailable)
make_mock_mask <- function(dim = c(10, 10, 10)) {
  set.seed(123)  # Reproducibility

  # Create array with ~50% "brain" voxels
  vol <- array(0, dim = dim)
  n_total <- prod(dim)
  n_brain <- floor(n_total / 2)

  # Randomly select brain voxels
  brain_indices <- sample(n_total, n_brain)
  vol[brain_indices] <- 1

  # Try to use neuroim2 if available
  if (requireNamespace("neuroim2", quietly = TRUE)) {
    space <- neuroim2::NeuroSpace(dim = dim, spacing = c(1, 1, 1))
    mask <- neuroim2::NeuroVol(vol, space)
    return(mask)
  }

  # Fallback: return array with class attribute
  structure(vol, class = c("mock_neurovol", "array"))
}

#' Create a mock pls_result object
#'
#' Main factory function for creating test fixtures with configurable components.
#'
#' @param n_voxels Number of voxels/features (default: 500)
#' @param n_lv Number of latent variables (default: 3)
#' @param n_obs Number of observations (default: 30)
#' @param include_boot Include bootstrap results (default: FALSE)
#' @param include_perm Include permutation results (default: FALSE)
#' @param include_mask Include a brain mask (default: FALSE)
#' @param method PLS method (1-6, default: 1 for mean-centering task PLS)
#' @param num_boot Number of bootstrap samples if include_boot (default: 100)
#' @param num_perm Number of permutations if include_perm (default: 100)
#' @return A pls_result object
make_mock_pls_result <- function(n_voxels = 500,
                                  n_lv = 3,
                                  n_obs = 30,
                                  include_boot = FALSE,
                                  include_perm = FALSE,
                                  include_mask = FALSE,
                                  method = 1L,
                                  num_boot = 100L,
                                  num_perm = 100L) {
  set.seed(42)  # Reproducibility


  # u: salience matrix (voxels x LVs) - brain loadings
  u <- matrix(rnorm(n_voxels * n_lv), nrow = n_voxels, ncol = n_lv)
  colnames(u) <- paste0("LV", seq_len(n_lv))

  # s: singular values (descending positive values)
  s <- sort(abs(rnorm(n_lv, mean = 10, sd = 3)), decreasing = TRUE)
  names(s) <- paste0("LV", seq_len(n_lv))

  # v: design/behavior loadings matrix (observations or conditions x LVs)
  v <- matrix(rnorm(n_obs * n_lv), nrow = n_obs, ncol = n_lv)
  colnames(v) <- paste0("LV", seq_len(n_lv))

  # usc: brain scores (observations x LVs)
  usc <- matrix(rnorm(n_obs * n_lv), nrow = n_obs, ncol = n_lv)
  colnames(usc) <- paste0("LV", seq_len(n_lv))

  # vsc: design scores (observations x LVs)
  vsc <- matrix(rnorm(n_obs * n_lv), nrow = n_obs, ncol = n_lv)
  colnames(vsc) <- paste0("LV", seq_len(n_lv))

  # Bootstrap result
  boot_result <- NULL
  if (include_boot) {
    # compare_u: bootstrap ratios (BSR) - same dims as u
    compare_u <- matrix(rnorm(n_voxels * n_lv, mean = 0, sd = 2),
                        nrow = n_voxels, ncol = n_lv)
    colnames(compare_u) <- paste0("LV", seq_len(n_lv))

    # u_se: standard errors - same dims as u
    u_se <- matrix(abs(rnorm(n_voxels * n_lv, mean = 0.5, sd = 0.1)),
                   nrow = n_voxels, ncol = n_lv)
    colnames(u_se) <- paste0("LV", seq_len(n_lv))

    boot_result <- plsrri:::new_pls_boot_result(
      num_boot = num_boot,
      boot_type = "strat",
      compare_u = compare_u,
      u_se = u_se,
      clim = 95
    )
  }

  # Permutation result
  perm_result <- NULL
  if (include_perm) {
    # sp: count of permuted singular values >= observed
    sp <- sample(0:num_perm, n_lv, replace = TRUE)

    # sprob: p-values (sp / num_perm)
    sprob <- sp / num_perm
    names(sprob) <- paste0("LV", seq_len(n_lv))

    perm_result <- plsrri:::new_pls_perm_result(
      num_perm = num_perm,
      sp = sp,
      sprob = sprob
    )
  }

  # Mask
  mask <- NULL
  if (include_mask) {
    mask <- make_mock_mask()
  }

  # Create result using internal constructor
  plsrri:::new_pls_result(
    method = method,
    u = u,
    s = s,
    v = v,
    usc = usc,
    vsc = vsc,
    perm_result = perm_result,
    boot_result = boot_result,
    num_subj_lst = c(10L),
    num_cond = 3L,
    mask = mask
  )
}

#' Create a mock pls_spec object
#'
#' For testing setup/specification modules.
#'
#' @param n_groups Number of groups (default: 1)
#' @param n_subj Number of subjects per group (default: 10)
#' @param n_cond Number of conditions (default: 3)
#' @param n_voxels Number of voxels/features (default: 500)
#' @return A pls_spec object
make_mock_spec <- function(n_groups = 1,
                           n_subj = 10,
                           n_cond = 3,
                           n_voxels = 500) {
  set.seed(123)  # Reproducibility

  # Create data matrices (one per group)
  n_obs_per_group <- n_subj * n_cond
  datamat_lst <- lapply(seq_len(n_groups), function(g) {
    matrix(rnorm(n_obs_per_group * n_voxels),
           nrow = n_obs_per_group,
           ncol = n_voxels)
  })

  # Build spec with data
  spec <- pls_spec()
  spec$datamat_lst <- datamat_lst
  spec$num_subj_lst <- rep(n_subj, n_groups)
  spec$num_cond <- n_cond
  spec$method <- 1L
  spec$groups <- paste0("Group", seq_len(n_groups))
  spec$conditions <- paste0("Cond", seq_len(n_cond))

  spec
}

#' Create a single-LV result (edge case)
#'
#' @return A pls_result with only 1 latent variable
make_single_lv_result <- function() {
  make_mock_pls_result(
    n_lv = 1,
    include_boot = TRUE,
    include_perm = TRUE
  )
}

#' Create a many-LV result (edge case)
#'
#' @return A pls_result with 10+ latent variables
make_many_lv_result <- function() {
  make_mock_pls_result(
    n_lv = 10,
    n_obs = 100,
    include_boot = TRUE,
    include_perm = TRUE
  )
}

#' Load a pre-built fixture by name
#'
#' @param name Fixture name without .rds extension
#' @return The fixture object
load_fixture <- function(name) {
  path <- testthat::test_path("fixtures", paste0(name, ".rds"))
  readRDS(path)
}
