#' S3 Classes for PLS Analysis
#'
#' @description
#' Core S3 class constructors for the plsrri package. These classes represent
#' PLS specifications, results, and inference components.
#'
#' @name pls-classes
NULL

# --- PLS Methods Enum ---
#' @keywords internal
PLS_METHODS <- list(

  MEAN_CENTERING_TASK = 1L,
  NON_ROTATED_TASK = 2L,

  REGULAR_BEHAVIOR = 3L,
  REGULAR_MULTIBLOCK = 4L,
  NON_ROTATED_BEHAVIOR = 5L,
  NON_ROTATED_MULTIBLOCK = 6L
)

#' @keywords internal
PLS_METHOD_NAMES <- c(
  "Mean-Centering Task PLS",
  "Non-Rotated Task PLS",
  "Regular Behavior PLS",
  "Regular Multiblock PLS",
  "Non-Rotated Behavior PLS",
  "Non-Rotated Multiblock PLS"
)

#' Map PLS Method Integer to Canonical String Name
#'
#' Single source of truth for integer-to-name mapping. All code paths
#' that convert method integers to strings should call this function.
#'
#' @param method Integer method code (1-6).
#' @return Character string: one of \code{"task"}, \code{"task_nonrotated"},
#'   \code{"behavior"}, \code{"multiblock"}, \code{"behavior_nonrotated"},
#'   \code{"multiblock_nonrotated"}.
#' @export
pls_method_int_to_name <- function(method) {
  method <- as.integer(method)[1]
  switch(
    as.character(method),
    "1" = "task",
    "2" = "task_nonrotated",
    "3" = "behavior",
    "4" = "multiblock",
    "5" = "behavior_nonrotated",
    "6" = "multiblock_nonrotated",
    stop("Unknown PLS method integer: ", method, call. = FALSE)
  )
}

#' Map PLS Method (Integer or String) to Human-Readable Label
#'
#' Returns the display label from \code{PLS_METHOD_NAMES} for any
#' recognised method identifier (integer 1-6 or canonical string name).
#'
#' @param method Integer (1-6) or character method identifier.
#' @return Human-readable label string.
#' @export
pls_method_label <- function(method) {
  key <- as.character(method)[1]
  # If it looks like an integer, use PLS_METHOD_NAMES directly

  idx <- suppressWarnings(as.integer(key))
  if (!is.na(idx) && idx >= 1L && idx <= 6L) {
    return(PLS_METHOD_NAMES[idx])
  }
  # Otherwise resolve string name to integer first
  int_val <- tryCatch(
    .resolve_method(key),
    error = function(e) NA_integer_
  )
  if (is.na(int_val)) return("Unknown")
  PLS_METHOD_NAMES[int_val]
}

#' @keywords internal
CORMODE_NAMES <- c(
  `0` = "pearson",
  `2` = "covariance",
  `4` = "cosine",
  `6` = "dot_product"
)

# --- PLS Specification ---

#' Create a PLS Specification Object
#'
#' @description
#' Creates an empty PLS specification that can be populated via the builder API.
#' This is the starting point for defining a PLS analysis.
#'
#' @param mask Optional brain mask (NeuroVol from neuroim2)
#'
#' @return A `pls_spec` object
#' @export
#'
#' @examples
#' spec <- pls_spec()
pls_spec <- function(mask = NULL) {
  structure(
    list(
      mask = mask,
      datamat_lst = list(),
      num_subj_lst = integer(0),
      num_cond = NULL,
      stacked_behavdata = NULL,
      stacked_designdata = NULL,
      method = 1L,
      num_perm = 0L,
      num_boot = 0L,
      num_split = 0L,
      clim = 95,
      is_struct = FALSE,
      bscan = NULL,
      meancentering_type = 0L,
      cormode = 0L,
      boot_type = "strat",
      groups = NULL,
      site = NULL,
      conditions = NULL,
      trial_data = NULL,
      ws_seed_info = NULL,
      .trial_raw = FALSE,
      .skip_site_diagnostics = FALSE
    ),
    class = "pls_spec"
  )
}

#' @export
print.pls_spec <- function(x, ...) {
  cli::cli_h1("PLS Specification")

  cli::cli_text("Method: {PLS_METHOD_NAMES[x$method]}")

  if (isTRUE(x$.trial_raw) || is.list(x$trial_data)) {
    cli::cli_text("Preprocessing: within-subject seed correlation maps")
  }

  if (length(x$datamat_lst) > 0) {
    n_groups <- length(x$datamat_lst)
    n_obs <- sum(sapply(x$datamat_lst, nrow))
    n_vox <- ncol(x$datamat_lst[[1]])
    cli::cli_text("Groups: {n_groups}")
    cli::cli_text("Total observations: {n_obs}")
    cli::cli_text("Voxels/features: {n_vox}")
  } else {
    cli::cli_text("No data loaded yet")
  }

  if (is.list(x$trial_data) && !length(x$datamat_lst)) {
    n_subj <- length(x$trial_data$beta_lst %||% list())
    cli::cli_text("Trial-level subjects: {n_subj}")
  }

  if (!is.null(x$num_cond)) {
    cli::cli_text("Conditions: {x$num_cond}")
  }

  if (!is.null(x$stacked_behavdata)) {
    cli::cli_text("Behavior measures: {ncol(x$stacked_behavdata)}")
  }

  if (!is.null(x$site)) {
    cli::cli_text("Sites: {length(unique(as.character(x$site)))}")
  }

  cli::cli_text("Permutations: {x$num_perm}")
  cli::cli_text("Bootstraps: {x$num_boot}")

  invisible(x)
}

# --- PLS Result ---

#' Create a PLS Result Object
#'
#' @param method Integer, PLS method (1-6)
#' @param u Salience (brain loadings) matrix
#' @param s Singular values
#' @param v Design/behavior loadings matrix
#' @param usc Brain scores
#' @param vsc Design/behavior scores
#' @param datamatcorrs_lst Correlation matrices by group (behavior PLS)
#' @param lvcorrs Latent variable correlations (behavior PLS)
#' @param perm_result Permutation result object
#' @param boot_result Bootstrap result object
#' @param splithalf_result Split-half result object
#' @param num_subj_lst Number of subjects per group
#' @param num_cond Number of conditions
#' @param bscan Conditions used for behavior block (multiblock)
#' @param stacked_designdata Design contrast matrix (non-rotated methods)
#' @param stacked_behavdata Behavior data matrix
#' @param other_input List of other inputs (meancentering_type, cormode)
#' @param TBv Task/Behavior v stored separately (multiblock)
#' @param TBusc Task/Behavior brain scores (multiblock)
#' @param TBvsc Task/Behavior design scores (multiblock)
#' @param is_struct Structure PLS flag
#' @param mask Brain mask (optional)
#'
#' @return A `pls_result` object
#' @keywords internal
new_pls_result <- function(method,
                           u, s, v,
                           usc = NULL, vsc = NULL,
                           datamatcorrs_lst = NULL,
                           lvcorrs = NULL,
                           perm_result = NULL,
                           boot_result = NULL,
                           splithalf_result = NULL,
                           num_subj_lst = NULL,
                           num_cond = NULL,
                           bscan = NULL,
                           stacked_designdata = NULL,
                           stacked_behavdata = NULL,
                           other_input = NULL,
                           TBv = NULL,
                           TBusc = NULL,
                           TBvsc = NULL,
                           is_struct = FALSE,
                           mask = NULL,
                           site = NULL,
                           site_diagnostics = NULL) {

  # Determine class based on method
  method_class <- switch(
    method,
    "pls_task",           # 1
    "pls_task_nonrot",    # 2
    "pls_behavior",       # 3
    "pls_multiblock",     # 4
    "pls_behavior_nonrot", # 5
    "pls_multiblock_nonrot" # 6
  )

  structure(
    list(
      method = method,
      u = u,
      s = s,
      v = v,
      usc = usc,
      vsc = vsc,
      datamatcorrs_lst = datamatcorrs_lst,
      lvcorrs = lvcorrs,
      perm_result = perm_result,
      boot_result = boot_result,
      splithalf_result = splithalf_result,
      num_subj_lst = num_subj_lst,
      num_cond = num_cond,
      bscan = bscan,
      stacked_designdata = stacked_designdata,
      stacked_behavdata = stacked_behavdata,
      other_input = other_input,
      TBv = TBv,
      TBusc = TBusc,
      TBvsc = TBvsc,
      is_struct = is_struct,
      mask = mask,
      site = site,
      site_diagnostics = site_diagnostics
    ),
    class = c(method_class, "pls_result")
  )
}

#' @export
print.pls_result <- function(x, ...) {
  cli::cli_h1("PLS Result")
  cli::cli_text("Method: {PLS_METHOD_NAMES[x$method]}")
  cli::cli_text("Latent Variables: {length(x$s)}")
  cli::cli_text("Voxels/Features: {nrow(x$u)}")

  if (!is.null(x$perm_result)) {
    sig_lvs <- sum(x$perm_result$sprob < 0.05)
    cli::cli_text("Significant LVs (p < 0.05): {sig_lvs} of {length(x$s)}")
  }

  if (!is.null(x$boot_result)) {
    cli::cli_text("Bootstrap samples: {x$boot_result$num_boot}")
    cli::cli_text("Confidence level: {x$boot_result$clim}%")
  }

  invisible(x)
}

#' @export
summary.pls_result <- function(object, ...) {
  cat("\n=== PLS Analysis Summary ===\n\n")
  cat("Method:", PLS_METHOD_NAMES[object$method], "\n")
  cat("Number of LVs:", length(object$s), "\n")
  cat("Variance explained by each LV:\n")

  total_var <- sum(object$s^2)
  var_exp <- (object$s^2 / total_var) * 100
  cum_var <- cumsum(var_exp)

  df <- data.frame(
    LV = seq_along(object$s),
    SingularValue = round(object$s, 4),
    VarExplained = round(var_exp, 2),
    CumVarExplained = round(cum_var, 2)
  )

  if (!is.null(object$perm_result)) {
    df$pvalue <- round(object$perm_result$sprob, 4)
  }

  print(df, row.names = FALSE)

  invisible(object)
}

# --- Permutation Result ---

#' Create a Permutation Result Object
#'
#' @param num_perm Number of permutations
#' @param sp Count of permuted singular values >= observed
#' @param sprob Probability (sp / num_perm)
#' @param permsamp Permutation sample matrix
#' @param Tpermsamp Task permutation samples (multiblock)
#' @param Bpermsamp Behavior permutation samples (multiblock)
#'
#' @return A `pls_perm_result` object
#' @keywords internal
new_pls_perm_result <- function(num_perm, sp, sprob, permsamp = NULL,
                                 Tpermsamp = NULL, Bpermsamp = NULL) {
  structure(
    list(
      num_perm = num_perm,
      sp = sp,
      sprob = sprob,
      permsamp = permsamp,
      Tpermsamp = Tpermsamp,
      Bpermsamp = Bpermsamp
    ),
    class = "pls_perm_result"
  )
}

#' @export
print.pls_perm_result <- function(x, ...) {
  cli::cli_h2("Permutation Test Results")
  cli::cli_text("Permutations: {x$num_perm}")

  df <- data.frame(
    LV = seq_along(x$sprob),
    Count = x$sp,
    pvalue = round(x$sprob, 4)
  )
  df$Sig <- ifelse(df$pvalue < 0.05, "*", "")
  df$Sig <- ifelse(df$pvalue < 0.01, "**", df$Sig)
  df$Sig <- ifelse(df$pvalue < 0.001, "***", df$Sig)

  print(df, row.names = FALSE)

  invisible(x)
}

# --- Bootstrap Result ---

#' Create a Bootstrap Result Object
#'
#' @param num_boot Number of bootstrap samples
#' @param boot_type "strat" or "nonstrat"
#' @param compare_u Bootstrap ratios (u / u_se)
#' @param u_se Standard error of saliences
#' @param clim Confidence level (0-100)
#' @param bootsamp Bootstrap sample matrix
#' @param bootsamp_4beh Bootstrap samples for behavior (if different)
#' @param distrib Distribution of bootstrap statistics
#' @param prop Proportions/probabilities
#' @param Tdistrib Task-block distribution (multiblock)
#' @param Tprop Task-block proportions (multiblock)
#' @param usc2 Brain scores from mean-centered data (task PLS)
#' @param orig_usc Original brain scores (task PLS)
#' @param ulusc Upper confidence bound for usc
#' @param llusc Lower confidence bound for usc
#' @param orig_corr Original correlations (behavior PLS)
#' @param ulcorr Upper confidence bound for correlations
#' @param llcorr Lower confidence bound for correlations
#' @param nonrotated_boot Whether using non-rotated bootstrap
#' @param num_LowVariability_behav_boots Count of low variability samples
#' @param badbeh Bad behavior indices
#' @param countnewtotal Count of resampled bad behavior
#'
#' @return A `pls_boot_result` object
#' @keywords internal
new_pls_boot_result <- function(num_boot,
                                 boot_type = "strat",
                                 compare_u = NULL,
                                 u_se = NULL,
                                 clim = 95,
                                 bootsamp = NULL,
                                 bootsamp_4beh = NULL,
                                 distrib = NULL,
                                 prop = NULL,
                                 Tdistrib = NULL,
                                 Tprop = NULL,
                                 usc2 = NULL,
                                 orig_usc = NULL,
                                 ulusc = NULL,
                                 llusc = NULL,
                                 ulusc_adj = NULL,
                                 llusc_adj = NULL,
                                 orig_corr = NULL,
                                 ulcorr = NULL,
                                 llcorr = NULL,
                                 ulcorr_adj = NULL,
                                 llcorr_adj = NULL,
                                 nonrotated_boot = FALSE,
                                 num_LowVariability_behav_boots = NULL,
                                 badbeh = NULL,
                                 countnewtotal = NULL) {
  structure(
    list(
      num_boot = num_boot,
      boot_type = boot_type,
      compare_u = compare_u,
      u_se = u_se,
      clim = clim,
      bootsamp = bootsamp,
      bootsamp_4beh = bootsamp_4beh,
      distrib = distrib,
      prop = prop,
      Tdistrib = Tdistrib,
      Tprop = Tprop,
      usc2 = usc2,
      orig_usc = orig_usc,
      ulusc = ulusc,
      llusc = llusc,
      ulusc_adj = ulusc_adj,
      llusc_adj = llusc_adj,
      orig_corr = orig_corr,
      ulcorr = ulcorr,
      llcorr = llcorr,
      ulcorr_adj = ulcorr_adj,
      llcorr_adj = llcorr_adj,
      nonrotated_boot = nonrotated_boot,
      num_LowVariability_behav_boots = num_LowVariability_behav_boots,
      badbeh = badbeh,
      countnewtotal = countnewtotal
    ),
    class = "pls_boot_result"
  )
}

#' @export
print.pls_boot_result <- function(x, ...) {
  cli::cli_h2("Bootstrap Results")
  cli::cli_text("Bootstrap samples: {x$num_boot}")
  cli::cli_text("Bootstrap type: {x$boot_type}")
  cli::cli_text("Confidence level: {x$clim}%")

  if (!is.null(x$compare_u)) {
    n_vox <- nrow(x$compare_u)
    n_lv <- ncol(x$compare_u)
    cli::cli_text("Bootstrap ratios computed for {n_vox} voxels x {n_lv} LVs")
  }

  invisible(x)
}

# --- Split-Half Result ---

#' Create a Split-Half Validation Result Object
#'
#' @param num_outer_perm Number of outer permutations
#' @param num_split Number of splits
#' @param orig_ucorr Original u (brain) correlation
#' @param orig_vcorr Original v (design/behavior) correlation
#' @param ucorr_prob Probability for u correlation
#' @param vcorr_prob Probability for v correlation
#' @param ucorr_ul Upper limit for u correlation
#' @param ucorr_ll Lower limit for u correlation
#' @param vcorr_ul Upper limit for v correlation
#' @param vcorr_ll Lower limit for v correlation
#'
#' @return A `pls_splithalf_result` object
#' @keywords internal
new_pls_splithalf_result <- function(num_outer_perm,
                                      num_split,
                                      orig_ucorr = NULL,
                                      orig_vcorr = NULL,
                                      ucorr_prob = NULL,
                                      vcorr_prob = NULL,
                                      ucorr_ul = NULL,
                                      ucorr_ll = NULL,
                                      vcorr_ul = NULL,
                                      vcorr_ll = NULL) {
  structure(
    list(
      num_outer_perm = num_outer_perm,
      num_split = num_split,
      orig_ucorr = orig_ucorr,
      orig_vcorr = orig_vcorr,
      ucorr_prob = ucorr_prob,
      vcorr_prob = vcorr_prob,
      ucorr_ul = ucorr_ul,
      ucorr_ll = ucorr_ll,
      vcorr_ul = vcorr_ul,
      vcorr_ll = vcorr_ll
    ),
    class = "pls_splithalf_result"
  )
}
