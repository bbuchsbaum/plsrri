# Pure computation functions for brain viewer modules
#
# These functions contain extracted business logic from mod_brain_viewer.R,
# mod_inspector.R, and mod_filter_bar.R. They operate on plain R objects
# (not reactives) and can be unit tested without Shiny context.

#' Get filter defaults with fallbacks
#'
#' Takes a filters list (may have NULL values) and returns list with defaults applied.
#' Extracted from mod_brain_viewer.R filter extraction logic.
#'
#' @param filters Named list of filter values (may contain NULLs)
#' @param defaults Named list of default values to use when filters are NULL
#' @return Named list with defaults applied for NULL values
#' @keywords internal
get_filter_defaults <- function(filters,
                                 defaults = list(lv = 1L, bsr_threshold = 3.0, what = "bsr")) {
  if (is.null(filters) || length(filters) == 0) {
    return(defaults)
  }

  result <- list()
  for (name in names(defaults)) {
    val <- filters[[name]]
    result[[name]] <- if (is.null(val)) defaults[[name]] else val
  }

  result
}

#' Format colorbar label
#'
#' Returns descriptive label for brain viewer colorbar based on display type.
#' Extracted from mod_brain_viewer.R colorbar_info renderUI.
#'
#' @param what Character: "bsr" or "salience"
#' @param threshold Numeric BSR threshold (used only when what == "bsr")
#' @return Character label for colorbar
#' @keywords internal
format_colorbar_label <- function(what, threshold = 3.0) {
  if (is.null(what) || !what %in% c("bsr", "salience")) {
    what <- "bsr"
  }

  if (what == "bsr") {
    sprintf("Bootstrap Ratio (|BSR| > %.1f)", threshold)
  } else {
    "Salience weights"
  }
}

#' Compute plot height based on view mode
#'
#' Returns appropriate plot height for brain viewer based on view mode.
#' Extracted from mod_brain_viewer.R renderPlot height function.
#'
#' @param view_mode Character: "ortho" or "montage"
#' @return Integer height in pixels
#' @keywords internal
compute_plot_height <- function(view_mode) {
  if (is.null(view_mode) || !view_mode %in% c("ortho", "montage")) {
    return(350L)
  }

  if (view_mode == "ortho") {
    400L
  } else {
    350L
  }
}

#' Compute BSR summary statistics
#'
#' Computes summary statistics for bootstrap ratio values including counts
#' of significant positive/negative voxels and extreme values.
#' Extracted from mod_inspector.R lv_details renderUI.
#'
#' @param bsr_values Numeric vector of BSR values
#' @param threshold Numeric threshold for significance (default: 3)
#' @return Named list with n_pos, n_neg, max_pos, max_neg, or NULL if input is NULL
#' @keywords internal
compute_bsr_summary <- function(bsr_values, threshold = 3) {
  if (is.null(bsr_values) || length(bsr_values) == 0) {
    return(NULL)
  }

  # Handle all-NA case
  if (all(is.na(bsr_values))) {
    return(list(
      n_pos = 0L,
      n_neg = 0L,
      max_pos = NA_real_,
      max_neg = NA_real_
    ))
  }

  list(
    n_pos = sum(bsr_values > threshold, na.rm = TRUE),
    n_neg = sum(bsr_values < -threshold, na.rm = TRUE),
    max_pos = max(bsr_values, na.rm = TRUE),
    max_neg = min(bsr_values, na.rm = TRUE)
  )
}

#' Compute LV variance statistics
#'
#' Computes variance explained and cumulative variance for a latent variable.
#' Extracted from mod_inspector.R lv_details renderUI.
#'
#' @param singular_values Numeric vector of singular values (result$s)
#' @param lv_index Integer index of the LV to compute stats for
#' @return Named list with var_explained and cum_var_explained as percentages
#' @keywords internal
compute_lv_stats <- function(singular_values, lv_index) {
  if (is.null(singular_values) || length(singular_values) == 0) {
    return(list(
      var_explained = NA_real_,
      cum_var_explained = NA_real_
    ))
  }

  if (is.null(lv_index) || lv_index < 1 || lv_index > length(singular_values)) {
    return(list(
      var_explained = NA_real_,
      cum_var_explained = NA_real_
    ))
  }

  # Variance explained as percentage: s^2 / sum(s^2) * 100
  var_exp <- (singular_values^2 / sum(singular_values^2)) * 100
  cum_var <- cumsum(var_exp)

  list(
    var_explained = var_exp[lv_index],
    cum_var_explained = cum_var[lv_index]
  )
}

#' Build LV choices for selectInput
#'
#' Builds named character vector of choices for LV selection dropdown.
#' Adds asterisk to significant LVs (p < 0.05).
#' Extracted from mod_filter_bar.R observe block.
#'
#' @param n_lv Integer number of latent variables
#' @param p_values Optional numeric vector of p-values (from perm_result$sprob)
#' @return Named character vector suitable for selectInput choices
#' @keywords internal
build_lv_choices <- function(n_lv, p_values = NULL) {
  if (is.null(n_lv) || n_lv < 1) {
    return(c("All" = "all"))
  }

  # Start with "All" option
  choices <- c("All" = "all")

  for (i in seq_len(n_lv)) {
    # Add significance indicator if p-values provided
    if (!is.null(p_values) && length(p_values) >= i && !is.na(p_values[i])) {
      if (p_values[i] < 0.05) {
        label <- sprintf("LV%d *", i)
      } else {
        label <- sprintf("LV%d", i)
      }
    } else {
      label <- sprintf("LV%d", i)
    }
    choices[label] <- as.character(i)
  }

  choices
}
