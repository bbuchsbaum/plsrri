# Test fct_brain_viewer pure functions
#
# These tests run WITHOUT Shiny context - plain testthat.
# Unit tests for business logic extracted from brain viewer modules.

# Source the functions directly (works both installed + dev mode)
module_file <- system.file("shiny/R/fct_brain_viewer.R", package = "plsrri")
if (module_file == "") {
  module_file <- testthat::test_path("../../inst/shiny/R/fct_brain_viewer.R")
}
source(module_file, local = FALSE)

describe("get_filter_defaults()", {

  it("returns all defaults when filters is NULL", {
    result <- get_filter_defaults(NULL)
    expect_equal(result$lv, 1L)
    expect_equal(result$bsr_threshold, 3.0)
    expect_equal(result$what, "bsr")
  })

  it("returns all defaults when filters is empty list", {
    result <- get_filter_defaults(list())
    expect_equal(result$lv, 1L)
    expect_equal(result$bsr_threshold, 3.0)
    expect_equal(result$what, "bsr")
  })

  it("returns all defaults when all filter values are NULL", {
    result <- get_filter_defaults(list(lv = NULL, bsr_threshold = NULL, what = NULL))
    expect_equal(result$lv, 1L)
    expect_equal(result$bsr_threshold, 3.0)
    expect_equal(result$what, "bsr")
  })

  it("keeps provided values and fills NULLs with defaults", {
    result <- get_filter_defaults(list(lv = NULL, bsr_threshold = 2.0, what = NULL))
    expect_equal(result$lv, 1L)  # default
    expect_equal(result$bsr_threshold, 2.0)  # provided
    expect_equal(result$what, "bsr")  # default
  })

  it("returns all provided values when none are NULL", {
    result <- get_filter_defaults(list(lv = 3L, bsr_threshold = 2.5, what = "salience"))
    expect_equal(result$lv, 3L)
    expect_equal(result$bsr_threshold, 2.5)
    expect_equal(result$what, "salience")
  })

  it("accepts custom defaults", {
    result <- get_filter_defaults(
      list(lv = NULL, bsr_threshold = NULL, what = NULL),
      defaults = list(lv = 2L, bsr_threshold = 4.0, what = "salience")
    )
    expect_equal(result$lv, 2L)
    expect_equal(result$bsr_threshold, 4.0)
    expect_equal(result$what, "salience")
  })

})

describe("format_colorbar_label()", {

  it("returns BSR label with threshold 3.0", {
    result <- format_colorbar_label("bsr", 3.0)
    expect_equal(result, "Bootstrap Ratio (|BSR| > 3.0)")
  })

  it("returns BSR label with threshold 2.5", {
    result <- format_colorbar_label("bsr", 2.5)
    expect_equal(result, "Bootstrap Ratio (|BSR| > 2.5)")
  })

  it("returns BSR label with threshold 5.0", {
    result <- format_colorbar_label("bsr", 5.0)
    expect_equal(result, "Bootstrap Ratio (|BSR| > 5.0)")
  })

  it("returns salience label", {
    result <- format_colorbar_label("salience", 3.0)
    expect_equal(result, "Salience weights")
  })

  it("ignores threshold for salience", {
    result <- format_colorbar_label("salience", 999)
    expect_equal(result, "Salience weights")
  })

  it("defaults to bsr for NULL what", {
    result <- format_colorbar_label(NULL, 3.0)
    expect_equal(result, "Bootstrap Ratio (|BSR| > 3.0)")
  })

  it("defaults to bsr for unknown what value", {
    result <- format_colorbar_label("unknown", 3.0)
    expect_equal(result, "Bootstrap Ratio (|BSR| > 3.0)")
  })

})

describe("compute_plot_height()", {

  it("returns 400 for ortho view", {
    expect_equal(compute_plot_height("ortho"), 400L)
  })

  it("returns 350 for montage view", {
    expect_equal(compute_plot_height("montage"), 350L)
  })

  it("returns 350 for NULL view_mode", {
    expect_equal(compute_plot_height(NULL), 350L)
  })

  it("returns 350 for unknown view_mode", {
    expect_equal(compute_plot_height("unknown"), 350L)
  })

  it("returns integer type", {
    result <- compute_plot_height("ortho")
    expect_type(result, "integer")
  })

})

describe("compute_bsr_summary()", {

  it("returns NULL for NULL input", {
    expect_null(compute_bsr_summary(NULL))
  })

  it("returns NULL for empty vector", {
    expect_null(compute_bsr_summary(numeric(0)))
  })

  it("counts zeros correctly (n_pos=0, n_neg=0)", {
    result <- compute_bsr_summary(c(0, 0, 0, 0), threshold = 3)
    expect_equal(result$n_pos, 0L)
    expect_equal(result$n_neg, 0L)
    expect_equal(result$max_pos, 0)
    expect_equal(result$max_neg, 0)
  })

  it("counts positive values above threshold", {
    result <- compute_bsr_summary(c(1, 2, 4, 5, 6), threshold = 3)
    expect_equal(result$n_pos, 3L)  # 4, 5, 6 are > 3
  })

  it("counts negative values below -threshold", {
    result <- compute_bsr_summary(c(-1, -2, -4, -5, -6), threshold = 3)
    expect_equal(result$n_neg, 3L)  # -4, -5, -6 are < -3
  })

  it("computes max_pos correctly", {
    result <- compute_bsr_summary(c(-5, 1, 4, 7, 2), threshold = 3)
    expect_equal(result$max_pos, 7)
  })

  it("computes max_neg correctly", {
    result <- compute_bsr_summary(c(-5, 1, 4, 7, 2), threshold = 3)
    expect_equal(result$max_neg, -5)
  })

  it("handles mixed positive and negative values", {
    result <- compute_bsr_summary(c(-5, -4, -2, 0, 2, 4, 5), threshold = 3)
    expect_equal(result$n_pos, 2L)  # 4, 5
    expect_equal(result$n_neg, 2L)  # -5, -4
    expect_equal(result$max_pos, 5)
    expect_equal(result$max_neg, -5)
  })

  it("handles NA values with na.rm", {
    result <- compute_bsr_summary(c(NA, 5, NA, -5, NA), threshold = 3)
    expect_equal(result$n_pos, 1L)
    expect_equal(result$n_neg, 1L)
    expect_equal(result$max_pos, 5)
    expect_equal(result$max_neg, -5)
  })

  it("handles all-NA input", {
    result <- compute_bsr_summary(c(NA, NA, NA))
    expect_equal(result$n_pos, 0L)
    expect_equal(result$n_neg, 0L)
    expect_true(is.na(result$max_pos))
    expect_true(is.na(result$max_neg))
  })

  it("respects custom threshold", {
    result <- compute_bsr_summary(c(1, 2, 3, 4, 5), threshold = 2)
    expect_equal(result$n_pos, 3L)  # 3, 4, 5 are > 2
  })

})

describe("compute_lv_stats()", {

  it("returns NA for NULL singular_values", {
    result <- compute_lv_stats(NULL, 1)
    expect_true(is.na(result$var_explained))
    expect_true(is.na(result$cum_var_explained))
  })

  it("returns NA for empty singular_values", {
    result <- compute_lv_stats(numeric(0), 1)
    expect_true(is.na(result$var_explained))
    expect_true(is.na(result$cum_var_explained))
  })

  it("returns NA for NULL lv_index", {
    result <- compute_lv_stats(c(10, 5, 2), NULL)
    expect_true(is.na(result$var_explained))
    expect_true(is.na(result$cum_var_explained))
  })

  it("returns NA for lv_index < 1", {
    result <- compute_lv_stats(c(10, 5, 2), 0)
    expect_true(is.na(result$var_explained))
    expect_true(is.na(result$cum_var_explained))
  })

  it("returns NA for lv_index > length", {
    result <- compute_lv_stats(c(10, 5, 2), 5)
    expect_true(is.na(result$var_explained))
    expect_true(is.na(result$cum_var_explained))
  })

  it("returns 100% for single LV (both var and cumulative)", {
    result <- compute_lv_stats(c(10), 1)
    expect_equal(result$var_explained, 100, tolerance = 1e-10)
    expect_equal(result$cum_var_explained, 100, tolerance = 1e-10)
  })

  it("computes variance correctly for multiple LVs", {
    # s = c(10, 5, 2): s^2 = c(100, 25, 4), sum = 129
    # var_exp = c(77.52%, 19.38%, 3.10%)
    s <- c(10, 5, 2)
    result <- compute_lv_stats(s, 1)
    expected <- (100 / 129) * 100
    expect_equal(result$var_explained, expected, tolerance = 0.01)
  })

  it("computes cumulative variance correctly", {
    s <- c(10, 5, 2)
    # LV2: cum_var = var[1] + var[2]
    result <- compute_lv_stats(s, 2)
    var_exp <- (s^2 / sum(s^2)) * 100
    expected_cum <- sum(var_exp[1:2])
    expect_equal(result$cum_var_explained, expected_cum, tolerance = 0.01)
  })

  it("cumulative variance at last LV is 100%", {
    s <- c(10, 5, 2)
    result <- compute_lv_stats(s, 3)
    expect_equal(result$cum_var_explained, 100, tolerance = 1e-10)
  })

  it("cumulative variance is >= variance at any LV", {
    s <- c(10, 5, 2)
    for (lv in 1:3) {
      result <- compute_lv_stats(s, lv)
      expect_gte(result$cum_var_explained, result$var_explained)
    }
  })

})

describe("build_lv_choices()", {

  it("returns only 'All' for NULL n_lv", {
    result <- build_lv_choices(NULL)
    expect_equal(length(result), 1)
    expect_equal(result[["All"]], "all")
  })

  it("returns only 'All' for n_lv < 1", {
    result <- build_lv_choices(0)
    expect_equal(length(result), 1)
    expect_equal(result[["All"]], "all")
  })

  it("returns 2 choices for n_lv=1", {
    result <- build_lv_choices(1)
    expect_equal(length(result), 2)
    expect_equal(result[["All"]], "all")
    expect_equal(result[["LV1"]], "1")
  })

  it("returns 4 choices for n_lv=3", {
    result <- build_lv_choices(3)
    expect_equal(length(result), 4)
    expect_equal(result[["All"]], "all")
    expect_equal(result[["LV1"]], "1")
    expect_equal(result[["LV2"]], "2")
    expect_equal(result[["LV3"]], "3")
  })

  it("adds asterisk for significant LVs (p < 0.05)", {
    result <- build_lv_choices(3, p_values = c(0.01, 0.10, 0.03))
    expect_true("LV1 *" %in% names(result))
    expect_true("LV2" %in% names(result))  # no asterisk
    expect_true("LV3 *" %in% names(result))
  })

  it("p_values=NULL means no asterisks", {
    result <- build_lv_choices(3, p_values = NULL)
    expect_false(any(grepl("\\*", names(result))))
  })

  it("p_value exactly 0.05 is not significant (< 0.05 required)", {
    result <- build_lv_choices(2, p_values = c(0.05, 0.049))
    expect_true("LV1" %in% names(result))  # no asterisk (p=0.05)
    expect_true("LV2 *" %in% names(result))  # asterisk (p=0.049)
  })

  it("handles NA p-values gracefully", {
    result <- build_lv_choices(3, p_values = c(0.01, NA, 0.03))
    expect_true("LV1 *" %in% names(result))
    expect_true("LV2" %in% names(result))  # NA treated as non-significant
    expect_true("LV3 *" %in% names(result))
  })

  it("handles p_values shorter than n_lv", {
    result <- build_lv_choices(5, p_values = c(0.01, 0.02))
    expect_true("LV1 *" %in% names(result))
    expect_true("LV2 *" %in% names(result))
    expect_true("LV3" %in% names(result))  # no p-value
    expect_true("LV4" %in% names(result))
    expect_true("LV5" %in% names(result))
  })

  it("values are character strings of integers", {
    result <- build_lv_choices(3)
    expect_equal(result[["LV1"]], "1")
    expect_equal(result[["LV2"]], "2")
    expect_equal(result[["LV3"]], "3")
    expect_type(result[["LV1"]], "character")
  })

})
