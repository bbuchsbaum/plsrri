# Tests for R/plot_scores.R

# Helper: build a minimal pls_result via pls_analysis
make_pls_result <- function() {
  set.seed(42)
  n_subj <- 8
  n_cond <- 3
  n_feat <- 30
  datamat <- matrix(rnorm(n_subj * n_cond * n_feat), n_subj * n_cond, n_feat)
  pls_analysis(
    datamat_lst   = list(datamat),
    num_subj_lst  = n_subj,
    num_cond      = n_cond,
    method        = 1L,
    progress      = FALSE
  )
}

test_that("plot_scores returns a ggplot for plot_type = 'bar'", {
  skip_if_not_installed("ggplot2")
  res <- make_pls_result()
  p <- plot_scores(res, lv = 1, type = "design", plot_type = "bar")
  expect_s3_class(p, "gg")
})

test_that("plot_scores returns a ggplot for plot_type = 'violin'", {
  skip_if_not_installed("ggplot2")
  res <- make_pls_result()
  p <- plot_scores(res, lv = 1, type = "design", plot_type = "violin")
  expect_s3_class(p, "gg")
})

test_that("plot_scores returns a ggplot for plot_type = 'box'", {
  skip_if_not_installed("ggplot2")
  res <- make_pls_result()
  p <- plot_scores(res, lv = 1, type = "design", plot_type = "box")
  expect_s3_class(p, "gg")
})

test_that("plot_scores returns a ggplot for plot_type = 'scatter'", {
  skip_if_not_installed("ggplot2")
  res <- make_pls_result()
  p <- plot_scores(res, lv = 1, type = "design", plot_type = "scatter")
  expect_s3_class(p, "gg")
})

test_that("plot_scores returns a ggplot for brain scores", {
  skip_if_not_installed("ggplot2")
  res <- make_pls_result()
  p <- plot_scores(res, lv = 1, type = "brain", plot_type = "bar")
  expect_s3_class(p, "gg")
})

test_that("plot_scores accepts a custom title", {
  skip_if_not_installed("ggplot2")
  res <- make_pls_result()
  p <- plot_scores(res, lv = 1, type = "design", plot_type = "bar", title = "My Title")
  expect_s3_class(p, "gg")
  # title should appear in the plot labels
  expect_equal(p$labels$title, "My Title")
})

test_that("plot_scores errors on invalid type", {
  skip_if_not_installed("ggplot2")
  res <- make_pls_result()
  expect_error(plot_scores(res, lv = 1, type = "bogus"))
})

test_that("plot_scores errors on invalid plot_type", {
  skip_if_not_installed("ggplot2")
  res <- make_pls_result()
  expect_error(plot_scores(res, lv = 1, plot_type = "bogus"))
})
