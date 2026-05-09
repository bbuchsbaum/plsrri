make_design_subspace_fit <- function(meancentering_type = 0L) {
  set.seed(2025 + as.integer(meancentering_type))
  datamat1 <- matrix(rnorm(4 * 6), nrow = 4, ncol = 6)
  datamat2 <- matrix(rnorm(4 * 6), nrow = 4, ncol = 6)

  fit <- pls_analysis(
    datamat_lst = list(datamat1, datamat2),
    num_subj_lst = c(2L, 2L),
    num_cond = 2L,
    method = 1L,
    meancentering_type = meancentering_type,
    keep_crossblock = TRUE,
    progress = FALSE
  )
  fit$groups <- c("control", "sdam")
  fit$conditions <- c("recog", "nback")
  fit
}

design_subspace_key <- function() {
  data.frame(
    condition = c("recog", "nback"),
    task = c("recog", "nback"),
    stringsAsFactors = FALSE
  )
}

test_that("pls_analysis can retain Task PLS crossblock metadata", {
  fit <- make_design_subspace_fit()

  expect_true(is.list(fit$task_pls))
  expect_equal(dim(fit$task_pls$crossblock), c(4L, 6L))
  expect_equal(dim(fit$task_pls$centering_operator), c(4L, 4L))
  expect_equal(fit$task_pls$cell_info$group_index, c(1L, 1L, 2L, 2L))
  expect_equal(fit$task_pls$cell_info$condition_index, c(1L, 2L, 1L, 2L))
  expect_equal(fit$task_pls$cell_info$cell_n, rep(2L, 4L))

  roundtrip <- mva_result_to_pls_result(pls_result_to_mva_result(fit))
  expect_equal(roundtrip$task_pls$crossblock, fit$task_pls$crossblock)
})

test_that("design cell table rehydrates condition metadata in PLS row order", {
  fit <- make_design_subspace_fit()
  design <- pls_design(~ group * task, condition_key = design_subspace_key())

  cells <- design_cell_table(fit, design = design)

  expect_equal(nrow(cells), 4L)
  expect_equal(as.character(cells$group), c("control", "control", "sdam", "sdam"))
  expect_equal(as.character(cells$condition), c("recog", "nback", "recog", "nback"))
  expect_equal(as.character(cells$task), c("recog", "nback", "recog", "nback"))
})

test_that("term ranks are computed in the centered Task PLS row space", {
  design <- pls_design(~ group * task, condition_key = design_subspace_key())

  type0 <- test_design_terms(make_design_subspace_fit(0L), design = design)
  expect_equal(type0$rank[match("group", type0$term)], 0L)
  expect_equal(type0$rank[match("task", type0$term)], 1L)
  expect_equal(type0$rank[match("group:task", type0$term)], 1L)

  type3 <- test_design_terms(make_design_subspace_fit(3L), design = design)
  expect_equal(type3$rank[match("group", type3$term)], 0L)
  expect_equal(type3$rank[match("task", type3$term)], 0L)
  expect_equal(type3$rank[match("group:task", type3$term)], 1L)
})

test_that("nested comparisons use residualized full design rank", {
  fit <- make_design_subspace_fit(0L)
  design <- pls_design(~ group * task, condition_key = design_subspace_key())

  cmp <- compare_designs(fit, design = design, reduced = ~ group, full = ~ group * task)
  expect_equal(cmp$added_terms, "task + group:task")
  expect_equal(cmp$rank, 2L)
  expect_true(is.finite(cmp$statistic))

  full_cmp <- compare_designs(fit, design = design, reduced = ~ 1, full = ~ group * task)
  expect_equal(full_cmp$rank, 2L)
  expect_equal(full_cmp$statistic, sum(fit$task_pls$crossblock^2), tolerance = 1e-10)

  same_cmp <- compare_designs(fit, design = design, reduced = ~ task * group, full = ~ group * task)
  expect_equal(same_cmp$rank, 0L)
  expect_equal(same_cmp$added_terms, "none")

  expect_error(
    compare_designs(fit, design = design, reduced = ~ group * task, full = ~ group),
    "reduced design is not nested"
  )
})

test_that("largest-root and LV decompositions are available as observed summaries", {
  fit <- make_design_subspace_fit(0L)
  design <- pls_design(~ group * task, condition_key = design_subspace_key())

  trace <- test_design_terms(fit, design = design, statistic = "trace")
  root <- test_design_terms(fit, design = design, statistic = "largest_root")
  row_weight_trace <- test_design_terms(fit, design = design, statistic = "trace", weights = "row_weights")
  expect_true(all(root$statistic <= trace$statistic + 1e-10))
  expect_equal(row_weight_trace$statistic, trace$statistic)

  decomp <- decompose_design_terms(fit, lv = 1L, design = design)
  expect_equal(decomp$term, trace$term)
  expect_true(all(is.finite(decomp$energy)))
  expect_true(all(decomp$fraction >= 0 & decomp$fraction <= 1 + 1e-10))
})

test_that("subspace tests require a stored crossblock and do not fake permutation p-values", {
  fit <- make_design_subspace_fit(0L)
  design <- pls_design(~ group * task, condition_key = design_subspace_key())
  fit$task_pls <- NULL

  expect_true(is.list(design_projectors(fit, design = design)))
  expect_error(test_design_terms(fit, design = design), "No Task PLS crossblock")
  expect_error(
    test_design_terms(make_design_subspace_fit(0L), design = design, nperm = 2L),
    "Permutation design-subspace inference is not implemented"
  )
})

test_that("SSB centering operators match pls_get_covcor weighting", {
  set.seed(3030)
  counts <- list(c(1L, 2L, 3L), c(2L, 1L, 2L))
  datamat_lst <- lapply(counts, function(n_by_cond) {
    matrix(rnorm(sum(n_by_cond) * 5), nrow = sum(n_by_cond), ncol = 5)
  })
  raw_cell_means <- do.call(rbind, Map(pls_task_mean_ssb, datamat_lst, counts))

  for (mct in c(0L, 2L)) {
    covcor <- pls_get_covcor(
      method = 1L,
      stacked_datamat = do.call(rbind, datamat_lst),
      num_groups = 2L,
      num_subj_lst = counts,
      num_cond = 3L,
      meancentering_type = mct
    )
    op <- plsrri:::.task_pls_centering_operator(
      num_subj_lst = counts,
      num_cond = 3L,
      meancentering_type = mct,
      method = 1L
    )
    expect_equal(op %*% raw_cell_means, covcor$datamatsvd, tolerance = 1e-12)
  }
})
