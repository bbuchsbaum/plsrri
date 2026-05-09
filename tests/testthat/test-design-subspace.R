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

reference_design_term_stats <- function(fit,
                                        design,
                                        statistic = c("trace", "largest_root"),
                                        weights = rep(1, nrow(fit$task_pls$crossblock))) {
  statistic <- match.arg(statistic)
  cells <- design_cell_table(fit, design = design)
  vars <- intersect(all.vars(design$formula), names(cells))
  factor_cols <- vars[vapply(cells[vars], is.factor, logical(1))]
  contrast_args <- if (length(factor_cols)) {
    setNames(rep(list(stats::contr.sum), length(factor_cols)), factor_cols)
  } else {
    NULL
  }
  model <- stats::model.matrix(design$formula, data = cells, contrasts.arg = contrast_args)
  centered_model <- fit$task_pls$centering_operator %*% model
  terms <- attr(stats::terms(design$formula), "term.labels")
  assign <- attr(model, "assign")
  crossblock_w <- fit$task_pls$crossblock * sqrt(weights)

  rows <- vector("list", length(terms))
  for (term_id in seq_along(terms)) {
    columns <- which(assign == term_id)
    xw <- centered_model[, columns, drop = FALSE] * sqrt(weights)
    qr_x <- qr(xw, tol = 1e-10)
    rank <- as.integer(qr_x$rank)
    statistic_value <- 0
    if (rank > 0L) {
      q <- qr.Q(qr_x)[, seq_len(rank), drop = FALSE]
      coordinates <- crossprod(q, crossblock_w)
      statistic_value <- if (identical(statistic, "trace")) {
        sum(coordinates^2)
      } else {
        d <- svd(coordinates, nu = 0L, nv = 0L)$d
        if (!length(d)) 0 else d[[1L]]^2
      }
    }
    rows[[term_id]] <- data.frame(
      term = terms[[term_id]],
      rank = rank,
      statistic = statistic_value,
      stringsAsFactors = FALSE
    )
  }

  do.call(rbind, rows)
}

make_random_design_subspace_fit <- function(seed) {
  set.seed(4100 + seed)
  num_groups <- sample(2:3, 1L)
  num_cond <- sample(2:4, 1L)
  num_features <- sample(4:7, 1L)
  num_subj_lst <- sample(2:4, num_groups, replace = TRUE)
  datamat_lst <- lapply(num_subj_lst, function(n_subj) {
    matrix(rnorm(n_subj * num_cond * num_features), nrow = n_subj * num_cond, ncol = num_features)
  })

  fit <- pls_analysis(
    datamat_lst = datamat_lst,
    num_subj_lst = num_subj_lst,
    num_cond = num_cond,
    method = 1L,
    meancentering_type = (seed - 1L) %% 4L,
    keep_crossblock = TRUE,
    progress = FALSE
  )
  fit$groups <- paste0("g", seq_len(num_groups))
  fit$conditions <- paste0("c", seq_len(num_cond))
  fit
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

test_that("term statistics match an explicit QR projection oracle", {
  fit <- make_design_subspace_fit(0L)
  design <- pls_design(~ group * task, condition_key = design_subspace_key())
  weights <- design_cell_table(fit, design = design)$cell_n

  for (statistic in c("trace", "largest_root")) {
    observed <- test_design_terms(fit, design = design, statistic = statistic, weights = "subject_count")
    expected <- reference_design_term_stats(fit, design, statistic = statistic, weights = weights)

    expect_equal(observed$term, expected$term)
    expect_equal(observed$rank, expected$rank)
    expect_equal(observed$statistic, expected$statistic, tolerance = 1e-10)
  }
})

test_that("subspace statistics obey scaling and condition-key order metamorphisms", {
  fit <- make_design_subspace_fit(0L)
  design <- pls_design(~ group * task, condition_key = design_subspace_key())
  reordered_design <- pls_design(~ group * task, condition_key = design_subspace_key()[2:1, , drop = FALSE])

  baseline <- test_design_terms(fit, design = design, statistic = "trace")
  reordered <- test_design_terms(fit, design = reordered_design, statistic = "trace")
  expect_equal(reordered$term, baseline$term)
  expect_equal(reordered$rank, baseline$rank)
  expect_equal(reordered$statistic, baseline$statistic, tolerance = 1e-12)

  alpha <- -2.75
  scaled <- fit
  scaled$task_pls$crossblock <- alpha * fit$task_pls$crossblock
  scaled_trace <- test_design_terms(scaled, design = design, statistic = "trace")
  scaled_root <- test_design_terms(scaled, design = design, statistic = "largest_root")
  root <- test_design_terms(fit, design = design, statistic = "largest_root")
  expect_equal(scaled_trace$statistic, alpha^2 * baseline$statistic, tolerance = 1e-10)
  expect_equal(scaled_root$statistic, alpha^2 * root$statistic, tolerance = 1e-10)

  cmp <- compare_designs(fit, design = design, reduced = ~ group, full = ~ group * task)
  scaled_cmp <- compare_designs(scaled, design = design, reduced = ~ group, full = ~ group * task)
  expect_equal(scaled_cmp$statistic, alpha^2 * cmp$statistic, tolerance = 1e-10)
})

test_that("randomized small designs satisfy rank and statistic invariants", {
  for (seed in 1:10) {
    fit <- make_random_design_subspace_fit(seed)
    design <- pls_design(~ group * condition)

    trace <- test_design_terms(fit, design = design, statistic = "trace")
    root <- test_design_terms(fit, design = design, statistic = "largest_root")

    expect_equal(trace$term, root$term)
    expect_equal(trace$rank, root$rank)
    expect_true(all(is.finite(trace$statistic)))
    expect_true(all(is.finite(root$statistic)))
    expect_true(all(trace$statistic >= -1e-10))
    expect_true(all(root$statistic >= -1e-10))
    expect_true(all(root$statistic <= trace$statistic + 1e-8))
    expect_true(all(trace$rank >= 0L & trace$rank <= nrow(fit$task_pls$crossblock)))
    expect_equal(nrow(trace), length(attr(stats::terms(design$formula), "term.labels")))

    cmp <- compare_designs(fit, design = design, reduced = ~ 1, full = ~ group * condition)
    total_energy <- sum(fit$task_pls$crossblock^2)
    expect_true(is.finite(cmp$statistic))
    expect_true(cmp$statistic >= -1e-10)
    expect_true(cmp$statistic <= total_energy + 1e-8)
    expect_true(cmp$rank <= nrow(fit$task_pls$crossblock))
  }
})

test_that("degenerate and contaminated crossblocks are handled explicitly", {
  fit <- make_design_subspace_fit(0L)
  design <- pls_design(~ group * task, condition_key = design_subspace_key())

  zero <- fit
  zero$task_pls$crossblock[] <- 0
  zero_terms <- test_design_terms(zero, design = design)
  zero_cmp <- compare_designs(zero, design = design, reduced = ~ group, full = ~ group * task)
  expect_equal(zero_terms$statistic, rep(0, nrow(zero_terms)))
  expect_equal(zero_cmp$statistic, 0)

  contaminated <- fit
  contaminated$task_pls$crossblock[1L, 1L] <- Inf
  expect_error(test_design_terms(contaminated, design = design), "non-finite")
  expect_error(
    compare_designs(contaminated, design = design, reduced = ~ group, full = ~ group * task),
    "non-finite"
  )
})
