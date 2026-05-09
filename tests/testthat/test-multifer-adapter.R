make_multifer_task_payload <- function(num_subj_lst = c(2L, 3L),
                                       num_cond = 2L,
                                       n_features = 5L) {
  sizes <- .plsrri_group_row_counts(num_subj_lst, num_cond)
  datamat_lst <- lapply(seq_along(sizes), function(g) {
    matrix(
      seq_len(sizes[[g]] * n_features) + 100 * g,
      nrow = sizes[[g]],
      ncol = n_features
    )
  })

  .plsrri_multifer_sync_cross_payload(list(
    datamat_lst = datamat_lst,
    num_subj_lst = num_subj_lst,
    num_cond = num_cond,
    method = 1L,
    stacked_behavdata = NULL,
    stacked_designdata = NULL,
    bscan = NULL,
    meancentering_type = 0L,
    cormode = 0L,
    boot_type = "strat",
    is_struct = FALSE,
    clim = 95
  ))
}

expect_valid_task_cross_payload <- function(payload, expected_counts) {
  expect_silent(.plsrri_multifer_assert_task_payload(payload))
  expect_equal(payload$relation, "covariance")
  expect_equal(payload$X, stack_datamats(payload$datamat_lst))
  expect_equal(nrow(payload$Y), nrow(payload$X))
  expect_equal(ncol(payload$Y), length(expected_counts))
  expect_equal(as.integer(rowSums(payload$Y)), rep(1L, nrow(payload$Y)))
  expect_equal(as.integer(colSums(payload$Y)), as.integer(expected_counts))
  expect_true(all(is.finite(payload$X)))
  expect_true(all(is.finite(payload$Y)))
}

test_that("configure records the multifer inference engine", {
  spec <- pls_spec() |>
    configure(inference = "multifer")

  expect_equal(spec$inference, "multifer")
})

test_that("task cross payload contract holds for balanced and ssb designs", {
  balanced <- make_multifer_task_payload(c(2L, 3L), num_cond = 2L)
  expect_valid_task_cross_payload(
    balanced,
    expected_counts = c(2L, 2L, 3L, 3L)
  )

  ssb_counts <- list(c(1L, 3L), c(2L, 1L))
  ssb <- make_multifer_task_payload(ssb_counts, num_cond = 2L)
  expect_valid_task_cross_payload(
    ssb,
    expected_counts = unlist(ssb_counts, use.names = FALSE)
  )
})

test_that("task design payload properties hold over randomized valid structures", {
  set.seed(2402)
  for (i in seq_len(12L)) {
    num_groups <- sample(1:3, 1L)
    num_cond <- sample(1:4, 1L)
    n_features <- sample(2:7, 1L)
    if (i %% 2L == 0L) {
      counts <- sample(1:4, num_groups, replace = TRUE)
      expected_counts <- rep(counts, each = num_cond)
    } else {
      counts <- replicate(
        num_groups,
        sample(1:4, num_cond, replace = TRUE),
        simplify = FALSE
      )
      expected_counts <- unlist(counts, use.names = FALSE)
    }

    payload <- make_multifer_task_payload(counts, num_cond, n_features)
    expect_valid_task_cross_payload(payload, expected_counts)
  }
})

test_that("resampling payload synchronization preserves the intended cross blocks", {
  payload <- make_multifer_task_payload(c(3L, 2L), num_cond = 2L)
  n <- nrow(payload$X)
  reverse_order <- rev(seq_len(n))

  reversed <- .plsrri_multifer_reorder_payload(payload, reverse_order)
  expect_valid_task_cross_payload(
    reversed,
    expected_counts = c(3L, 3L, 2L, 2L)
  )
  expect_equal(reversed$X, payload$X[reverse_order, , drop = FALSE])
  expect_equal(reversed$Y, payload$Y)

  identity <- .plsrri_multifer_reorder_payload(payload, seq_len(n))
  expect_equal(identity$X, payload$X)
  expect_equal(identity$Y, payload$Y)
})

test_that("adapter hook statistics agree with the plsrri fit oracle", {
  testthat::skip_if_not_installed("multifer")
  payload <- make_multifer_task_payload(c(4L, 3L), num_cond = 3L)
  adapter <- adapter_plsrri_task()
  fit <- .plsrri_multifer_fit_task(payload)

  expect_equal(adapter$roots(fit), fit$s^2, tolerance = 1e-10)
  expect_equal(
    adapter$component_stat(fit, payload, 1L),
    fit$s[[1L]]^2,
    tolerance = 1e-10
  )
  expect_equal(adapter$loadings(fit, "X"), fit$u)
  expect_equal(adapter$loadings(fit, "Y"), fit$v)
  expect_equal(
    adapter$project_scores(fit, payload, "X"),
    stack_datamats(payload$datamat_lst) %*% fit$u,
    tolerance = 1e-10
  )
})

test_that("McIntosh null and bootstrap actions keep payloads internally synchronized", {
  testthat::skip_if_not_installed("multifer")
  payload <- make_multifer_task_payload(c(4L, 3L), num_cond = 2L)
  adapter <- adapter_plsrri_task()
  fit <- .plsrri_multifer_fit_task(payload)

  set.seed(2403)
  null_payload <- adapter$null_action(fit, payload)
  expect_valid_task_cross_payload(
    null_payload,
    expected_counts = c(4L, 4L, 3L, 3L)
  )
  expect_equal(ncol(null_payload$permsamp), 1L)
  expect_setequal(as.integer(null_payload$permsamp[, 1L]), seq_len(nrow(payload$X)))
  expect_equal(
    null_payload$X,
    payload$X[as.integer(null_payload$permsamp[, 1L]), , drop = FALSE]
  )
  expect_equal(null_payload$Y, payload$Y)

  set.seed(2404)
  boot <- adapter$bootstrap_action(fit, payload, design = NULL, replicate = 7L)
  expect_s3_class(boot$fit, "pls_task")
  expect_length(boot$resample_indices, nrow(payload$X))
  expect_equal(boot$info$kind, "pls_boot_order")
  expect_equal(boot$info$replicate, 7L)
})

test_that("payload validation rejects malformed and contaminated inputs", {
  payload <- make_multifer_task_payload(c(2L, 2L), num_cond = 2L)

  bad_y <- payload
  bad_y$Y <- bad_y$Y[-1L, , drop = FALSE]
  expect_error(
    .plsrri_multifer_assert_task_payload(bad_y),
    "same number of rows"
  )

  bad_method <- payload
  bad_method$method <- 3L
  expect_error(
    .plsrri_multifer_assert_task_payload(bad_method),
    "method 1"
  )

  bad_data <- payload
  bad_data$datamat_lst[[1L]][1L, 1L] <- Inf
  bad_data <- .plsrri_multifer_sync_cross_payload(bad_data)
  expect_error(
    .plsrri_multifer_assert_task_payload(bad_data),
    "finite"
  )
})

test_that("plsrri task adapter declares true cross covariance geometry", {
  testthat::skip_if_not_installed("multifer")
  testthat::skip_if_not(
    .plsrri_multifer_has_cross_adapter_execution(),
    "multifer lacks cross adapter component execution"
  )

  adapter <- adapter_plsrri_task()

  expect_equal(adapter$shape_kinds, "cross")
  expect_true(any(
    adapter$capabilities$geometry == "cross" &
      adapter$capabilities$relation == "covariance"
  ))
  expect_equal(adapter$component_execution, "adapter")
})

test_that("method 1 task PLS can run through the optional multifer bridge", {
  testthat::skip_if_not_installed("multifer")
  testthat::skip_if_not(
    .plsrri_multifer_has_cross_adapter_execution(),
    "multifer lacks cross adapter component execution"
  )

  set.seed(2401)
  n_subj <- 8L
  n_cond <- 3L
  n_features <- 12L
  datamat <- matrix(rnorm(n_subj * n_cond * n_features), n_subj * n_cond, n_features)

  result <- pls_spec() |>
    add_subjects(list(datamat), groups = n_subj) |>
    add_conditions(n_cond) |>
    configure(method = "task", inference = "multifer", nperm = 9L, nboot = 4L) |>
    run(progress = FALSE)

  expect_s3_class(result, "pls_task")
  expect_equal(result$inference_engine, "multifer")
  expect_equal(result$multifer_adapter, "plsrri_task")
  expect_s3_class(result$multifer_result, "infer_result")
  expect_match(
    result$multifer_result$provenance$capabilities,
    "^cross/covariance:"
  )

  expect_s3_class(result$perm_result, "pls_perm_result")
  expect_equal(length(result$perm_result$sprob), length(result$s))
  expect_true(all(result$perm_result$sprob >= 0 & result$perm_result$sprob <= 1))

  expect_s3_class(result$boot_result, "pls_boot_result")
  expect_equal(dim(result$boot_result$compare_u), dim(result$u))
  expect_true(all(is.finite(result$boot_result$compare_u)))
})
