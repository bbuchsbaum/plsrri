test_that("configure records the multifer inference engine", {
  spec <- pls_spec() |>
    configure(inference = "multifer")

  expect_equal(spec$inference, "multifer")
})

test_that("method 1 task PLS can run through the optional multifer bridge", {
  testthat::skip_if_not_installed("multifer")

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

  expect_s3_class(result$perm_result, "pls_perm_result")
  expect_equal(length(result$perm_result$sprob), length(result$s))
  expect_true(all(result$perm_result$sprob >= 0 & result$perm_result$sprob <= 1))

  expect_s3_class(result$boot_result, "pls_boot_result")
  expect_equal(dim(result$boot_result$compare_u), dim(result$u))
  expect_true(all(is.finite(result$boot_result$compare_u)))
})
