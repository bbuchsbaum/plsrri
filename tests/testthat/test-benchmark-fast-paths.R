test_that("benchmark_fast_paths returns structured timing rows", {
  res <- benchmark_fast_paths(
    operations = "bootstrap",
    method = 1L,
    num_subj_lst = c(4L, 4L),
    num_cond = 2L,
    n_features = 24L,
    num_boot = 4L,
    reps = 1L,
    workers = 1L,
    seed = 11L
  )

  expect_s3_class(res, "data.frame")
  expect_true(all(c(
    "operation", "method", "scenario", "workers", "elapsed_sec",
    "num_groups", "num_cond", "n_features", "num_boot", "num_perm", "rep"
  ) %in% names(res)))
  expect_true(all(res$operation == "bootstrap"))
  expect_true(all(res$elapsed_sec >= 0))
  expect_true(all(c("baseline", "exact_fast") %in% res$scenario))
})
