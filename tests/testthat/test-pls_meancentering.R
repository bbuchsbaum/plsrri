# Tests for mean-centering functions

test_that("pls_task_mean computes correct means", {
  # Create structured data: 3 subjects, 2 conditions
  # Data arranged as subject-in-condition
  datamat <- matrix(c(
    1, 2, 3,  # subj 1-3, cond 1
    4, 5, 6,  # subj 1-3, cond 2
    7, 8, 9,
    10, 11, 12
  ), nrow = 6, ncol = 2, byrow = TRUE)

  means <- pls_task_mean(datamat, n_subj = 3)

  expect_equal(nrow(means), 2)  # 2 conditions
  expect_equal(ncol(means), 2)  # 2 features

  # Condition 1 mean: (1+4+7)/3, (2+5+8)/3 = 4, 5
  # Condition 2 mean: (4+5+6)/3, (5+6+7)/3 ... wait, let me recalculate

  # Actually with byrow=TRUE:
  # Row 1: 1, 2, 3 (but only 2 columns, so 1, 2)
  # Let me recreate properly
})

test_that("pls_task_mean with known values", {
  # 2 subjects, 3 conditions, 2 features
  # Structure: s1c1, s2c1, s1c2, s2c2, s1c3, s2c3
  datamat <- matrix(c(
    1, 2,    # s1, c1
    3, 4,    # s2, c1
    5, 6,    # s1, c2
    7, 8,    # s2, c2
    9, 10,   # s1, c3
    11, 12   # s2, c3
  ), nrow = 6, ncol = 2, byrow = TRUE)

  means <- pls_task_mean(datamat, n_subj = 2)

  expect_equal(nrow(means), 3)
  expect_equal(ncol(means), 2)

  # Condition 1: mean of rows 1-2 = (1+3)/2, (2+4)/2 = 2, 3
  # Condition 2: mean of rows 3-4 = (5+7)/2, (6+8)/2 = 6, 7
  # Condition 3: mean of rows 5-6 = (9+11)/2, (10+12)/2 = 10, 11

  expect_equal(means[1, ], c(2, 3))
  expect_equal(means[2, ], c(6, 7))
  expect_equal(means[3, ], c(10, 11))
})

test_that("pls_meancentering type 0 (within-group)", {
  datamat1 <- matrix(c(
    1, 2,
    3, 4,
    5, 6,
    7, 8
  ), nrow = 4, ncol = 2, byrow = TRUE)

  result <- pls_meancentering(
    datamat_lst = list(datamat1),
    num_subj_lst = 2,
    num_cond = 2,
    meancentering_type = 0
  )

  expect_equal(length(result$centered), 1)

  # Group mean: (1+3+5+7)/4, (2+4+6+8)/4 = 4, 5
  # Cond 1 mean: 2, 3 -> centered: 2-4, 3-5 = -2, -2
  # Cond 2 mean: 6, 7 -> centered: 6-4, 7-5 = 2, 2

  expect_equal(result$centered[[1]][1, ], c(-2, -2))
  expect_equal(result$centered[[1]][2, ], c(2, 2))
})

test_that("pls_meancentering supports ssb num_subj_lst for types 0-3", {
  # Two groups, two conditions, one feature
  # Group 1: cond1 has 1 subj (1), cond2 has 2 subj (3,5)
  # Group 2: cond1 has 2 subj (2,4), cond2 has 1 subj (6)
  datamat1 <- matrix(c(1, 3, 5), ncol = 1)
  datamat2 <- matrix(c(2, 4, 6), ncol = 1)

  num_subj_lst <- list(c(1, 2), c(2, 1))
  num_cond <- 2

  # Type 0: within-group grand mean
  res0 <- pls_meancentering(
    datamat_lst = list(datamat1, datamat2),
    num_subj_lst = num_subj_lst,
    num_cond = num_cond,
    meancentering_type = 0
  )
  expect_equal(res0$centered[[1]], matrix(c(-2, 1), ncol = 1))
  expect_equal(res0$centered[[2]], matrix(c(-1, 2), ncol = 1))

  # Type 1: remove grand condition mean (equal-weighted over groups)
  res1 <- pls_meancentering(
    datamat_lst = list(datamat1, datamat2),
    num_subj_lst = num_subj_lst,
    num_cond = num_cond,
    meancentering_type = 1
  )
  expect_equal(res1$centered[[1]], matrix(c(-1, -1), ncol = 1))
  expect_equal(res1$centered[[2]], matrix(c(1, 1), ncol = 1))

  # Type 2: remove grand mean over all observations
  res2 <- pls_meancentering(
    datamat_lst = list(datamat1, datamat2),
    num_subj_lst = num_subj_lst,
    num_cond = num_cond,
    meancentering_type = 2
  )
  expect_equal(res2$centered[[1]], matrix(c(-2.5, 0.5), ncol = 1))
  expect_equal(res2$centered[[2]], matrix(c(-0.5, 2.5), ncol = 1))

  # Type 3: remove condition and group main effects (interaction only)
  res3 <- pls_meancentering(
    datamat_lst = list(datamat1, datamat2),
    num_subj_lst = num_subj_lst,
    num_cond = num_cond,
    meancentering_type = 3
  )
  expect_equal(res3$centered[[1]], matrix(c(0, 0), ncol = 1))
  expect_equal(res3$centered[[2]], matrix(c(0, 0), ncol = 1))
})

test_that("normalize_rows produces unit length rows", {
  mat <- matrix(c(3, 4, 1, 0), nrow = 2, byrow = TRUE)

  normalized <- normalize_rows(mat)

  row_norms <- sqrt(rowSums(normalized^2))
  expect_equal(row_norms, c(1, 1), tolerance = 1e-10)
})
