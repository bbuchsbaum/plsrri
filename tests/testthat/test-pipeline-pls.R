# Tests for R/pipeline_pls.R

test_that(".pipeline_method_name returns valid method names", {
  expect_equal(plsrri:::.pipeline_method_name("task"), "task")
  expect_equal(plsrri:::.pipeline_method_name("Task"), "task")
  expect_equal(plsrri:::.pipeline_method_name("TASK"), "task")
  expect_equal(plsrri:::.pipeline_method_name("behavior"), "behavior")
  expect_equal(plsrri:::.pipeline_method_name("behavior_nonrotated"), "behavior_nonrotated")
  expect_equal(plsrri:::.pipeline_method_name("multiblock"), "multiblock")
  expect_equal(plsrri:::.pipeline_method_name("multiblock_nonrotated"), "multiblock_nonrotated")
  expect_equal(plsrri:::.pipeline_method_name("task_nonrotated"), "task_nonrotated")
})

test_that(".pipeline_method_name errors on invalid method", {
  expect_error(
    plsrri:::.pipeline_method_name("invalid_method"),
    "Unsupported pls.method"
  )
  expect_error(
    plsrri:::.pipeline_method_name(""),
    "Unsupported pls.method"
  )
})

test_that(".pipeline_pls_summary returns a data.frame with expected columns", {
  set.seed(42)
  # Minimal pls_result-like list with required fields
  mock_result <- list(
    s = c(5.2, 3.1, 1.4),
    perm_result = NULL
  )
  out <- plsrri:::.pipeline_pls_summary(mock_result)
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 3L)
  expect_true(all(c("lv", "singular_value", "variance_explained") %in% names(out)))
  expect_equal(out$lv, 1:3)
  expect_equal(out$singular_value, mock_result$s)
  expect_true(all(out$variance_explained >= 0))
  expect_true(abs(sum(out$variance_explained) - 1) < 1e-10)
})

test_that(".pipeline_pls_summary includes p_value when perm_result present", {
  set.seed(42)
  mock_result <- list(
    s = c(4.0, 2.0),
    perm_result = list(sprob = c(0.01, 0.08))
  )
  out <- plsrri:::.pipeline_pls_summary(mock_result)
  expect_true("p_value" %in% names(out))
  expect_equal(out$p_value, c(0.01, 0.08))
})

test_that(".pipeline_pls_summary omits p_value when perm_result is NULL", {
  mock_result <- list(s = c(3.0, 1.5), perm_result = NULL)
  out <- plsrri:::.pipeline_pls_summary(mock_result)
  expect_false("p_value" %in% names(out))
})

test_that(".pipeline_parse_basis_manifest returns voxel_map when no condition/basis_index cols", {
  set.seed(42)
  map_rows <- data.frame(
    group = c("g1", "g1"),
    subject = c("sub-01", "sub-02"),
    condition = c("cond_a", "cond_b"),
    file = c("a.nii", "b.nii"),
    type = c("beta", "beta"),
    statistic = c("t", "t"),
    label = c("cond_a", "cond_b"),
    task = c("task1", "task1"),
    mask_file = c(NA_character_, NA_character_),
    stringsAsFactors = FALSE
  )
  # No basis_index column -> plain voxel_map path
  input_spec <- list(basis_pattern = NULL)
  result <- plsrri:::.pipeline_parse_basis_manifest(map_rows, input_spec)
  expect_equal(result$kind, "voxel_map")
  expect_s3_class(result$manifest, "data.frame")
  expect_true("condition" %in% names(result$manifest))
})

test_that(".pipeline_parse_basis_manifest uses condition/basis_index when present", {
  set.seed(42)
  map_rows <- data.frame(
    group = c("g1", "g1", "g1"),
    subject = c("sub-01", "sub-01", "sub-01"),
    condition = c("cond_a", "cond_a", "cond_a"),
    basis_index = c(1L, 2L, 3L),
    file = c("a.nii", "b.nii", "c.nii"),
    type = c("beta", "beta", "beta"),
    statistic = c("t", "t", "t"),
    label = c("cond_a_b1", "cond_a_b2", "cond_a_b3"),
    task = c("task1", "task1", "task1"),
    mask_file = c(NA_character_, NA_character_, NA_character_),
    stringsAsFactors = FALSE
  )
  input_spec <- list(basis_order = NULL)
  result <- plsrri:::.pipeline_parse_basis_manifest(map_rows, input_spec)
  expect_equal(result$kind, "voxel_lag")
  expect_true("lag" %in% names(result$manifest))
  expect_equal(result$manifest$lag, c(0L, 1L, 2L))
})

test_that(".pipeline_parse_basis_manifest parses labels with basis_pattern", {
  set.seed(42)
  map_rows <- data.frame(
    group = c("g1", "g1", "g1", "g1"),
    subject = c("sub-01", "sub-01", "sub-02", "sub-02"),
    condition = NA_character_,
    file = c("a.nii", "b.nii", "c.nii", "d.nii"),
    type = rep("beta", 4),
    statistic = rep("t", 4),
    label = c("face_early", "face_late", "house_early", "house_late"),
    task = rep("task1", 4),
    mask_file = rep(NA_character_, 4),
    stringsAsFactors = FALSE
  )
  # basis_index column absent; use pattern instead
  input_spec <- list(
    basis_pattern = "^([a-z]+)_([a-z]+)$",
    condition_group = 1L,
    basis_group = 2L,
    basis_order = c("early", "late")
  )
  result <- plsrri:::.pipeline_parse_basis_manifest(map_rows, input_spec)
  expect_equal(result$kind, "voxel_lag")
  expect_equal(result$manifest$condition, c("face", "face", "house", "house"))
  expect_equal(result$manifest$lag, c(0L, 1L, 0L, 1L))
  expect_equal(result$basis_order, c("early", "late"))
})

test_that(".pipeline_align_behavior aligns rows and returns a matrix", {
  set.seed(42)
  tmp_csv <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp_csv))

  beh_df <- data.frame(
    subject   = c("sub-01", "sub-02", "sub-01", "sub-02"),
    condition = c("face", "face", "house", "house"),
    score_a   = c(1.1, 2.2, 3.3, 4.4),
    score_b   = c(0.1, 0.2, 0.3, 0.4),
    stringsAsFactors = FALSE
  )
  utils::write.csv(beh_df, tmp_csv, row.names = FALSE)

  brain_manifest <- data.frame(
    subject   = c("sub-01", "sub-02", "sub-01", "sub-02"),
    condition = c("house", "face", "face", "house"),
    stringsAsFactors = FALSE
  )

  behavior_spec <- list(
    file = tmp_csv,
    subject_col = "subject",
    condition_col = "condition",
    measures = c("score_a", "score_b")
  )

  result <- plsrri:::.pipeline_align_behavior(brain_manifest, behavior_spec)
  expect_true(is.matrix(result))
  expect_equal(nrow(result), nrow(brain_manifest))
  expect_equal(ncol(result), 2L)
  expect_equal(colnames(result), c("score_a", "score_b"))
  # sub-01/house is row 3 in beh_df -> score_a = 3.3
  expect_equal(result[1, "score_a"], 3.3)
  # sub-02/face is row 2 in beh_df -> score_a = 2.2
  expect_equal(result[2, "score_a"], 2.2)
})

test_that(".pipeline_align_behavior errors on missing behavior rows", {
  tmp_csv <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp_csv))
  beh_df <- data.frame(
    subject = "sub-01", condition = "face", score = 1.0,
    stringsAsFactors = FALSE
  )
  utils::write.csv(beh_df, tmp_csv, row.names = FALSE)

  brain_manifest <- data.frame(
    subject = c("sub-01", "sub-99"),
    condition = c("face", "face"),
    stringsAsFactors = FALSE
  )
  behavior_spec <- list(file = tmp_csv)

  expect_error(
    plsrri:::.pipeline_align_behavior(brain_manifest, behavior_spec),
    "missing rows"
  )
})

test_that(".pipeline_align_behavior errors on duplicate behavior keys", {
  tmp_csv <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp_csv))
  beh_df <- data.frame(
    subject = c("sub-01", "sub-01"),
    condition = c("face", "face"),
    score = c(1.0, 2.0),
    stringsAsFactors = FALSE
  )
  utils::write.csv(beh_df, tmp_csv, row.names = FALSE)

  brain_manifest <- data.frame(
    subject = "sub-01", condition = "face",
    stringsAsFactors = FALSE
  )
  behavior_spec <- list(file = tmp_csv)

  expect_error(
    plsrri:::.pipeline_align_behavior(brain_manifest, behavior_spec),
    "duplicate"
  )
})
