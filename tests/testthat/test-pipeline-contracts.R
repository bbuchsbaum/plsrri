# Contract-oriented tests for staged plscli helpers.

test_that("shared observation ordering preserves balanced subject-major layout", {
  obs <- data.frame(
    group = c("g1", "g1", "g1", "g1"),
    subject = c("s2", "s1", "s2", "s1"),
    condition = c("c1", "c1", "c2", "c2"),
    stringsAsFactors = FALSE
  )

  ordered <- plsrri:::.order_manifest_observations(obs, groups = "g1", conditions = c("c1", "c2"))
  out <- ordered$obs_by_group[[1]]

  expect_equal(as.character(out$condition), c("c1", "c1", "c2", "c2"))
  expect_equal(as.character(out$subject), c("s1", "s2", "s1", "s2"))
  expect_equal(ordered$num_subj_lst, 2L)
})

test_that("pipeline_build_pls_manifest prefers explicit basis metadata over regex parsing", {
  map_rows <- data.frame(
    work_id = rep("w0001", 4),
    subject = rep(c("01", "02"), each = 2),
    group = "all",
    task = "toy",
    type = "estimates",
    label = c("face_anything", "face_anything", "scene_anything", "scene_anything"),
    statistic = "estimate",
    file = paste0(tempdir(), "/map", seq_len(4), ".nii.gz"),
    mask_file = "mask.nii.gz",
    condition = c("face", "face", "scene", "scene"),
    basis_label = c("t0", "t1", "t0", "t1"),
    basis_index = c(1L, 2L, 1L, 2L),
    stringsAsFactors = FALSE
  )

  info <- plsrri:::.pipeline_build_pls_manifest(map_rows, list(
    type = "estimates",
    statistic = "estimate",
    basis_pattern = "^no_match$",
    condition_group = 1L,
    basis_group = 2L
  ))

  expect_equal(info$kind, "voxel_lag")
  expect_equal(sort(unique(info$manifest$condition)), c("face", "scene"))
  expect_equal(sort(unique(info$manifest$lag)), c(0L, 1L))
})

test_that("pipeline_select_firstlevel_outputs enforces requested labels", {
  map_rows <- data.frame(
    work_id = "w0001",
    subject = c("01", "01"),
    group = "all",
    task = "toy",
    type = "estimates",
    label = c("face", "scene"),
    statistic = "estimate",
    file = c("face.nii.gz", "scene.nii.gz"),
    mask_file = "mask.nii.gz",
    stringsAsFactors = FALSE
  )

  selected <- plsrri:::.pipeline_select_firstlevel_outputs(map_rows, list(
    type = "estimates",
    statistic = "estimate",
    labels = "scene",
    label_map = list(scene = "places")
  ))

  expect_equal(selected$label, "scene")
  expect_equal(selected$condition, "places")
})

test_that("pipeline_build_analysis_plan resolves a single shared mask", {
  map_rows <- data.frame(
    work_id = c("w0001", "w0002"),
    subject = c("01", "02"),
    group = "all",
    task = "toy",
    type = "estimates",
    label = c("face", "face"),
    statistic = "estimate",
    file = c("face01.nii.gz", "face02.nii.gz"),
    mask_file = c("mask.nii.gz", "mask.nii.gz"),
    stringsAsFactors = FALSE
  )

  plan <- plsrri:::.pipeline_build_analysis_plan(
    map_rows,
    list(type = "estimates", statistic = "estimate"),
    analysis = "pls"
  )

  expect_equal(plan$analysis, "pls")
  expect_equal(plan$mask_file, "mask.nii.gz")
  expect_true(is.data.frame(plan$manifest))
})

test_that("pipeline_resolve_analysis_mask rejects mixed masks", {
  manifest <- data.frame(
    group = c("all", "all"),
    subject = c("01", "02"),
    condition = c("face", "face"),
    file = c("face01.nii.gz", "face02.nii.gz"),
    mask_file = c("mask1.nii.gz", "mask2.nii.gz"),
    stringsAsFactors = FALSE
  )

  expect_error(
    plsrri:::.pipeline_resolve_analysis_mask(manifest),
    "multiple mask files"
  )
})

test_that("pipeline_build_firstlevel_plan creates deterministic work units", {
  discovery <- data.frame(
    subject = c("02", "01", "01"),
    group = c("all", "all", "all"),
    task = c("toy", "toy", "other"),
    run = c("1", "1", "1"),
    scan_file = paste0("scan", 1:3, ".nii.gz"),
    events_file = paste0("events", 1:3, ".tsv"),
    mask_file = "mask.nii.gz",
    n_volumes = c(10L, 11L, 12L),
    tr = c(2, 2, 2),
    stringsAsFactors = FALSE
  )

  plan <- plsrri:::.pipeline_build_firstlevel_plan(discovery, "/tmp/fl")

  expect_equal(plan$work_id, c("w0001", "w0002", "w0003"))
  expect_equal(plan$scan_count, c(1L, 1L, 1L))
  expect_equal(plan$work_dir, file.path("/tmp/fl", c("w0001", "w0002", "w0003")))
})

test_that("pipeline_firstlevel_output_file produces stable sanitized names", {
  work_row <- data.frame(
    subject = "01 A",
    task = "faces/localizer",
    work_dir = "/tmp/w0001",
    stringsAsFactors = FALSE
  )

  path <- plsrri:::.pipeline_firstlevel_output_file(
    work_row,
    output_type = "estimates",
    label = "face+t0",
    statistic = "z value"
  )

  expect_equal(
    path,
    "/tmp/w0001/sub-01-A_task-faces-localizer_type-estimates_label-face-t0_stat-z-value.nii.gz"
  )
})
