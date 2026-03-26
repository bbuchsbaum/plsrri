# Tests for manifest-based datamat ingestion (NIfTI dim4 = lags)

test_that("add_subjects_manifest builds voxel×lag feature layout and processes to matrices", {
  skip_if_not_installed("neuroim2")
  skip_if_not_installed("RNifti")

  # Small mask with 2 voxels in a 2x2x2 volume
  mask_arr <- array(0, dim = c(2, 2, 2))
  mask_arr[1, 1, 1] <- 1
  mask_arr[2, 1, 1] <- 1

  space <- neuroim2::NeuroSpace(dim = c(2, 2, 2), spacing = c(1, 1, 1))
  mask <- neuroim2::NeuroVol(mask_arr, space)

  # Create 4D files where each lag volume is filled with the lag index
  make_lag_file <- function(n_lags = 3L) {
    arr <- array(0, dim = c(2, 2, 2, n_lags))
    for (lag in seq_len(n_lags)) arr[, , , lag] <- lag
    fn <- tempfile(fileext = ".nii")
    RNifti::writeNifti(arr, fn)
    fn
  }

  files <- replicate(4, make_lag_file(3L))
  manifest <- data.frame(
    subject = rep(c("s1", "s2"), each = 2),
    condition = rep(c("c1", "c2"), times = 2),
    file = files,
    stringsAsFactors = FALSE
  )

  spec <- pls_spec() |>
    add_subjects_manifest(manifest = manifest, mask = mask)

  expect_true(isTRUE(spec$.manifest_raw))
  expect_equal(spec$num_cond, 2L)
  expect_true(is.list(spec$feature_layout))
  expect_equal(spec$feature_layout$kind, "voxel_lag")
  expect_equal(spec$feature_layout$n_voxels, 2L)
  expect_equal(spec$feature_layout$n_lags, 3L)

  spec2 <- plsrri:::.process_manifest_data(spec)
  expect_false(isTRUE(spec2$.manifest_raw))
  expect_true(is.matrix(spec2$datamat_lst[[1]]))
  expect_equal(dim(spec2$datamat_lst[[1]]), c(4, 6))

  # Verify MATLAB-compatible fold order (lag varies fastest within voxel)
  expect_equal(as.numeric(spec2$datamat_lst[[1]][1, ]), c(1, 2, 3, 1, 2, 3))
})

test_that("manifest file[volspec] syntax selects a subset of lags", {
  skip_if_not_installed("neuroim2")
  skip_if_not_installed("RNifti")

  mask_arr <- array(0, dim = c(2, 2, 2))
  mask_arr[1, 1, 1] <- 1
  mask_arr[2, 1, 1] <- 1
  space <- neuroim2::NeuroSpace(dim = c(2, 2, 2), spacing = c(1, 1, 1))
  mask <- neuroim2::NeuroVol(mask_arr, space)

  arr <- array(0, dim = c(2, 2, 2, 3))
  arr[, , , 1] <- 1
  arr[, , , 2] <- 2
  arr[, , , 3] <- 3
  fn <- tempfile(fileext = ".nii")
  RNifti::writeNifti(arr, fn)

  manifest <- data.frame(
    subject = rep(c("s1", "s2"), each = 2),
    condition = rep(c("c1", "c2"), times = 2),
    file = rep(paste0(fn, "[1:2]"), 4),
    stringsAsFactors = FALSE
  )

  spec <- pls_spec() |>
    add_subjects_manifest(manifest = manifest, mask = mask)

  expect_equal(spec$feature_layout$n_lags, 2L)

  spec2 <- plsrri:::.process_manifest_data(spec)
  expect_equal(dim(spec2$datamat_lst[[1]]), c(4, 4))
  expect_equal(as.numeric(spec2$datamat_lst[[1]][1, ]), c(1, 2, 1, 2))
})

