# Seed PLS: manifest-based voxel×lag inputs

test_that("run() can derive seed behavior data from manifest voxel×lag datamat", {
  skip_if_not_installed("neuroim2")
  skip_if_not_installed("RNifti")

  # Mask with 2 voxels in a 2x2x2 volume
  mask_arr <- array(0, dim = c(2, 2, 2))
  mask_arr[1, 1, 1] <- 1
  mask_arr[2, 1, 1] <- 1
  space <- neuroim2::NeuroSpace(dim = c(2, 2, 2), spacing = c(1, 1, 1))
  mask <- neuroim2::NeuroVol(mask_arr, space)

  # Custom labeled atlas: ROI 1 = voxel1, ROI 2 = voxel2
  atlas_arr <- array(0, dim = c(2, 2, 2))
  atlas_arr[1, 1, 1] <- 1
  atlas_arr[2, 1, 1] <- 2
  atlas <- neuroim2::NeuroVol(atlas_arr, space)

  # Create per-observation 4D files (dim4 = lags) with distinct values
  make_obs_file <- function(obs_id, n_lags = 3L) {
    arr <- array(0, dim = c(2, 2, 2, n_lags))
    for (lag in seq_len(n_lags)) {
      arr[1, 1, 1, lag] <- obs_id * 10 + lag
      arr[2, 1, 1, lag] <- obs_id * 100 + lag
    }
    fn <- tempfile(fileext = ".nii")
    RNifti::writeNifti(arr, fn)
    fn
  }

  files <- vapply(seq_len(4), make_obs_file, character(1))
  manifest <- data.frame(
    subject = c("s1", "s2", "s1", "s2"),
    condition = c("c1", "c1", "c2", "c2"),
    file = files,
    stringsAsFactors = FALSE
  )

  spec <- pls_spec() |>
    add_subjects_manifest(manifest = manifest, mask = mask)
  spec$method <- 3L
  spec$num_perm <- 0L
  spec$num_boot <- 0L
  spec$cormode <- 0L
  spec$seed_info <- list(
    seed_source = "custom_atlas",
    seed_atlas = atlas,
    roi_ids = c(1L, 2L),
    roi_labels = c("A", "B"),
    summary = "mean"
  )

  result <- run(spec, progress = FALSE)

  expect_true(is.matrix(result$stacked_behavdata))
  expect_equal(ncol(result$stacked_behavdata), 6L)
  expect_equal(
    colnames(result$stacked_behavdata),
    c("A_lag0", "A_lag1", "A_lag2", "B_lag0", "B_lag1", "B_lag2")
  )

  expect_equal(
    as.numeric(result$stacked_behavdata[1, ]),
    c(11, 12, 13, 101, 102, 103)
  )
})

