# Tests for map-manifest ingestion (single 3D map per observation)

test_that("add_subjects_map_manifest builds voxel-map layout and processes to matrices", {
  skip_if_not_installed("neuroim2")
  skip_if_not_installed("RNifti")

  mask_arr <- array(0, dim = c(2, 2, 2))
  mask_arr[1, 1, 1] <- 1
  mask_arr[2, 1, 1] <- 1

  space <- neuroim2::NeuroSpace(dim = c(2, 2, 2), spacing = c(1, 1, 1))
  mask <- neuroim2::NeuroVol(mask_arr, space)

  make_map <- function(value) {
    arr <- array(0, dim = c(2, 2, 2))
    arr[1, 1, 1] <- value
    arr[2, 1, 1] <- value + 0.5
    fn <- tempfile(fileext = ".nii.gz")
    RNifti::writeNifti(arr, fn)
    fn
  }

  files <- c(make_map(1), make_map(2), make_map(3), make_map(4))
  manifest <- data.frame(
    subject = rep(c("s1", "s2"), each = 2),
    condition = rep(c("c1", "c2"), times = 2),
    file = files,
    stringsAsFactors = FALSE
  )

  spec <- pls_spec() |>
    add_subjects_map_manifest(manifest = manifest, mask = mask)

  expect_true(isTRUE(spec$.map_manifest_raw))
  expect_equal(spec$num_cond, 2L)
  expect_equal(spec$feature_layout$kind, "voxel_map")
  expect_equal(spec$feature_layout$n_voxels, 2L)

  spec2 <- plsrri:::.process_map_manifest_data(spec)
  expect_false(isTRUE(spec2$.map_manifest_raw))
  expect_true(is.matrix(spec2$datamat_lst[[1]]))
  expect_equal(dim(spec2$datamat_lst[[1]]), c(4, 2))
  expect_equal(as.numeric(spec2$datamat_lst[[1]][1, ]), c(1, 1.5))
})
