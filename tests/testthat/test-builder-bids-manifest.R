# Tests for BIDS -> manifest builder

test_that("add_subjects_bids_manifest builds a manifest spec with lag labels preserved", {
  skip_if_not_installed("bidser")
  skip_if_not_installed("neuroim2")
  skip_if_not_installed("RNifti")

  # Create a fake BIDS directory root
  bids_root <- tempfile("bids_")
  dir.create(bids_root)
  writeLines('{"Name":"test","BIDSVersion":"1.8.0"}', file.path(bids_root, "dataset_description.json"))

  # Mask (2 voxels)
  mask_arr <- array(0, dim = c(2, 2, 2))
  mask_arr[1, 1, 1] <- 1
  mask_arr[2, 1, 1] <- 1
  mask_file <- file.path(bids_root, "mask.nii")
  RNifti::writeNifti(mask_arr, mask_file)

  # Make 4D lag files for 2 subjects x 2 conditions
  make_lag_file <- function(subj, cond, n_lags = 3L) {
    arr <- array(0, dim = c(2, 2, 2, n_lags))
    for (lag in seq_len(n_lags)) arr[, , , lag] <- lag
    fn <- file.path(bids_root, sprintf("sub-%s_cond-%s_desc-fir_bold.nii.gz", subj, cond))
    RNifti::writeNifti(arr, fn)
    fn
  }

  files <- c(
    make_lag_file("01", "c1"),
    make_lag_file("01", "c2"),
    make_lag_file("02", "c1"),
    make_lag_file("02", "c2")
  )

  local_mocked_bindings(
    .bidser_bids_project = function(path, fmriprep = FALSE, ...) {
      structure(list(path = path, has_fmriprep = fmriprep), class = "bids_project")
    },
    .bidser_participants = function(x, ...) {
      c("01", "02")
    },
    .bidser_mask_files = function(x, subid, full_path = FALSE, ...) {
      mask_file
    },
    .bidser_search_files = function(x, regex = ".*", full_path = FALSE, ...) {
      bn <- basename(files)
      keep <- grepl(regex, bn, perl = TRUE)
      if (!any(keep)) return(NULL)
      files[keep]
    },
    .package = "plsrri"
  )

  expect_equal(plsrri:::.bidser_participants(NULL), c("01", "02"))
  expect_equal(plsrri:::.bidser_mask_files(NULL, subid = "^01$", full_path = TRUE), mask_file)
  expect_true(length(plsrri:::.bidser_search_files(NULL, regex = "^sub-01", full_path = TRUE)) > 0)
	  file_regex <- ".*_desc-fir_bold\\.nii(\\.gz)?$"
  rx_full <- paste0("^sub-01.*", file_regex)
  expect_true(any(grepl(rx_full, basename(files), perl = TRUE)))
  expect_true(length(plsrri:::.bidser_search_files(NULL, regex = rx_full, full_path = TRUE)) > 0)

  spec <- pls_spec() |>
    add_subjects_bids_manifest(
      bids_dir = bids_root,
      file_regex = file_regex,
      condition_keys = "cond",
      volumes = "2:3",
      mask_method = "first",
      strict = TRUE
    )

  expect_true(isTRUE(spec$.manifest_raw))
  expect_equal(spec$num_cond, 2L)
  expect_equal(spec$feature_layout$kind, "voxel_lag")
  expect_equal(spec$feature_layout$n_lags, 2L)
  expect_equal(as.integer(spec$feature_layout$lag_labels), c(1L, 2L))
})
