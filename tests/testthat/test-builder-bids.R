# Tests for BIDS integration via bidser
#
# These tests verify that plsrri correctly uses the bidser API.
# They use mocking to test the integration without requiring actual BIDS data.

# Skip all if bidser not available
skip_if_no_bidser <- function() {

  skip_if_not_installed("bidser")
}

# -----------------------------------------------------------------------------
# API Contract Tests - verify we call bidser functions correctly
# -----------------------------------------------------------------------------

test_that("bidser::participants returns character vector, not data.frame", {
  skip_if_no_bidser()

  # This test documents the expected bidser API behavior

  # If this fails, bidser's API changed and we need to update our code

  # Create a mock bids_project object to test with
  # We can't easily test with real data, but we can verify the contract
  mock_bids <- structure(
    list(
      part_df = data.frame(
        participant_id = c("sub-01", "sub-02"),
        group = c("control", "patient"),
        stringsAsFactors = FALSE
      )
    ),
    class = "bids_project"
  )

  # The real participants() should return a character vector
  # This is what we need to verify our code expects
  result <- bidser::participants(mock_bids)

  expect_type(result, "character")
  expect_false(is.data.frame(result))
})

test_that("bidser::preproc_scans uses 'subid' parameter, not 'subject'", {
  skip_if_no_bidser()


  # Check that preproc_scans has subid as a parameter
  args <- names(formals(bidser::preproc_scans))
  expect_true("subid" %in% args)
  expect_false("subject" %in% args)
})

test_that("bidser::mask_files uses 'subid' parameter", {
  skip_if_no_bidser()

  args <- names(formals(bidser::mask_files))
  expect_true("subid" %in% args)
})

# Note: search_files uses ... for extra args, so we don't test its formals directly.
# We only test functions we actually use: preproc_scans and mask_files.

# -----------------------------------------------------------------------------
# Unit Tests with Mocking
# -----------------------------------------------------------------------------

test_that(".add_subjects_bids validates task parameter", {
  skip_if_no_bidser()
  skip_if_not_installed("neuroim2")

  spec <- pls_spec()

  # Should error if task is NULL
  expect_error(
    .add_subjects_bids(spec, "/fake/path", groups = NULL, task = NULL,
                       space = "MNI", mask_method = "intersection"),
    "task must be specified"
  )
})

test_that(".add_subjects_bids handles group specification", {
  skip_if_no_bidser()
  skip_if_not_installed("neuroim2")

  # We'll use local_mocked_bindings to mock bidser functions
  # This requires testthat >= 3.0.0

  local_mocked_bindings(
    bids_project = function(path, fmriprep = FALSE, ...) {
      structure(
        list(
          path = path,
          part_df = data.frame(
            participant_id = c("01", "02", "03", "04"),
            group = c("control", "control", "patient", "patient"),
            stringsAsFactors = FALSE
          ),
          has_fmriprep = fmriprep
        ),
        class = "bids_project"
      )
    },
    participants = function(x, ...) {
      c("01", "02", "03", "04")
    },
    preproc_scans = function(x, subid, task, space, full_path = FALSE, ...) {
      # Return NULL to simulate no files found (which will cause warning/skip)
      NULL
    },
    mask_files = function(x, subid, space, full_path = FALSE, ...) {
      NULL
    },
    .package = "bidser"
  )

  spec <- pls_spec()

  # Test groups = "all"
  # This will fail because no files are found, but it tests the group logic
  expect_error(
    .add_subjects_bids(spec, "/fake/path", groups = "all", task = "rest",
                       space = "MNI", mask_method = "intersection"),
    "No data loaded"  # Expected because preproc_scans returns NULL
  )
})

# -----------------------------------------------------------------------------
# Integration Tests (skip if no real test data)
# -----------------------------------------------------------------------------

test_that("BIDS integration works with real bidser project", {
  skip_if_no_bidser()
  skip_if_not_installed("neuroim2")
  skip("Integration test requires real BIDS data - run manually")

  # This test would be run manually with actual BIDS data
  # Example:
  # bids_path <- "/path/to/test/bids/dataset"
  # skip_if(!dir.exists(bids_path), "Test BIDS dataset not available")
  #
  # spec <- pls_spec() |>
  #   add_subjects(bids_path, task = "rest", space = "MNI152NLin2009cAsym")
  #
  # expect_true(length(spec$datamat_lst) > 0)
  # expect_true(sum(spec$num_subj_lst) > 0)
})

# -----------------------------------------------------------------------------
# Regression Tests - specific bugs we've fixed
# -----------------------------------------------------------------------------

test_that("participants() return type is correctly handled", {
  skip_if_no_bidser()

  # Regression test: we previously assumed participants() returns a data.frame
  # with $participant_id column, but it returns a character vector

  # Verify our internal function doesn't try to subset with $
  # by checking the code doesn't contain the bug pattern

  code <- deparse(body(.add_subjects_bids))
  code_str <- paste(code, collapse = "\n")

  # Should NOT contain: participants(...)$participant_id
  expect_false(
    grepl("participants\\([^)]+\\)\\$participant_id", code_str),
    info = "Code should not try to access $participant_id on participants() result"
  )
})

test_that("bidser functions called with correct parameter names", {
  skip_if_no_bidser()

  code <- deparse(body(.add_subjects_bids))
  code_str <- paste(code, collapse = "\n")

  # Should use 'subid =' not 'subject =' in bidser function calls
  expect_false(
    grepl("(preproc_scans|mask_files).*subject\\s*=", code_str),
    info = "bidser functions should use 'subid' parameter, not 'subject'"
  )

  # Verify we ARE using subid correctly

  expect_true(
    grepl("preproc_scans.*subid\\s*=", code_str),
    info = "preproc_scans should be called with subid parameter"
  )

  expect_true(
    grepl("mask_files.*subid\\s*=", code_str),
    info = "mask_files should be called with subid parameter"
  )
})

# -----------------------------------------------------------------------------
# Mock-based Full Pipeline Test
# -----------------------------------------------------------------------------

test_that("BIDS pipeline with mocked bidser", {
  skip_if_no_bidser()
  skip_if_not_installed("neuroim2")

  # Create minimal mock objects
  mock_vol <- array(rnorm(10 * 10 * 10 * 5), dim = c(10, 10, 10, 5))
  mock_mask <- array(1, dim = c(10, 10, 10))

  # Track which functions were called with what arguments
  call_log <- list()

  local_mocked_bindings(
    bids_project = function(path, fmriprep = FALSE, ...) {
      call_log$bids_project <<- list(path = path, fmriprep = fmriprep)
      structure(
        list(
          path = path,
          part_df = data.frame(
            participant_id = c("01", "02"),
            group = c("A", "A"),
            stringsAsFactors = FALSE
          ),
          has_fmriprep = fmriprep
        ),
        class = "bids_project"
      )
    },
    participants = function(x, ...) {
      call_log$participants <<- TRUE
      c("01", "02")  # Must return character vector
    },
    preproc_scans = function(x, subid, task, space, full_path = FALSE, ...) {
      call_log$preproc_scans <<- c(call_log$preproc_scans, list(
        list(subid = subid, task = task, space = space)
      ))
      # Return a fake file path
      "/fake/path/func.nii.gz"
    },
    mask_files = function(x, subid, space, full_path = FALSE, ...) {
      call_log$mask_files <<- c(call_log$mask_files, list(
        list(subid = subid, space = space)
      ))
      "/fake/path/mask.nii.gz"
    },
    .package = "bidser"
  )

  local_mocked_bindings(
    read_vol = function(path, ...) {
      if (grepl("mask", path)) {
        structure(mock_mask, class = c("NeuroVol", "array"),
                  space = list())
      } else {
        structure(mock_vol, class = c("NeuroVol", "array"),
                  space = list())
      }
    },
    space = function(x, ...) {
      list()
    },
    NeuroVol = function(data, space, ...) {
      structure(data, class = c("NeuroVol", "array"), space = space)
    },
    .package = "neuroim2"
  )

  spec <- pls_spec()

  # This should work with mocked functions
  result_spec <- .add_subjects_bids(
    spec,
    bids_dir = "/fake/bids",
    groups = NULL,
    task = "rest",
    space = "MNI",
    mask_method = "intersection"
  )

  # Verify bidser was called correctly
  expect_true(call_log$bids_project$fmriprep)
  expect_true(call_log$participants)

  # Verify preproc_scans was called with 'subid', not 'subject'
  expect_true(length(call_log$preproc_scans) >= 1)
  expect_true("subid" %in% names(call_log$preproc_scans[[1]]))
  expect_false("subject" %in% names(call_log$preproc_scans[[1]]))

  # Verify mask_files was called with 'subid'
  expect_true(length(call_log$mask_files) >= 1)
  expect_true("subid" %in% names(call_log$mask_files[[1]]))
})
