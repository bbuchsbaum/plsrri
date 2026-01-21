# Test fct_data_validation pure functions
# These tests run WITHOUT Shiny context - plain testthat

# Helper to get module file path (matches pattern from other test files)
test_module_path <- function() {
  # Handle both installed package and dev mode
  pkg_path <- system.file("shiny/R", package = "plsrri")
  if (pkg_path == "") {
    # Check if we're running from package root or tests dir
    if (file.exists("inst/shiny/R")) {
      pkg_path <- "inst/shiny/R"
    } else if (file.exists("../../inst/shiny/R")) {
      pkg_path <- "../../inst/shiny/R"
    } else {
      pkg_path <- file.path(getwd(), "inst/shiny/R")
    }
  }
  pkg_path
}

# Source the functions directly (they're internal to shiny app)
module_path <- test_module_path()
source(file.path(module_path, "fct_data_validation.R"))

# ===========================================================================
# validate_setup_config tests
# ===========================================================================

describe("validate_setup_config", {
  it("returns empty vector for valid config", {
    groups <- list(
      list(name = "Group 1", n_subj = 10),
      list(name = "Group 2", n_subj = 8)
    )
    errors <- validate_setup_config(
      data_source = "manual",
      data_loaded = TRUE,
      bids_dir = NULL,
      groups = groups,
      num_conditions = 3,
      num_boot = 100
    )
    expect_equal(errors, character(0))
  })

  it("returns error for missing data in manual mode", {
    groups <- list(list(name = "Group 1", n_subj = 10))
    errors <- validate_setup_config(
      data_source = "manual",
      data_loaded = FALSE,
      bids_dir = NULL,
      groups = groups,
      num_conditions = 3,
      num_boot = 0
    )
    expect_true("No data matrices loaded" %in% errors)
  })

  it("returns error for missing BIDS directory", {
    groups <- list(list(name = "Group 1", n_subj = 10))
    errors <- validate_setup_config(
      data_source = "bids",
      data_loaded = FALSE,
      bids_dir = 0L,  # Integer means not selected
      groups = groups,
      num_conditions = 3,
      num_boot = 0
    )
    expect_true("No BIDS directory selected" %in% errors)
  })

  it("returns error for zero groups", {
    errors <- validate_setup_config(
      data_source = "manual",
      data_loaded = TRUE,
      bids_dir = NULL,
      groups = list(),
      num_conditions = 3,
      num_boot = 0
    )
    expect_true("At least one group required" %in% errors)
  })

  it("returns error for zero subjects in group", {
    groups <- list(list(name = "Group 1", n_subj = 0))
    errors <- validate_setup_config(
      data_source = "manual",
      data_loaded = TRUE,
      bids_dir = NULL,
      groups = groups,
      num_conditions = 3,
      num_boot = 0
    )
    expect_true("All groups must have at least 1 subject" %in% errors)
  })

  it("returns error for zero conditions", {
    groups <- list(list(name = "Group 1", n_subj = 10))
    errors <- validate_setup_config(
      data_source = "manual",
      data_loaded = TRUE,
      bids_dir = NULL,
      groups = groups,
      num_conditions = 0,
      num_boot = 0
    )
    expect_true("At least one condition required" %in% errors)
  })

  it("returns error for bootstrap with less than 3 subjects", {
    groups <- list(list(name = "Group 1", n_subj = 2))
    errors <- validate_setup_config(
      data_source = "manual",
      data_loaded = TRUE,
      bids_dir = NULL,
      groups = groups,
      num_conditions = 3,
      num_boot = 100
    )
    expect_true("Bootstrap requires at least 3 subjects per group" %in% errors)
  })

  it("returns multiple errors together", {
    groups <- list(list(name = "Group 1", n_subj = 2))
    errors <- validate_setup_config(
      data_source = "manual",
      data_loaded = FALSE,
      bids_dir = NULL,
      groups = groups,
      num_conditions = 0,
      num_boot = 100
    )
    expect_true(length(errors) >= 3)
    expect_true("No data matrices loaded" %in% errors)
    expect_true("At least one condition required" %in% errors)
    expect_true("Bootstrap requires at least 3 subjects per group" %in% errors)
  })
})

# ===========================================================================
# validate_groups tests
# ===========================================================================

describe("validate_groups", {
  it("returns empty vector for valid groups", {
    groups <- list(
      list(name = "Group 1", n_subj = 10),
      list(name = "Group 2", n_subj = 8)
    )
    errors <- validate_groups(groups)
    expect_equal(errors, character(0))
  })

  it("returns error for empty groups list", {
    errors <- validate_groups(list())
    expect_true("At least one group required" %in% errors)
  })

  it("returns error for group with 0 subjects", {
    groups <- list(list(name = "Group 1", n_subj = 0))
    errors <- validate_groups(groups)
    expect_true("All groups must have at least 1 subject" %in% errors)
  })

  it("returns error for group with negative subjects", {
    groups <- list(list(name = "Group 1", n_subj = -5))
    errors <- validate_groups(groups)
    expect_true("All groups must have at least 1 subject" %in% errors)
  })

  it("returns error for NA subject count", {
    groups <- list(list(name = "Group 1", n_subj = NA))
    errors <- validate_groups(groups)
    expect_true("All groups must have at least 1 subject" %in% errors)
  })
})

# ===========================================================================
# validate_data_source tests
# ===========================================================================

describe("validate_data_source", {
  it("returns empty vector for manual with data loaded", {
    errors <- validate_data_source(
      data_source = "manual",
      data_loaded = TRUE,
      bids_dir = NULL
    )
    expect_equal(errors, character(0))
  })

  it("returns error for manual with data not loaded", {
    errors <- validate_data_source(
      data_source = "manual",
      data_loaded = FALSE,
      bids_dir = NULL
    )
    expect_true("No data matrices loaded" %in% errors)
  })

  it("returns empty vector for bids with valid directory", {
    # Non-integer means directory was selected
    errors <- validate_data_source(
      data_source = "bids",
      data_loaded = FALSE,
      bids_dir = list(root = "home", path = "data/bids")
    )
    expect_equal(errors, character(0))
  })

  it("returns error for bids with integer directory", {
    # Integer means no directory selected (shinyFiles convention)
    errors <- validate_data_source(
      data_source = "bids",
      data_loaded = FALSE,
      bids_dir = 0L
    )
    expect_true("No BIDS directory selected" %in% errors)
  })

  it("returns empty vector for load mode", {
    errors <- validate_data_source(
      data_source = "load",
      data_loaded = FALSE,
      bids_dir = NULL
    )
    expect_equal(errors, character(0))
  })
})

# ===========================================================================
# map_method_to_int tests
# ===========================================================================

describe("map_method_to_int", {
  it("maps task to 1L", {
    result <- map_method_to_int("task")
    expect_equal(result, 1L)
  })

  it("maps behavior to 3L", {
    result <- map_method_to_int("behavior")
    expect_equal(result, 3L)
  })

  it("maps multiblock to 4L", {
    result <- map_method_to_int("multiblock")
    expect_equal(result, 4L)
  })

  it("defaults unknown method to 1L", {
    result <- map_method_to_int("unknown")
    expect_equal(result, 1L)
  })

  it("defaults empty string to 1L", {
    result <- map_method_to_int("")
    expect_equal(result, 1L)
  })
})

# ===========================================================================
# parse_uploaded_file tests
# ===========================================================================

describe("parse_uploaded_file", {
  it("parses csv file to matrix", {
    # Create temp CSV file
    tmp_csv <- tempfile(fileext = ".csv")
    mat <- matrix(1:12, nrow = 3, ncol = 4)
    rownames(mat) <- paste0("row", 1:3)
    write.csv(mat, tmp_csv)

    result <- parse_uploaded_file(tmp_csv, "test.csv")
    expect_true(is.matrix(result))
    expect_equal(nrow(result), 3)
    expect_equal(ncol(result), 4)

    unlink(tmp_csv)
  })

  it("parses rds file to matrix", {
    # Create temp RDS file
    tmp_rds <- tempfile(fileext = ".rds")
    mat <- matrix(1:12, nrow = 3, ncol = 4)
    saveRDS(mat, tmp_rds)

    result <- parse_uploaded_file(tmp_rds, "test.rds")
    expect_true(is.matrix(result))
    expect_equal(nrow(result), 3)
    expect_equal(ncol(result), 4)

    unlink(tmp_rds)
  })

  it("parses rda file to matrix", {
    # Create temp RDA file
    tmp_rda <- tempfile(fileext = ".rda")
    mat <- matrix(1:12, nrow = 3, ncol = 4)
    save(mat, file = tmp_rda)

    result <- parse_uploaded_file(tmp_rda, "test.rda")
    expect_true(is.matrix(result))
    expect_equal(nrow(result), 3)
    expect_equal(ncol(result), 4)

    unlink(tmp_rda)
  })

  it("returns NULL for invalid file", {
    result <- parse_uploaded_file("/nonexistent/path.csv", "fake.csv")
    expect_null(result)
  })

  it("returns NULL for unsupported extension", {
    tmp_txt <- tempfile(fileext = ".txt")
    writeLines("not a matrix", tmp_txt)

    result <- parse_uploaded_file(tmp_txt, "test.txt")
    expect_null(result)

    unlink(tmp_txt)
  })

  it("returns NULL if file content is not a matrix", {
    # Create temp RDS with non-matrix data
    tmp_rds <- tempfile(fileext = ".rds")
    saveRDS(data.frame(a = 1:3, b = 4:6), tmp_rds)

    result <- parse_uploaded_file(tmp_rds, "test.rds")
    expect_null(result)

    unlink(tmp_rds)
  })

  it("handles CSV file case-insensitively", {
    tmp_csv <- tempfile(fileext = ".CSV")
    mat <- matrix(1:6, nrow = 2, ncol = 3)
    rownames(mat) <- paste0("row", 1:2)
    write.csv(mat, tmp_csv)

    result <- parse_uploaded_file(tmp_csv, "test.CSV")
    expect_true(is.matrix(result))

    unlink(tmp_csv)
  })
})
