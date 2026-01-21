# Test file for mod_setup Shiny module
# Tests reactive server logic using testServer()

# Required packages for Shiny module testing
library(shiny)
library(shinyjs)
library(shinyFiles)
library(bslib)
library(R6)

# Helper to get module file path
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

# Source module files and dependencies for testing
# Note: Must source to parent frame (test environment), not local
module_path <- test_module_path()
source(file.path(module_path, "ui_components.R"), local = FALSE)
source(file.path(module_path, "state.R"), local = FALSE)
source(file.path(module_path, "mod_setup.R"), local = FALSE)

# Helper to create minimal state_rv for testing
make_test_state_rv <- function() {
  shiny::reactiveValues(
    step = 1L,
    max_step = 1L,
    spec = NULL,
    result = NULL,
    analysis_status = "ready"
  )
}

describe("setup_server initialization", {
  it("returns list with continue and spec reactives", {
    shiny::testServer(setup_server, args = list(state_rv = make_test_state_rv()), {
      # Module should return a list
      result <- session$returned
      expect_type(result, "list")
      expect_named(result, c("continue", "spec"))

      # Both should be reactive
      expect_true(shiny::is.reactive(result$continue) || shiny::is.reactivevalues(result$continue) || is.function(result$continue))
      expect_true(shiny::is.reactive(result$spec))
    })
  })

  it("initializes with data_loaded = FALSE", {
    shiny::testServer(setup_server, args = list(state_rv = make_test_state_rv()), {
      # local_rv should have data_loaded = FALSE initially
      # We can't directly access local_rv, but we can test via outputs
      # When data_loaded is FALSE, data_status should show pending
      expect_false(local_rv$data_loaded)
    })
  })

  it("initializes with default groups", {
    shiny::testServer(setup_server, args = list(state_rv = make_test_state_rv()), {
      # Should have 2 default groups
      expect_length(local_rv$groups, 2)
      expect_equal(local_rv$groups[[1]]$name, "Group 1")
      expect_equal(local_rv$groups[[2]]$name, "Group 2")
    })
  })
})

describe("setup_server validation", {
  it("reports validation errors for missing data", {
    shiny::testServer(setup_server, args = list(state_rv = make_test_state_rv()), {
      # Set inputs to manual mode (default)
      session$setInputs(
        data_source = "manual",
        method = "task",
        num_conditions = 2
      )

      # Validation should report missing data
      errors <- validate_setup()
      expect_true("No data matrices loaded" %in% errors)
    })
  })

  it("validates BIDS directory selection", {
    shiny::testServer(setup_server, args = list(state_rv = make_test_state_rv()), {
      session$setInputs(
        data_source = "bids",
        method = "task",
        num_conditions = 2,
        bids_dir = 0L  # Integer means no directory selected
      )

      errors <- validate_setup()
      expect_true("No BIDS directory selected" %in% errors)
    })
  })

  it("validates bootstrap requires minimum subjects", {
    shiny::testServer(setup_server, args = list(state_rv = make_test_state_rv()), {
      # Set a group with only 2 subjects
      local_rv$groups <- list(
        list(name = "Group 1", n_subj = 2)
      )
      local_rv$data_loaded <- TRUE
      local_rv$data_matrices <- list(matrix(1, nrow = 6, ncol = 10))

      session$setInputs(
        data_source = "manual",
        method = "task",
        num_conditions = 3,
        num_boot = 100  # Bootstrap requires >= 3 subjects
      )

      errors <- validate_setup()
      expect_true("Bootstrap requires at least 3 subjects per group" %in% errors)
    })
  })

  it("no validation errors when setup is valid", {
    shiny::testServer(setup_server, args = list(state_rv = make_test_state_rv()), {
      # Create valid setup
      local_rv$groups <- list(
        list(name = "Group 1", n_subj = 10)
      )
      local_rv$data_loaded <- TRUE
      local_rv$data_matrices <- list(matrix(1, nrow = 30, ncol = 100))

      session$setInputs(
        data_source = "manual",
        method = "task",
        num_conditions = 3,
        num_perm = 100,
        num_boot = 50
      )

      errors <- validate_setup()
      expect_length(errors, 0)
    })
  })
})

describe("setup_server spec building", {
  it("method selection maps to correct integer", {
    shiny::testServer(setup_server, args = list(state_rv = make_test_state_rv()), {
      # Setup valid data
      local_rv$groups <- list(list(name = "Group 1", n_subj = 10))
      local_rv$data_loaded <- TRUE
      local_rv$data_matrices <- list(matrix(1, nrow = 30, ncol = 100))

      # Test task method (should be 1)
      session$setInputs(
        data_source = "manual",
        method = "task",
        num_conditions = 3,
        num_perm = 100,
        num_boot = 50,
        confidence = 95,
        boot_type = "strat",
        num_split = 0,
        meancentering = "0"
      )
      session$setInputs(btn_continue = 1)

      spec <- local_rv$spec
      expect_equal(spec$method, 1L)
    })
  })

  it("behavior method maps to integer 3", {
    shiny::testServer(setup_server, args = list(state_rv = make_test_state_rv()), {
      # Setup valid data
      local_rv$groups <- list(list(name = "Group 1", n_subj = 10))
      local_rv$data_loaded <- TRUE
      local_rv$data_matrices <- list(matrix(1, nrow = 30, ncol = 100))

      session$setInputs(
        data_source = "manual",
        method = "behavior",
        num_conditions = 3,
        num_perm = 100,
        num_boot = 50,
        confidence = 95,
        boot_type = "strat",
        num_split = 0,
        meancentering = "0"
      )
      session$setInputs(btn_continue = 1)

      spec <- local_rv$spec
      expect_equal(spec$method, 3L)
    })
  })

  it("multiblock method maps to integer 4", {
    shiny::testServer(setup_server, args = list(state_rv = make_test_state_rv()), {
      # Setup valid data
      local_rv$groups <- list(list(name = "Group 1", n_subj = 10))
      local_rv$data_loaded <- TRUE
      local_rv$data_matrices <- list(matrix(1, nrow = 30, ncol = 100))

      session$setInputs(
        data_source = "manual",
        method = "multiblock",
        num_conditions = 3,
        num_perm = 100,
        num_boot = 50,
        confidence = 95,
        boot_type = "strat",
        num_split = 0,
        meancentering = "0"
      )
      session$setInputs(btn_continue = 1)

      spec <- local_rv$spec
      expect_equal(spec$method, 4L)
    })
  })

  it("captures resampling parameters in spec", {
    shiny::testServer(setup_server, args = list(state_rv = make_test_state_rv()), {
      # Setup valid data
      local_rv$groups <- list(list(name = "Group 1", n_subj = 10))
      local_rv$data_loaded <- TRUE
      local_rv$data_matrices <- list(matrix(1, nrow = 30, ncol = 100))

      session$setInputs(
        data_source = "manual",
        method = "task",
        num_conditions = 3,
        num_perm = 500,
        num_boot = 200,
        confidence = 95,
        boot_type = "nonstrat",
        num_split = 100,
        meancentering = "1"
      )
      session$setInputs(btn_continue = 1)

      spec <- local_rv$spec
      expect_equal(spec$num_perm, 500L)
      expect_equal(spec$num_boot, 200L)
      expect_equal(spec$clim, 95L)
      expect_equal(spec$boot_type, "nonstrat")
      expect_equal(spec$num_split, 100L)
      expect_equal(spec$meancentering_type, 1L)
    })
  })

  it("captures group information in spec", {
    shiny::testServer(setup_server, args = list(state_rv = make_test_state_rv()), {
      # Setup valid data with 2 groups
      local_rv$groups <- list(
        list(name = "Control", n_subj = 15),
        list(name = "Patient", n_subj = 12)
      )
      local_rv$data_loaded <- TRUE
      local_rv$data_matrices <- list(
        matrix(1, nrow = 45, ncol = 100),
        matrix(1, nrow = 36, ncol = 100)
      )

      session$setInputs(
        data_source = "manual",
        method = "task",
        num_conditions = 3,
        num_perm = 100,
        num_boot = 50,
        confidence = 95,
        boot_type = "strat",
        num_split = 0,
        meancentering = "0"
      )
      session$setInputs(btn_continue = 1)

      spec <- local_rv$spec
      expect_equal(spec$num_subj_lst, c(15L, 12L))
      expect_equal(spec$groups, c("Control", "Patient"))
      expect_equal(spec$num_cond, 3L)
    })
  })
})

describe("setup_server continue trigger", {
  it("increments continue trigger with valid setup", {
    shiny::testServer(setup_server, args = list(state_rv = make_test_state_rv()), {
      # Setup valid data
      local_rv$groups <- list(list(name = "Group 1", n_subj = 10))
      local_rv$data_loaded <- TRUE
      local_rv$data_matrices <- list(matrix(1, nrow = 30, ncol = 100))

      session$setInputs(
        data_source = "manual",
        method = "task",
        num_conditions = 3,
        num_perm = 100,
        num_boot = 50,
        confidence = 95,
        boot_type = "strat",
        num_split = 0,
        meancentering = "0"
      )

      # Get initial trigger value
      initial_value <- continue_trigger()

      # Click continue
      session$setInputs(btn_continue = 1)

      # Trigger should increment
      expect_gt(continue_trigger(), initial_value)
    })
  })

  it("does not increment trigger with invalid setup", {
    shiny::testServer(setup_server, args = list(state_rv = make_test_state_rv()), {
      # Invalid setup - no data loaded
      session$setInputs(
        data_source = "manual",
        method = "task",
        num_conditions = 3
      )

      # Get initial trigger value
      initial_value <- continue_trigger()

      # Click continue (should not work due to validation)
      session$setInputs(btn_continue = 1)

      # Trigger should NOT increment (validation fails)
      expect_equal(continue_trigger(), initial_value)
    })
  })

  it("builds spec on continue", {
    shiny::testServer(setup_server, args = list(state_rv = make_test_state_rv()), {
      # Setup valid data
      local_rv$groups <- list(list(name = "Group 1", n_subj = 10))
      local_rv$data_loaded <- TRUE
      local_rv$data_matrices <- list(matrix(1, nrow = 30, ncol = 100))

      session$setInputs(
        data_source = "manual",
        method = "task",
        num_conditions = 3,
        num_perm = 100,
        num_boot = 50,
        confidence = 95,
        boot_type = "strat",
        num_split = 0,
        meancentering = "0"
      )

      # Initial spec should be NULL
      expect_null(local_rv$spec)

      # Click continue
      session$setInputs(btn_continue = 1)

      # Spec should now be populated
      expect_false(is.null(local_rv$spec))
      expect_s3_class(local_rv$spec, "pls_spec")
    })
  })
})

describe("setup_server group management", {
  it("can add groups", {
    shiny::testServer(setup_server, args = list(state_rv = make_test_state_rv()), {
      # Start with 2 groups
      expect_length(local_rv$groups, 2)

      # Add a group
      session$setInputs(add_group = 1)

      # Should have 3 groups
      expect_length(local_rv$groups, 3)
      expect_equal(local_rv$groups[[3]]$name, "Group 3")
    })
  })

  it("can remove groups", {
    shiny::testServer(setup_server, args = list(state_rv = make_test_state_rv()), {
      # Start with 2 groups
      expect_length(local_rv$groups, 2)

      # Remove a group
      session$setInputs(remove_group = 1)

      # Should have 1 group
      expect_length(local_rv$groups, 1)
    })
  })

  it("cannot remove last group", {
    shiny::testServer(setup_server, args = list(state_rv = make_test_state_rv()), {
      # Reduce to 1 group
      local_rv$groups <- list(list(name = "Group 1", n_subj = 10))

      # Try to remove
      session$setInputs(remove_group = 1)

      # Should still have 1 group
      expect_length(local_rv$groups, 1)
    })
  })
})
