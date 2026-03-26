# Test file for mod_analyze Shiny module
# Tests reactive server logic using testServer()

# Required packages for Shiny module testing
library(shiny)
library(shinyjs)
library(bslib)
library(R6)

# Helper to get module file path
test_module_path <- function() {
  pkg_path <- system.file("shiny/R", package = "plsrri")
  if (pkg_path == "") {
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
module_path <- test_module_path()
source(file.path(module_path, "ui_components.R"), local = FALSE)
source(file.path(module_path, "state.R"), local = FALSE)
source(file.path(module_path, "fct_prepared_analysis.R"), local = FALSE)
source(file.path(module_path, "mod_analyze.R"), local = FALSE)

# Helper to create minimal state_rv for testing
make_test_state_rv <- function(step = 1L, spec = NULL, analysis_status = "ready") {
  shiny::reactiveValues(
    step = step,
    max_step = step,
    spec = spec,
    result = NULL,
    analysis_status = analysis_status,
    analysis_source = "direct",
    prepared_analysis = NULL,
    analyze_mode = "pls_only"
  )
}

describe("analyze_server initialization", {
  it("returns list with complete and result reactives", {
    shiny::testServer(analyze_server, args = list(state_rv = make_test_state_rv()), {
      result <- session$returned
      expect_type(result, "list")
      expect_named(result, c("complete", "result"))

      # Both should be reactive/function
      expect_true(is.function(result$complete) || shiny::is.reactive(result$complete))
      expect_true(shiny::is.reactive(result$result))
    })
  })

  it("initializes with status 'ready'", {
    shiny::testServer(analyze_server, args = list(state_rv = make_test_state_rv()), {
      expect_equal(local_rv$status, "ready")
    })
  })

  it("initializes with progress 0", {
    shiny::testServer(analyze_server, args = list(state_rv = make_test_state_rv()), {
      expect_equal(local_rv$progress, 0)
    })
  })

  it("initializes with null result", {
    shiny::testServer(analyze_server, args = list(state_rv = make_test_state_rv()), {
      expect_null(local_rv$result)
    })
  })

  it("initializes with null error_message", {
    shiny::testServer(analyze_server, args = list(state_rv = make_test_state_rv()), {
      expect_null(local_rv$error_message)
    })
  })
})

describe("analyze_server prepared-analysis review", {
  it("can summarize a prepared analysis without a prebuilt pls_spec", {
    state_rv <- make_test_state_rv(step = 2L, spec = NULL)
    state_rv$analysis_source <- "bids_in_app"
    state_rv$prepared_analysis <- new_prepared_analysis(
      analysis_source = "bids_in_app",
      analyze_mode = "end_to_end",
      pipeline_spec = list(dummy = TRUE),
      pipeline_root = "/tmp/plsrri-out",
      pls_options = list(method = "task", nperm = 10L, nboot = 5L),
      pls_input = list(type = "estimates", statistic = "estimate"),
      summary = list(
        n_groups = 2L,
        n_observations = 24L,
        n_conditions = 3L,
        n_features = 200L,
        method_label = "Mean-Centering Task PLS",
        nperm = 10L,
        nboot = 5L
      )
    )

    shiny::testServer(analyze_server, args = list(state_rv = state_rv), {
      summary <- prepared_analysis_review_summary(state_rv$prepared_analysis)
      expect_equal(summary$mode_label, "First-level + PLS")
      expect_equal(summary$source_label, "BIDS workstation pipeline")
      expect_equal(summary$n_observations, 24L)
    })
  })

  it("can summarize attach-mode prepared analysis with a runnable spec", {
    spec <- pls_spec() |>
      add_subjects(list(matrix(rnorm(12), nrow = 4)), groups = 2) |>
      add_conditions(2) |>
      configure(method = "task", nperm = 25, nboot = 10)

    state_rv <- make_test_state_rv(step = 2L, spec = spec)
    state_rv$analysis_source <- "attach"
    state_rv$prepared_analysis <- new_prepared_analysis(
      analysis_source = "attach",
      analyze_mode = "pls_only",
      spec = spec,
      pipeline_root = "/tmp/plsrri-attach",
      analysis_plan = list(input_type = "estimates", statistic = "estimate"),
      pls_input = list(type = "estimates", statistic = "estimate"),
      summary = list(
        n_groups = 1L,
        n_subjects = 2L,
        n_observations = 4L,
        n_conditions = 2L,
        n_features = 3L,
        method_label = "Mean-Centering Task PLS",
        nperm = 25L,
        nboot = 10L
      )
    )

    shiny::testServer(analyze_server, args = list(state_rv = state_rv), {
      summary <- prepared_analysis_review_summary(state_rv$prepared_analysis)
      expect_equal(summary$mode_label, "PLS only")
      expect_equal(summary$source_label, "Attached first-level outputs")
      expect_equal(summary$nperm, 25L)
      expect_equal(summary$nboot, 10L)
    })
  })
})

describe("analyze_server status tracking", {
  it("has total_stages set to 5", {
    shiny::testServer(analyze_server, args = list(state_rv = make_test_state_rv()), {
      expect_equal(local_rv$total_stages, 5)
    })
  })

  it("starts with stage_num 0", {
    shiny::testServer(analyze_server, args = list(state_rv = make_test_state_rv()), {
      expect_equal(local_rv$stage_num, 0)
    })
  })

  it("has empty stage string initially", {
    shiny::testServer(analyze_server, args = list(state_rv = make_test_state_rv()), {
      expect_equal(local_rv$stage, "")
    })
  })

  it("tracks elapsed time as 0 initially", {
    shiny::testServer(analyze_server, args = list(state_rv = make_test_state_rv()), {
      expect_equal(local_rv$elapsed_seconds, 0)
    })
  })

  it("has null start_time initially", {
    shiny::testServer(analyze_server, args = list(state_rv = make_test_state_rv()), {
      expect_null(local_rv$start_time)
    })
  })
})

describe("analyze_server completion handling", {
  it("complete trigger starts at 0", {
    shiny::testServer(analyze_server, args = list(state_rv = make_test_state_rv()), {
      expect_equal(complete_trigger(), 0)
    })
  })

  it("result reactive returns NULL initially", {
    shiny::testServer(analyze_server, args = list(state_rv = make_test_state_rv()), {
      result <- session$returned
      expect_null(result$result())
    })
  })

  it("can manually set result", {
    shiny::testServer(analyze_server, args = list(state_rv = make_test_state_rv()), {
      # Manually set a mock result
      mock_result <- make_mock_pls_result(n_voxels = 100, n_lv = 3)
      local_rv$result <- mock_result

      # Result should be accessible
      expect_s3_class(local_rv$result, "pls_result")
    })
  })

  it("status can be set to complete", {
    shiny::testServer(analyze_server, args = list(state_rv = make_test_state_rv()), {
      local_rv$status <- "complete"
      local_rv$progress <- 100

      expect_equal(local_rv$status, "complete")
      expect_equal(local_rv$progress, 100)
    })
  })
})

describe("analyze_server error handling", {
  it("can set status to error", {
    shiny::testServer(analyze_server, args = list(state_rv = make_test_state_rv()), {
      local_rv$status <- "error"
      local_rv$error_message <- "Test error message"

      expect_equal(local_rv$status, "error")
      expect_equal(local_rv$error_message, "Test error message")
    })
  })

  it("error_message is accessible after error", {
    shiny::testServer(analyze_server, args = list(state_rv = make_test_state_rv()), {
      local_rv$status <- "error"
      local_rv$error_message <- "Analysis failed: insufficient data"

      expect_match(local_rv$error_message, "insufficient data")
    })
  })

  it("status remains error until reset", {
    shiny::testServer(analyze_server, args = list(state_rv = make_test_state_rv()), {
      local_rv$status <- "error"
      expect_equal(local_rv$status, "error")

      # Manual reset
      local_rv$status <- "ready"
      expect_equal(local_rv$status, "ready")
    })
  })
})

describe("analyze_server cancellation", {
  it("can set status to cancelled", {
    shiny::testServer(analyze_server, args = list(state_rv = make_test_state_rv()), {
      local_rv$status <- "cancelled"
      expect_equal(local_rv$status, "cancelled")
    })
  })

  it("cancel button sets status to cancelled when running", {
    shiny::testServer(analyze_server, args = list(state_rv = make_test_state_rv()), {
      # Simulate running state
      local_rv$status <- "running"

      # Click cancel
      session$setInputs(btn_cancel = 1)

      # Status should be cancelled
      expect_equal(local_rv$status, "cancelled")
    })
  })

  it("cancel button does nothing when not running", {
    shiny::testServer(analyze_server, args = list(state_rv = make_test_state_rv()), {
      # Status is ready (not running)
      expect_equal(local_rv$status, "ready")

      # Click cancel
      session$setInputs(btn_cancel = 1)

      # Status should still be ready
      expect_equal(local_rv$status, "ready")
    })
  })
})

describe("analyze_server back button", {
  it("back button resets status to ready", {
    state_rv <- make_test_state_rv(step = 2L)

    shiny::testServer(analyze_server, args = list(state_rv = state_rv), {
      local_rv$status <- "running"
      local_rv$progress <- 50

      # Click back
      session$setInputs(btn_back = 1)

      # Status should reset
      expect_equal(local_rv$status, "ready")
      expect_equal(local_rv$progress, 0)
    })
  })

  it("back button sets step to 1", {
    state_rv <- make_test_state_rv(step = 2L)

    shiny::testServer(analyze_server, args = list(state_rv = state_rv), {
      # Click back
      session$setInputs(btn_back = 1)

      # State should be updated
      expect_equal(state_rv$step, 1L)
    })
  })

  it("back_error button resets from error state", {
    state_rv <- make_test_state_rv(step = 2L, analysis_status = "error")

    shiny::testServer(analyze_server, args = list(state_rv = state_rv), {
      local_rv$status <- "error"
      local_rv$error_message <- "Test error"

      # Click back from error
      session$setInputs(btn_back_error = 1)

      # Should reset
      expect_equal(local_rv$status, "ready")
      expect_equal(local_rv$progress, 0)
      expect_equal(state_rv$step, 1L)
    })
  })
})

describe("analyze_server state synchronization", {
  it("syncs analysis_status to state_rv on cancel", {
    state_rv <- make_test_state_rv(step = 2L, analysis_status = "running")

    shiny::testServer(analyze_server, args = list(state_rv = state_rv), {
      local_rv$status <- "running"

      # Cancel
      session$setInputs(btn_cancel = 1)

      # state_rv should be updated
      expect_equal(state_rv$analysis_status, "ready")
    })
  })

  it("syncs analysis_status to state_rv on back", {
    state_rv <- make_test_state_rv(step = 2L, analysis_status = "running")

    shiny::testServer(analyze_server, args = list(state_rv = state_rv), {
      # Back
      session$setInputs(btn_back = 1)

      # state_rv should be updated
      expect_equal(state_rv$analysis_status, "ready")
    })
  })
})

describe("analyze_server with mock results", {
  it("can store mock pls_result", {
    shiny::testServer(analyze_server, args = list(state_rv = make_test_state_rv()), {
      # Create and store mock result
      mock_result <- make_mock_pls_result(
        n_voxels = 500,
        n_lv = 3,
        include_boot = TRUE,
        include_perm = TRUE
      )
      local_rv$result <- mock_result
      local_rv$status <- "complete"
      local_rv$progress <- 100

      # Verify result properties
      expect_equal(nrow(local_rv$result$u), 500)
      expect_equal(length(local_rv$result$s), 3)
      expect_false(is.null(local_rv$result$boot_result))
      expect_false(is.null(local_rv$result$perm_result))
    })
  })

  it("result reactive reflects stored result", {
    shiny::testServer(analyze_server, args = list(state_rv = make_test_state_rv()), {
      mock_result <- make_mock_pls_result()
      local_rv$result <- mock_result

      # Access via returned reactive
      result <- session$returned
      expect_s3_class(result$result(), "pls_result")
    })
  })
})
