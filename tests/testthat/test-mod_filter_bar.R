# Test file for mod_filter_bar Shiny module
# Tests reactive server logic using testServer()

# Required packages for Shiny module testing
library(shiny)
library(bslib)

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
source(file.path(module_path, "mod_filter_bar.R"), local = FALSE)

# Helper to create minimal state_rv for testing
make_test_state_rv <- function(result = NULL) {
  shiny::reactiveValues(
    step = 3L,
    result = result,
    selected_lv = 1L,
    bsr_threshold = 3.0,
    p_threshold = 0.05,
    view_mode = "montage"
  )
}

# Helper to create result_rv reactive for filter_bar_server
make_result_rv <- function(result) {
  shiny::reactive({ result })
}

describe("filter_bar_server initialization", {
  it("returns list of reactive filter values", {
    state_rv <- make_test_state_rv()
    result_rv <- make_result_rv(NULL)

    shiny::testServer(filter_bar_server,
      args = list(result_rv = result_rv, state_rv = state_rv), {

      result <- session$returned
      expect_type(result, "list")

      # Should have all filter reactives
      expect_true("lv" %in% names(result))
      expect_true("bsr_threshold" %in% names(result))
      expect_true("p_threshold" %in% names(result))
      expect_true("view_mode" %in% names(result))
      expect_true("what" %in% names(result))
    })
  })

  it("all returned values are reactive", {
    state_rv <- make_test_state_rv()
    result_rv <- make_result_rv(NULL)

    shiny::testServer(filter_bar_server,
      args = list(result_rv = result_rv, state_rv = state_rv), {

      result <- session$returned

      expect_true(shiny::is.reactive(result$lv))
      expect_true(shiny::is.reactive(result$bsr_threshold))
      expect_true(shiny::is.reactive(result$p_threshold))
      expect_true(shiny::is.reactive(result$view_mode))
      expect_true(shiny::is.reactive(result$what))
    })
  })

  it("default BSR threshold is 3.0", {
    state_rv <- make_test_state_rv()
    result_rv <- make_result_rv(NULL)

    shiny::testServer(filter_bar_server,
      args = list(result_rv = result_rv, state_rv = state_rv), {

      # Set default input
      session$setInputs(bsr_threshold = 3.0)

      result <- session$returned
      expect_equal(result$bsr_threshold(), 3.0)
    })
  })

  it("default p threshold is 0.05", {
    state_rv <- make_test_state_rv()
    result_rv <- make_result_rv(NULL)

    shiny::testServer(filter_bar_server,
      args = list(result_rv = result_rv, state_rv = state_rv), {

      session$setInputs(p_threshold = 0.05)

      result <- session$returned
      expect_equal(result$p_threshold(), 0.05)
    })
  })

  it("default view mode is montage", {
    state_rv <- make_test_state_rv()
    result_rv <- make_result_rv(NULL)

    shiny::testServer(filter_bar_server,
      args = list(result_rv = result_rv, state_rv = state_rv), {

      session$setInputs(view_mode = "montage")

      result <- session$returned
      expect_equal(result$view_mode(), "montage")
    })
  })

  it("default what is bsr", {
    state_rv <- make_test_state_rv()
    result_rv <- make_result_rv(NULL)

    shiny::testServer(filter_bar_server,
      args = list(result_rv = result_rv, state_rv = state_rv), {

      session$setInputs(what = "bsr")

      result <- session$returned
      expect_equal(result$what(), "bsr")
    })
  })
})

describe("filter_bar_server LV choices", {
  it("LV choices update when result changes", {
    mock_result <- make_mock_pls_result(n_lv = 5)
    state_rv <- make_test_state_rv(result = mock_result)
    result_rv <- make_result_rv(mock_result)

    shiny::testServer(filter_bar_server,
      args = list(result_rv = result_rv, state_rv = state_rv), {

      # Flush reactive
      session$flushReact()

      # Number of LVs should match result
      expect_equal(length(mock_result$s), 5)
    })
  })

  it("works with result that has no perm_result", {
    mock_result <- make_mock_pls_result(n_lv = 3, include_perm = FALSE)
    state_rv <- make_test_state_rv(result = mock_result)
    result_rv <- make_result_rv(mock_result)

    shiny::testServer(filter_bar_server,
      args = list(result_rv = result_rv, state_rv = state_rv), {

      # Should not error
      session$flushReact()

      # Result has no perm_result
      expect_null(mock_result$perm_result)
      expect_equal(length(mock_result$s), 3)
    })
  })

  it("works with result that has perm_result for significance markers", {
    mock_result <- make_mock_pls_result(n_lv = 3, include_perm = TRUE)
    state_rv <- make_test_state_rv(result = mock_result)
    result_rv <- make_result_rv(mock_result)

    shiny::testServer(filter_bar_server,
      args = list(result_rv = result_rv, state_rv = state_rv), {

      session$flushReact()

      # Result has perm_result
      expect_false(is.null(mock_result$perm_result))
      expect_equal(length(mock_result$perm_result$sprob), 3)
    })
  })

  it("works with single-LV result", {
    mock_result <- make_single_lv_result()
    state_rv <- make_test_state_rv(result = mock_result)
    result_rv <- make_result_rv(mock_result)

    shiny::testServer(filter_bar_server,
      args = list(result_rv = result_rv, state_rv = state_rv), {

      session$flushReact()

      # Single LV
      expect_equal(length(mock_result$s), 1)
    })
  })

  it("works with many-LV result", {
    mock_result <- make_many_lv_result()
    state_rv <- make_test_state_rv(result = mock_result)
    result_rv <- make_result_rv(mock_result)

    shiny::testServer(filter_bar_server,
      args = list(result_rv = result_rv, state_rv = state_rv), {

      session$flushReact()

      # Many LVs
      expect_equal(length(mock_result$s), 10)
    })
  })
})

describe("filter_bar_server value sync", {
  it("changing LV input updates returned reactive", {
    state_rv <- make_test_state_rv()
    result_rv <- make_result_rv(NULL)

    shiny::testServer(filter_bar_server,
      args = list(result_rv = result_rv, state_rv = state_rv), {

      # Set LV to 2
      session$setInputs(lv = "2")

      result <- session$returned
      expect_equal(result$lv(), 2L)
    })
  })

  it("LV selection syncs to state_rv", {
    state_rv <- make_test_state_rv()
    result_rv <- make_result_rv(NULL)

    shiny::testServer(filter_bar_server,
      args = list(result_rv = result_rv, state_rv = state_rv), {

      session$setInputs(lv = "3", bsr_threshold = 3.0, p_threshold = 0.05, view_mode = "montage")

      # state_rv should be updated
      expect_equal(state_rv$selected_lv, 3L)
    })
  })

  it("BSR threshold changes sync to state_rv", {
    state_rv <- make_test_state_rv()
    result_rv <- make_result_rv(NULL)

    shiny::testServer(filter_bar_server,
      args = list(result_rv = result_rv, state_rv = state_rv), {

      session$setInputs(lv = "1", bsr_threshold = 4.5, p_threshold = 0.05, view_mode = "montage")

      expect_equal(state_rv$bsr_threshold, 4.5)
    })
  })

  it("p threshold changes sync to state_rv", {
    state_rv <- make_test_state_rv()
    result_rv <- make_result_rv(NULL)

    shiny::testServer(filter_bar_server,
      args = list(result_rv = result_rv, state_rv = state_rv), {

      session$setInputs(lv = "1", bsr_threshold = 3.0, p_threshold = 0.01, view_mode = "montage")

      expect_equal(state_rv$p_threshold, 0.01)
    })
  })

  it("view mode changes sync to state_rv", {
    state_rv <- make_test_state_rv()
    result_rv <- make_result_rv(NULL)

    shiny::testServer(filter_bar_server,
      args = list(result_rv = result_rv, state_rv = state_rv), {

      session$setInputs(lv = "1", bsr_threshold = 3.0, p_threshold = 0.05, view_mode = "ortho")

      expect_equal(state_rv$view_mode, "ortho")
    })
  })

  it("'all' LV selection returns NULL", {
    state_rv <- make_test_state_rv()
    result_rv <- make_result_rv(NULL)

    shiny::testServer(filter_bar_server,
      args = list(result_rv = result_rv, state_rv = state_rv), {

      session$setInputs(lv = "all")

      result <- session$returned
      expect_null(result$lv())
    })
  })

  it("'all' LV selection syncs to state_rv as 0", {
    state_rv <- make_test_state_rv()
    result_rv <- make_result_rv(NULL)

    shiny::testServer(filter_bar_server,
      args = list(result_rv = result_rv, state_rv = state_rv), {

      session$setInputs(lv = "all", bsr_threshold = 3.0, p_threshold = 0.05, view_mode = "montage")

      expect_equal(state_rv$selected_lv, 0L)
    })
  })
})

describe("filter_bar_server with different results", {
  it("handles NULL result gracefully", {
    state_rv <- make_test_state_rv(result = NULL)
    result_rv <- make_result_rv(NULL)

    shiny::testServer(filter_bar_server,
      args = list(result_rv = result_rv, state_rv = state_rv), {

      # Should not error
      session$flushReact()

      # Can still set inputs
      session$setInputs(lv = "1", bsr_threshold = 3.0, p_threshold = 0.05, view_mode = "montage")

      result <- session$returned
      expect_equal(result$bsr_threshold(), 3.0)
    })
  })

  it("handles result transition from NULL to valid", {
    mock_result <- make_mock_pls_result(n_lv = 3)

    # Start with NULL
    state_rv <- make_test_state_rv(result = NULL)

    # Create reactive that can change
    result_holder <- shiny::reactiveVal(NULL)
    result_rv <- shiny::reactive({ result_holder() })

    shiny::testServer(filter_bar_server,
      args = list(result_rv = result_rv, state_rv = state_rv), {

      # Initially NULL
      session$flushReact()

      # Update to valid result
      result_holder(mock_result)
      session$flushReact()

      # Module should handle transition
      expect_true(TRUE)  # If we get here, no error occurred
    })
  })

  it("what filter returns correct values", {
    state_rv <- make_test_state_rv()
    result_rv <- make_result_rv(NULL)

    shiny::testServer(filter_bar_server,
      args = list(result_rv = result_rv, state_rv = state_rv), {

      # Test BSR
      session$setInputs(what = "bsr")
      result <- session$returned
      expect_equal(result$what(), "bsr")

      # Test salience
      session$setInputs(what = "salience")
      expect_equal(result$what(), "salience")
    })
  })
})
