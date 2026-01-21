# Tests for explore module server logic
#
# testServer() tests for mod_explore.R covering LV selection,
# result handling, and variance calculations.
#
# Note: explore_server orchestrates child modules (filter_bar, brain_viewer, inspector).
# These tests focus on the parent module's own reactive logic. Child modules
# have their own test files.

describe("explore_server initialization", {

  it("module initializes with selected_lv = 1", {
    shiny::testServer(explore_server, {
      # Local state should start at 1
      expect_equal(local_rv$selected_lv, 1L)
    }, args = list(
      state_rv = shiny::reactiveValues(
        step = 3L,
        max_step = 3L,
        result = make_mock_pls_result(),
        selected_lv = 1L,
        bsr_threshold = 3.0,
        p_threshold = 0.05,
        view_mode = "montage"
      )
    ))
  })

  it("module initializes without error when result is NULL", {
    shiny::testServer(explore_server, {
      expect_null(result_rv())
      expect_equal(local_rv$selected_lv, 1L)
    }, args = list(
      state_rv = shiny::reactiveValues(
        step = 3L,
        max_step = 3L,
        result = NULL,
        selected_lv = 1L,
        bsr_threshold = 3.0,
        p_threshold = 0.05,
        view_mode = "montage"
      )
    ))
  })

  it("result_rv reactive returns state result", {
    result <- make_mock_pls_result()
    shiny::testServer(explore_server, {
      returned_result <- result_rv()
      expect_s3_class(returned_result, "pls_result")
      expect_equal(returned_result$s, result$s)
    }, args = list(
      state_rv = shiny::reactiveValues(
        step = 3L,
        max_step = 3L,
        result = result,
        selected_lv = 1L,
        bsr_threshold = 3.0,
        p_threshold = 0.05,
        view_mode = "montage"
      )
    ))
  })

})

describe("explore_server LV selection", {

  it("local state can be modified directly", {
    shiny::testServer(explore_server, {
      # Initial state
      expect_equal(local_rv$selected_lv, 1L)

      # Direct modification
      local_rv$selected_lv <- 2L
      expect_equal(local_rv$selected_lv, 2L)
    }, args = list(
      state_rv = shiny::reactiveValues(
        step = 3L,
        max_step = 3L,
        result = make_mock_pls_result(n_lv = 3),
        selected_lv = 1L,
        bsr_threshold = 3.0,
        p_threshold = 0.05,
        view_mode = "montage"
      )
    ))
  })

  it("selected_lv reactive reflects local state changes", {
    shiny::testServer(explore_server, {
      # Initial value
      expect_equal(selected_lv(), 1L)

      # Update local state
      local_rv$selected_lv <- 3L

      # Reactive should reflect change
      expect_equal(selected_lv(), 3L)
    }, args = list(
      state_rv = shiny::reactiveValues(
        step = 3L,
        max_step = 3L,
        result = make_mock_pls_result(n_lv = 3),
        selected_lv = 1L,
        bsr_threshold = 3.0,
        p_threshold = 0.05,
        view_mode = "montage"
      )
    ))
  })

  it("LV selection can be set to any valid LV index", {
    result <- make_mock_pls_result(n_lv = 5)
    shiny::testServer(explore_server, {
      for (lv in 1:5) {
        local_rv$selected_lv <- lv
        expect_equal(selected_lv(), lv)
      }
    }, args = list(
      state_rv = shiny::reactiveValues(
        step = 3L,
        max_step = 3L,
        result = result,
        selected_lv = 1L,
        bsr_threshold = 3.0,
        p_threshold = 0.05,
        view_mode = "montage"
      )
    ))
  })

})

describe("explore_server with different results", {

  it("handles result with perm_result (shows significance)", {
    result <- make_mock_pls_result(include_perm = TRUE)
    shiny::testServer(explore_server, {
      returned_result <- result_rv()

      expect_false(is.null(returned_result$perm_result))
      expect_equal(length(returned_result$perm_result$sprob), length(returned_result$s))
    }, args = list(
      state_rv = shiny::reactiveValues(
        step = 3L,
        max_step = 3L,
        result = result,
        selected_lv = 1L,
        bsr_threshold = 3.0,
        p_threshold = 0.05,
        view_mode = "montage"
      )
    ))
  })

  it("handles result without perm_result", {
    result <- make_mock_pls_result(include_perm = FALSE)
    shiny::testServer(explore_server, {
      returned_result <- result_rv()

      expect_null(returned_result$perm_result)
      # Should still work without error
      expect_equal(local_rv$selected_lv, 1L)
    }, args = list(
      state_rv = shiny::reactiveValues(
        step = 3L,
        max_step = 3L,
        result = result,
        selected_lv = 1L,
        bsr_threshold = 3.0,
        p_threshold = 0.05,
        view_mode = "montage"
      )
    ))
  })

  it("handles result with many LVs", {
    result <- make_many_lv_result()
    shiny::testServer(explore_server, {
      returned_result <- result_rv()

      expect_gte(length(returned_result$s), 10)

      # Can select any LV via local state
      local_rv$selected_lv <- 10L
      expect_equal(selected_lv(), 10L)
    }, args = list(
      state_rv = shiny::reactiveValues(
        step = 3L,
        max_step = 3L,
        result = result,
        selected_lv = 1L,
        bsr_threshold = 3.0,
        p_threshold = 0.05,
        view_mode = "montage"
      )
    ))
  })

  it("handles result with only 1 LV", {
    result <- make_single_lv_result()
    shiny::testServer(explore_server, {
      returned_result <- result_rv()

      expect_equal(length(returned_result$s), 1)
      expect_equal(local_rv$selected_lv, 1L)
    }, args = list(
      state_rv = shiny::reactiveValues(
        step = 3L,
        max_step = 3L,
        result = result,
        selected_lv = 1L,
        bsr_threshold = 3.0,
        p_threshold = 0.05,
        view_mode = "montage"
      )
    ))
  })

  it("handles full result (boot + perm + mask)", {
    result <- load_fixture("pls_result_full")
    shiny::testServer(explore_server, {
      returned_result <- result_rv()

      expect_false(is.null(returned_result$boot_result))
      expect_false(is.null(returned_result$perm_result))
      expect_false(is.null(returned_result$mask))
    }, args = list(
      state_rv = shiny::reactiveValues(
        step = 3L,
        max_step = 3L,
        result = result,
        selected_lv = 1L,
        bsr_threshold = 3.0,
        p_threshold = 0.05,
        view_mode = "montage"
      )
    ))
  })

})

describe("explore_server variance calculation", {

  it("variance explained percentages sum to 100", {
    result <- make_mock_pls_result()
    shiny::testServer(explore_server, {
      returned_result <- result_rv()

      # Calculate variance explained as module does
      var_exp <- (returned_result$s^2 / sum(returned_result$s^2)) * 100

      expect_equal(sum(var_exp), 100, tolerance = 1e-10)
    }, args = list(
      state_rv = shiny::reactiveValues(
        step = 3L,
        max_step = 3L,
        result = result,
        selected_lv = 1L,
        bsr_threshold = 3.0,
        p_threshold = 0.05,
        view_mode = "montage"
      )
    ))
  })

  it("variance values are positive and in descending order", {
    result <- make_mock_pls_result()
    shiny::testServer(explore_server, {
      returned_result <- result_rv()

      # Singular values should be positive
      expect_true(all(returned_result$s > 0))

      # Singular values should be in descending order
      expect_equal(returned_result$s, sort(returned_result$s, decreasing = TRUE))
    }, args = list(
      state_rv = shiny::reactiveValues(
        step = 3L,
        max_step = 3L,
        result = result,
        selected_lv = 1L,
        bsr_threshold = 3.0,
        p_threshold = 0.05,
        view_mode = "montage"
      )
    ))
  })

  it("cumulative variance is monotonically increasing", {
    result <- make_mock_pls_result()
    shiny::testServer(explore_server, {
      returned_result <- result_rv()

      var_exp <- (returned_result$s^2 / sum(returned_result$s^2)) * 100
      cum_var <- cumsum(var_exp)

      # Cumulative should increase
      for (i in 2:length(cum_var)) {
        expect_gte(cum_var[i], cum_var[i - 1])
      }

      # Final cumulative should be 100
      expect_equal(unname(cum_var[length(cum_var)]), 100, tolerance = 1e-10)
    }, args = list(
      state_rv = shiny::reactiveValues(
        step = 3L,
        max_step = 3L,
        result = result,
        selected_lv = 1L,
        bsr_threshold = 3.0,
        p_threshold = 0.05,
        view_mode = "montage"
      )
    ))
  })

  it("individual LV variance can be computed correctly", {
    result <- make_mock_pls_result(n_lv = 3)
    shiny::testServer(explore_server, {
      returned_result <- result_rv()
      lv <- local_rv$selected_lv

      # Get variance for selected LV
      var_exp <- (returned_result$s^2 / sum(returned_result$s^2)) * 100
      lv_variance <- var_exp[lv]

      expect_true(lv_variance > 0)
      expect_true(lv_variance <= 100)
    }, args = list(
      state_rv = shiny::reactiveValues(
        step = 3L,
        max_step = 3L,
        result = result,
        selected_lv = 1L,
        bsr_threshold = 3.0,
        p_threshold = 0.05,
        view_mode = "montage"
      )
    ))
  })

})

describe("explore_server result structure validation", {

  it("result has required components for LV list rendering", {
    result <- make_mock_pls_result(include_perm = TRUE)
    shiny::testServer(explore_server, {
      returned_result <- result_rv()

      # Check required components for lv_list rendering
      expect_true(!is.null(returned_result$s))
      n_lv <- length(returned_result$s)
      expect_true(n_lv > 0)

      # P-values if available
      if (!is.null(returned_result$perm_result)) {
        expect_equal(length(returned_result$perm_result$sprob), n_lv)
      }
    }, args = list(
      state_rv = shiny::reactiveValues(
        step = 3L,
        max_step = 3L,
        result = result,
        selected_lv = 1L,
        bsr_threshold = 3.0,
        p_threshold = 0.05,
        view_mode = "montage"
      )
    ))
  })

  it("result u matrix dimensions match n_lv", {
    result <- make_mock_pls_result(n_voxels = 100, n_lv = 4)
    shiny::testServer(explore_server, {
      returned_result <- result_rv()

      expect_equal(nrow(returned_result$u), 100)
      expect_equal(ncol(returned_result$u), 4)
      expect_equal(length(returned_result$s), 4)
    }, args = list(
      state_rv = shiny::reactiveValues(
        step = 3L,
        max_step = 3L,
        result = result,
        selected_lv = 1L,
        bsr_threshold = 3.0,
        p_threshold = 0.05,
        view_mode = "montage"
      )
    ))
  })

  it("scores matrices have correct dimensions", {
    result <- make_mock_pls_result(n_obs = 25, n_lv = 3)
    shiny::testServer(explore_server, {
      returned_result <- result_rv()

      # Brain scores
      expect_equal(nrow(returned_result$usc), 25)
      expect_equal(ncol(returned_result$usc), 3)

      # Design scores
      expect_equal(nrow(returned_result$vsc), 25)
      expect_equal(ncol(returned_result$vsc), 3)
    }, args = list(
      state_rv = shiny::reactiveValues(
        step = 3L,
        max_step = 3L,
        result = result,
        selected_lv = 1L,
        bsr_threshold = 3.0,
        p_threshold = 0.05,
        view_mode = "montage"
      )
    ))
  })

})
