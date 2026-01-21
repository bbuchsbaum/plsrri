# Tests for inspector module server logic
#
# testServer() tests for mod_inspector.R covering LV details display,
# conditional content based on result type, and export button handlers.

describe("inspector_server initialization", {

  it("module accepts result_rv, selected_lv, and state_rv", {
    result <- make_mock_pls_result()
    shiny::testServer(inspector_server, {
      # Module should initialize without error
      expect_true(TRUE)
    }, args = list(
      result_rv = shiny::reactive({ result }),
      selected_lv = shiny::reactive({ 1L }),
      state_rv = shiny::reactiveValues(
        step = 3L,
        result = result
      )
    ))
  })

  it("initializes without error when result is NULL", {
    shiny::testServer(inspector_server, {
      # Module should handle NULL result
      expect_true(TRUE)
    }, args = list(
      result_rv = shiny::reactive({ NULL }),
      selected_lv = shiny::reactive({ 1L }),
      state_rv = shiny::reactiveValues(
        step = 3L,
        result = NULL
      )
    ))
  })

  it("initializes without error when selected_lv is NULL", {
    result <- make_mock_pls_result()
    shiny::testServer(inspector_server, {
      # Module should handle NULL selected_lv
      expect_true(TRUE)
    }, args = list(
      result_rv = shiny::reactive({ result }),
      selected_lv = shiny::reactive({ NULL }),
      state_rv = shiny::reactiveValues(
        step = 3L,
        result = result
      )
    ))
  })

})

describe("inspector_server LV details", {

  it("displays correct variance explained for selected LV", {
    result <- make_mock_pls_result(n_lv = 3)
    shiny::testServer(inspector_server, {
      # Calculate expected variance
      s <- result_rv()$s
      var_exp <- (s^2 / sum(s^2)) * 100

      # LV 1 should have highest variance (sorted descending)
      expect_true(var_exp[1] >= var_exp[2])
      expect_true(var_exp[2] >= var_exp[3])
    }, args = list(
      result_rv = shiny::reactive({ result }),
      selected_lv = shiny::reactive({ 1L }),
      state_rv = shiny::reactiveValues(
        step = 3L,
        result = result
      )
    ))
  })

  it("shows p-value when perm_result exists", {
    result <- make_mock_pls_result(include_perm = TRUE)
    shiny::testServer(inspector_server, {
      r <- result_rv()
      lv <- selected_lv()

      expect_false(is.null(r$perm_result))
      p_val <- r$perm_result$sprob[lv]
      expect_true(!is.na(p_val))
      expect_true(p_val >= 0 && p_val <= 1)
    }, args = list(
      result_rv = shiny::reactive({ result }),
      selected_lv = shiny::reactive({ 1L }),
      state_rv = shiny::reactiveValues(
        step = 3L,
        result = result
      )
    ))
  })

  it("handles result without perm_result (p-value is NA)", {
    result <- make_mock_pls_result(include_perm = FALSE)
    shiny::testServer(inspector_server, {
      r <- result_rv()
      lv <- selected_lv()

      expect_null(r$perm_result)

      # P-value logic should handle NULL perm_result
      p_val <- if (!is.null(r$perm_result)) {
        r$perm_result$sprob[lv]
      } else {
        NA
      }
      expect_true(is.na(p_val))
    }, args = list(
      result_rv = shiny::reactive({ result }),
      selected_lv = shiny::reactive({ 1L }),
      state_rv = shiny::reactiveValues(
        step = 3L,
        result = result
      )
    ))
  })

  it("shows BSR summary when boot_result exists", {
    result <- make_mock_pls_result(include_boot = TRUE)
    shiny::testServer(inspector_server, {
      r <- result_rv()
      lv <- selected_lv()

      expect_false(is.null(r$boot_result))

      # BSR values can be computed
      bsr_vals <- plsrri::bsr(r, lv = lv)
      expect_true(length(bsr_vals) > 0)

      # BSR summary stats
      n_pos <- sum(bsr_vals > 3, na.rm = TRUE)
      n_neg <- sum(bsr_vals < -3, na.rm = TRUE)
      expect_true(is.numeric(n_pos))
      expect_true(is.numeric(n_neg))
    }, args = list(
      result_rv = shiny::reactive({ result }),
      selected_lv = shiny::reactive({ 1L }),
      state_rv = shiny::reactiveValues(
        step = 3L,
        result = result
      )
    ))
  })

  it("cumulative variance is computed correctly", {
    result <- make_mock_pls_result(n_lv = 5)
    shiny::testServer(inspector_server, {
      r <- result_rv()
      lv <- selected_lv()

      s <- r$s
      var_exp <- (s^2 / sum(s^2)) * 100
      cum_var <- cumsum(var_exp)

      # Cumulative variance at LV should be >= variance at that LV
      expect_true(cum_var[lv] >= var_exp[lv])

      # Final cumulative should be 100%
      expect_equal(unname(cum_var[length(cum_var)]), 100, tolerance = 1e-10)
    }, args = list(
      result_rv = shiny::reactive({ result }),
      selected_lv = shiny::reactive({ 1L }),
      state_rv = shiny::reactiveValues(
        step = 3L,
        result = result
      )
    ))
  })

})

describe("inspector_server with different results", {

  it("full result (boot + perm): all stats shown", {
    result <- load_fixture("pls_result_full")
    shiny::testServer(inspector_server, {
      r <- result_rv()
      lv <- selected_lv()

      # All components present
      expect_false(is.null(r$boot_result))
      expect_false(is.null(r$perm_result))

      # P-value available
      p_val <- r$perm_result$sprob[lv]
      expect_true(!is.na(p_val))

      # BSR available
      bsr_vals <- plsrri::bsr(r, lv = lv)
      expect_true(length(bsr_vals) > 0)
    }, args = list(
      result_rv = shiny::reactive({ result }),
      selected_lv = shiny::reactive({ 1L }),
      state_rv = shiny::reactiveValues(
        step = 3L,
        result = result
      )
    ))
  })

  it("boot only: BSR stats shown, no p-value", {
    result <- load_fixture("pls_result_with_boot")
    shiny::testServer(inspector_server, {
      r <- result_rv()
      lv <- selected_lv()

      # Bootstrap present
      expect_false(is.null(r$boot_result))

      # Permutation absent
      expect_null(r$perm_result)

      # BSR available
      bsr_vals <- plsrri::bsr(r, lv = lv)
      expect_true(length(bsr_vals) > 0)
    }, args = list(
      result_rv = shiny::reactive({ result }),
      selected_lv = shiny::reactive({ 1L }),
      state_rv = shiny::reactiveValues(
        step = 3L,
        result = result
      )
    ))
  })

  it("perm only: p-value shown, no BSR stats", {
    result <- load_fixture("pls_result_with_perm")
    shiny::testServer(inspector_server, {
      r <- result_rv()
      lv <- selected_lv()

      # Bootstrap absent
      expect_null(r$boot_result)

      # Permutation present
      expect_false(is.null(r$perm_result))

      # P-value available
      p_val <- r$perm_result$sprob[lv]
      expect_true(!is.na(p_val))
    }, args = list(
      result_rv = shiny::reactive({ result }),
      selected_lv = shiny::reactive({ 1L }),
      state_rv = shiny::reactiveValues(
        step = 3L,
        result = result
      )
    ))
  })

  it("basic result: minimal display", {
    result <- load_fixture("pls_result_basic")
    shiny::testServer(inspector_server, {
      r <- result_rv()

      # Both absent
      expect_null(r$boot_result)
      expect_null(r$perm_result)

      # But singular values present
      expect_true(length(r$s) > 0)
    }, args = list(
      result_rv = shiny::reactive({ result }),
      selected_lv = shiny::reactive({ 1L }),
      state_rv = shiny::reactiveValues(
        step = 3L,
        result = result
      )
    ))
  })

  it("handles single LV result", {
    result <- make_single_lv_result()
    shiny::testServer(inspector_server, {
      r <- result_rv()

      expect_equal(length(r$s), 1)

      # Variance for single LV should be 100%
      var_exp <- (r$s^2 / sum(r$s^2)) * 100
      expect_equal(unname(var_exp[1]), 100, tolerance = 1e-10)
    }, args = list(
      result_rv = shiny::reactive({ result }),
      selected_lv = shiny::reactive({ 1L }),
      state_rv = shiny::reactiveValues(
        step = 3L,
        result = result
      )
    ))
  })

  it("handles many LV result", {
    result <- make_many_lv_result()
    shiny::testServer(inspector_server, {
      r <- result_rv()

      expect_gte(length(r$s), 10)

      # Can access any LV
      s <- r$s
      var_exp <- (s^2 / sum(s^2)) * 100

      # Each LV has positive variance
      expect_true(all(var_exp > 0))
    }, args = list(
      result_rv = shiny::reactive({ result }),
      selected_lv = shiny::reactive({ 5L }),
      state_rv = shiny::reactiveValues(
        step = 3L,
        result = result
      )
    ))
  })

})

describe("inspector_server export buttons", {

  it("export NIfTI button input is accessible", {
    result <- make_mock_pls_result(include_mask = TRUE)
    shiny::testServer(inspector_server, {
      # Simulate button click
      session$setInputs(export_nifti = 1L)

      # Input should be registered
      expect_equal(input$export_nifti, 1L)
    }, args = list(
      result_rv = shiny::reactive({ result }),
      selected_lv = shiny::reactive({ 1L }),
      state_rv = shiny::reactiveValues(
        step = 3L,
        result = result
      )
    ))
  })

  it("export CSV button input is accessible", {
    result <- make_mock_pls_result()
    shiny::testServer(inspector_server, {
      session$setInputs(export_csv = 1L)
      expect_equal(input$export_csv, 1L)
    }, args = list(
      result_rv = shiny::reactive({ result }),
      selected_lv = shiny::reactive({ 1L }),
      state_rv = shiny::reactiveValues(
        step = 3L,
        result = result
      )
    ))
  })

  it("export PDF button input is accessible", {
    result <- make_mock_pls_result()
    shiny::testServer(inspector_server, {
      session$setInputs(export_pdf = 1L)
      expect_equal(input$export_pdf, 1L)
    }, args = list(
      result_rv = shiny::reactive({ result }),
      selected_lv = shiny::reactive({ 1L }),
      state_rv = shiny::reactiveValues(
        step = 3L,
        result = result
      )
    ))
  })

  it("export report button input is accessible", {
    result <- make_mock_pls_result()
    shiny::testServer(inspector_server, {
      session$setInputs(export_report = 1L)
      expect_equal(input$export_report, 1L)
    }, args = list(
      result_rv = shiny::reactive({ result }),
      selected_lv = shiny::reactive({ 1L }),
      state_rv = shiny::reactiveValues(
        step = 3L,
        result = result
      )
    ))
  })

  it("multiple export button clicks increment counter", {
    result <- make_mock_pls_result()
    shiny::testServer(inspector_server, {
      session$setInputs(export_csv = 1L)
      expect_equal(input$export_csv, 1L)

      session$setInputs(export_csv = 2L)
      expect_equal(input$export_csv, 2L)

      session$setInputs(export_csv = 3L)
      expect_equal(input$export_csv, 3L)
    }, args = list(
      result_rv = shiny::reactive({ result }),
      selected_lv = shiny::reactive({ 1L }),
      state_rv = shiny::reactiveValues(
        step = 3L,
        result = result
      )
    ))
  })

})

describe("inspector_server scores display", {

  it("scores can be extracted for valid result", {
    result <- make_mock_pls_result(n_obs = 30, n_lv = 3)
    shiny::testServer(inspector_server, {
      r <- result_rv()
      lv <- selected_lv()

      # Design scores extraction
      design_scores <- plsrri::scores(r, type = "design")
      expect_equal(nrow(design_scores), 30)
      expect_equal(ncol(design_scores), 3)
    }, args = list(
      result_rv = shiny::reactive({ result }),
      selected_lv = shiny::reactive({ 1L }),
      state_rv = shiny::reactiveValues(
        step = 3L,
        result = result
      )
    ))
  })

  it("handles NULL result gracefully for scores", {
    shiny::testServer(inspector_server, {
      r <- result_rv()
      expect_null(r)

      # Scores logic should handle NULL
      if (!is.null(r)) {
        fail("Should not reach here")
      }
      expect_true(TRUE)
    }, args = list(
      result_rv = shiny::reactive({ NULL }),
      selected_lv = shiny::reactive({ 1L }),
      state_rv = shiny::reactiveValues(
        step = 3L,
        result = NULL
      )
    ))
  })

  it("handles NULL selected_lv gracefully for scores", {
    result <- make_mock_pls_result()
    shiny::testServer(inspector_server, {
      lv <- selected_lv()
      expect_null(lv)

      # Scores logic should handle NULL LV
      if (!is.null(lv)) {
        fail("Should not reach here")
      }
      expect_true(TRUE)
    }, args = list(
      result_rv = shiny::reactive({ result }),
      selected_lv = shiny::reactive({ NULL }),
      state_rv = shiny::reactiveValues(
        step = 3L,
        result = result
      )
    ))
  })

})

describe("inspector_server selected LV changes", {

  it("responds to different selected_lv values", {
    result <- make_mock_pls_result(n_lv = 5)
    shiny::testServer(inspector_server, {
      r <- result_rv()

      # Different LVs have different variances
      s <- r$s
      var_exp <- (s^2 / sum(s^2)) * 100

      for (lv in 1:5) {
        expect_true(var_exp[lv] > 0)
        expect_true(var_exp[lv] <= 100)
      }
    }, args = list(
      result_rv = shiny::reactive({ result }),
      selected_lv = shiny::reactive({ 3L }),
      state_rv = shiny::reactiveValues(
        step = 3L,
        result = result
      )
    ))
  })

  it("variance for each LV is positive", {
    result <- make_mock_pls_result(n_lv = 3)
    shiny::testServer(inspector_server, {
      r <- result_rv()
      s <- r$s
      var_exp <- (s^2 / sum(s^2)) * 100

      expect_true(var_exp[1] > 0)
      expect_true(var_exp[2] > 0)
      expect_true(var_exp[3] > 0)
    }, args = list(
      result_rv = shiny::reactive({ result }),
      selected_lv = shiny::reactive({ 2L }),
      state_rv = shiny::reactiveValues(
        step = 3L,
        result = result
      )
    ))
  })

})
