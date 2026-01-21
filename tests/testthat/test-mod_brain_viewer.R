# Tests for brain viewer module server logic
#
# testServer() tests for mod_brain_viewer.R covering view toggle,
# filter integration, result handling, and click events.

describe("brain_viewer_server initialization", {

  it("initializes with default view_mode as montage", {
    shiny::testServer(brain_viewer_server, {
      expect_equal(local_rv$view_mode, "montage")
    }, args = list(
      result_rv = shiny::reactive({ make_mock_pls_result() }),
      filters = list(
        lv = shiny::reactive({ 1L }),
        bsr_threshold = shiny::reactive({ 3.0 }),
        p_threshold = shiny::reactive({ 0.05 }),
        view_mode = shiny::reactive({ "montage" }),
        what = shiny::reactive({ "bsr" })
      )
    ))
  })

  it("initializes with selected_coord as NULL", {
    shiny::testServer(brain_viewer_server, {
      expect_null(local_rv$selected_coord)
    }, args = list(
      result_rv = shiny::reactive({ make_mock_pls_result() }),
      filters = list(
        lv = shiny::reactive({ 1L }),
        bsr_threshold = shiny::reactive({ 3.0 }),
        p_threshold = shiny::reactive({ 0.05 }),
        view_mode = shiny::reactive({ "montage" }),
        what = shiny::reactive({ "bsr" })
      )
    ))
  })

  it("returns reactive with coord and view_mode", {
    shiny::testServer(brain_viewer_server, {
      returned <- session$getReturned()
      returned_val <- returned()

      expect_true("coord" %in% names(returned_val))
      expect_true("view_mode" %in% names(returned_val))
      expect_equal(returned_val$view_mode, "montage")
    }, args = list(
      result_rv = shiny::reactive({ make_mock_pls_result() }),
      filters = list(
        lv = shiny::reactive({ 1L }),
        bsr_threshold = shiny::reactive({ 3.0 }),
        p_threshold = shiny::reactive({ 0.05 }),
        view_mode = shiny::reactive({ "montage" }),
        what = shiny::reactive({ "bsr" })
      )
    ))
  })

})

describe("brain_viewer_server view toggle", {

  it("clicking montage button sets view_mode to montage", {
    shiny::testServer(brain_viewer_server, {
      # Start in ortho mode
      local_rv$view_mode <- "ortho"

      # Simulate montage button click
      session$setInputs(btn_montage = 1L)

      expect_equal(local_rv$view_mode, "montage")
    }, args = list(
      result_rv = shiny::reactive({ make_mock_pls_result() }),
      filters = list(
        lv = shiny::reactive({ 1L }),
        bsr_threshold = shiny::reactive({ 3.0 }),
        p_threshold = shiny::reactive({ 0.05 }),
        view_mode = shiny::reactive({ NULL }),
        what = shiny::reactive({ "bsr" })
      )
    ))
  })

  it("clicking ortho button sets view_mode to ortho", {
    shiny::testServer(brain_viewer_server, {
      # Start in montage mode
      expect_equal(local_rv$view_mode, "montage")

      # Simulate ortho button click
      session$setInputs(btn_ortho = 1L)

      expect_equal(local_rv$view_mode, "ortho")
    }, args = list(
      result_rv = shiny::reactive({ make_mock_pls_result() }),
      filters = list(
        lv = shiny::reactive({ 1L }),
        bsr_threshold = shiny::reactive({ 3.0 }),
        p_threshold = shiny::reactive({ 0.05 }),
        view_mode = shiny::reactive({ NULL }),
        what = shiny::reactive({ "bsr" })
      )
    ))
  })

  it("view mode can be toggled back and forth", {
    shiny::testServer(brain_viewer_server, {
      expect_equal(local_rv$view_mode, "montage")

      session$setInputs(btn_ortho = 1L)
      expect_equal(local_rv$view_mode, "ortho")

      session$setInputs(btn_montage = 1L)
      expect_equal(local_rv$view_mode, "montage")

      session$setInputs(btn_ortho = 2L)
      expect_equal(local_rv$view_mode, "ortho")
    }, args = list(
      result_rv = shiny::reactive({ make_mock_pls_result() }),
      filters = list(
        lv = shiny::reactive({ 1L }),
        bsr_threshold = shiny::reactive({ 3.0 }),
        p_threshold = shiny::reactive({ 0.05 }),
        view_mode = shiny::reactive({ NULL }),
        what = shiny::reactive({ "bsr" })
      )
    ))
  })

  it("returned reactive reflects view_mode changes", {
    shiny::testServer(brain_viewer_server, {
      returned <- session$getReturned()

      expect_equal(returned()$view_mode, "montage")

      session$setInputs(btn_ortho = 1L)
      expect_equal(returned()$view_mode, "ortho")

      session$setInputs(btn_montage = 1L)
      expect_equal(returned()$view_mode, "montage")
    }, args = list(
      result_rv = shiny::reactive({ make_mock_pls_result() }),
      filters = list(
        lv = shiny::reactive({ 1L }),
        bsr_threshold = shiny::reactive({ 3.0 }),
        p_threshold = shiny::reactive({ 0.05 }),
        view_mode = shiny::reactive({ NULL }),
        what = shiny::reactive({ "bsr" })
      )
    ))
  })

})

describe("brain_viewer_server with result", {

  it("handles NULL result gracefully", {
    shiny::testServer(brain_viewer_server, {
      # Module should initialize without error
      expect_equal(local_rv$view_mode, "montage")
      expect_null(local_rv$selected_coord)
    }, args = list(
      result_rv = shiny::reactive({ NULL }),
      filters = list(
        lv = shiny::reactive({ 1L }),
        bsr_threshold = shiny::reactive({ 3.0 }),
        p_threshold = shiny::reactive({ 0.05 }),
        view_mode = shiny::reactive({ "montage" }),
        what = shiny::reactive({ "bsr" })
      )
    ))
  })

  it("handles result with mask", {
    result <- make_mock_pls_result(include_mask = TRUE, include_boot = TRUE)
    shiny::testServer(brain_viewer_server, {
      # Module should initialize without error
      expect_equal(local_rv$view_mode, "montage")
      expect_false(is.null(result_rv()$mask))
    }, args = list(
      result_rv = shiny::reactive({ result }),
      filters = list(
        lv = shiny::reactive({ 1L }),
        bsr_threshold = shiny::reactive({ 3.0 }),
        p_threshold = shiny::reactive({ 0.05 }),
        view_mode = shiny::reactive({ "montage" }),
        what = shiny::reactive({ "bsr" })
      )
    ))
  })

  it("handles result without mask", {
    result <- make_mock_pls_result(include_mask = FALSE)
    shiny::testServer(brain_viewer_server, {
      expect_equal(local_rv$view_mode, "montage")
      expect_null(result_rv()$mask)
    }, args = list(
      result_rv = shiny::reactive({ result }),
      filters = list(
        lv = shiny::reactive({ 1L }),
        bsr_threshold = shiny::reactive({ 3.0 }),
        p_threshold = shiny::reactive({ 0.05 }),
        view_mode = shiny::reactive({ "montage" }),
        what = shiny::reactive({ "bsr" })
      )
    ))
  })

  it("handles full result with all components", {
    result <- load_fixture("pls_result_full")
    shiny::testServer(brain_viewer_server, {
      expect_equal(local_rv$view_mode, "montage")
      expect_false(is.null(result_rv()$boot_result))
      expect_false(is.null(result_rv()$perm_result))
      expect_false(is.null(result_rv()$mask))
    }, args = list(
      result_rv = shiny::reactive({ result }),
      filters = list(
        lv = shiny::reactive({ 1L }),
        bsr_threshold = shiny::reactive({ 3.0 }),
        p_threshold = shiny::reactive({ 0.05 }),
        view_mode = shiny::reactive({ "montage" }),
        what = shiny::reactive({ "bsr" })
      )
    ))
  })

})

describe("brain_viewer_server filter integration", {

  it("accepts filter LV value", {
    shiny::testServer(brain_viewer_server, {
      # Filter LV should be accessible
      expect_equal(filters$lv(), 2L)
    }, args = list(
      result_rv = shiny::reactive({ make_mock_pls_result(n_lv = 5) }),
      filters = list(
        lv = shiny::reactive({ 2L }),
        bsr_threshold = shiny::reactive({ 3.0 }),
        p_threshold = shiny::reactive({ 0.05 }),
        view_mode = shiny::reactive({ "montage" }),
        what = shiny::reactive({ "bsr" })
      )
    ))
  })

  it("accepts filter BSR threshold", {
    shiny::testServer(brain_viewer_server, {
      expect_equal(filters$bsr_threshold(), 2.5)
    }, args = list(
      result_rv = shiny::reactive({ make_mock_pls_result(include_boot = TRUE) }),
      filters = list(
        lv = shiny::reactive({ 1L }),
        bsr_threshold = shiny::reactive({ 2.5 }),
        p_threshold = shiny::reactive({ 0.05 }),
        view_mode = shiny::reactive({ "montage" }),
        what = shiny::reactive({ "bsr" })
      )
    ))
  })

  it("accepts filter what selection (bsr/salience)", {
    shiny::testServer(brain_viewer_server, {
      expect_equal(filters$what(), "salience")
    }, args = list(
      result_rv = shiny::reactive({ make_mock_pls_result() }),
      filters = list(
        lv = shiny::reactive({ 1L }),
        bsr_threshold = shiny::reactive({ 3.0 }),
        p_threshold = shiny::reactive({ 0.05 }),
        view_mode = shiny::reactive({ "montage" }),
        what = shiny::reactive({ "salience" })
      )
    ))
  })

  it("handles NULL filter values gracefully", {
    shiny::testServer(brain_viewer_server, {
      # With NULL filter values, module should still initialize
      expect_equal(local_rv$view_mode, "montage")
    }, args = list(
      result_rv = shiny::reactive({ make_mock_pls_result() }),
      filters = list(
        lv = shiny::reactive({ NULL }),
        bsr_threshold = shiny::reactive({ NULL }),
        p_threshold = shiny::reactive({ NULL }),
        view_mode = shiny::reactive({ NULL }),
        what = shiny::reactive({ NULL })
      )
    ))
  })

})

describe("brain_viewer_server click handling", {

  it("plot click updates selected_coord", {
    shiny::testServer(brain_viewer_server, {
      expect_null(local_rv$selected_coord)

      # Simulate plot click
      session$setInputs(plot_click = list(x = 50, y = 30))

      expect_equal(local_rv$selected_coord$x, 50)
      expect_equal(local_rv$selected_coord$y, 30)
    }, args = list(
      result_rv = shiny::reactive({ make_mock_pls_result() }),
      filters = list(
        lv = shiny::reactive({ 1L }),
        bsr_threshold = shiny::reactive({ 3.0 }),
        p_threshold = shiny::reactive({ 0.05 }),
        view_mode = shiny::reactive({ "montage" }),
        what = shiny::reactive({ "bsr" })
      )
    ))
  })

  it("coordinate is accessible from returned reactive", {
    shiny::testServer(brain_viewer_server, {
      returned <- session$getReturned()

      # Initially NULL
      expect_null(returned()$coord)

      # After click
      session$setInputs(plot_click = list(x = 100, y = 75))

      coord <- returned()$coord
      expect_equal(coord$x, 100)
      expect_equal(coord$y, 75)
    }, args = list(
      result_rv = shiny::reactive({ make_mock_pls_result() }),
      filters = list(
        lv = shiny::reactive({ 1L }),
        bsr_threshold = shiny::reactive({ 3.0 }),
        p_threshold = shiny::reactive({ 0.05 }),
        view_mode = shiny::reactive({ "montage" }),
        what = shiny::reactive({ "bsr" })
      )
    ))
  })

  it("multiple clicks update coordinate correctly", {
    shiny::testServer(brain_viewer_server, {
      returned <- session$getReturned()

      session$setInputs(plot_click = list(x = 10, y = 20))
      expect_equal(returned()$coord$x, 10)
      expect_equal(returned()$coord$y, 20)

      session$setInputs(plot_click = list(x = 55, y = 45))
      expect_equal(returned()$coord$x, 55)
      expect_equal(returned()$coord$y, 45)
    }, args = list(
      result_rv = shiny::reactive({ make_mock_pls_result() }),
      filters = list(
        lv = shiny::reactive({ 1L }),
        bsr_threshold = shiny::reactive({ 3.0 }),
        p_threshold = shiny::reactive({ 0.05 }),
        view_mode = shiny::reactive({ "montage" }),
        what = shiny::reactive({ "bsr" })
      )
    ))
  })

})

describe("brain_viewer_server axis selection", {

  it("default axis selection is axial (3)", {
    shiny::testServer(brain_viewer_server, {
      session$setInputs(axis = "3")
      expect_equal(input$axis, "3")
    }, args = list(
      result_rv = shiny::reactive({ make_mock_pls_result() }),
      filters = list(
        lv = shiny::reactive({ 1L }),
        bsr_threshold = shiny::reactive({ 3.0 }),
        p_threshold = shiny::reactive({ 0.05 }),
        view_mode = shiny::reactive({ "montage" }),
        what = shiny::reactive({ "bsr" })
      )
    ))
  })

  it("coronal axis can be selected", {
    shiny::testServer(brain_viewer_server, {
      session$setInputs(axis = "2")
      expect_equal(input$axis, "2")
    }, args = list(
      result_rv = shiny::reactive({ make_mock_pls_result() }),
      filters = list(
        lv = shiny::reactive({ 1L }),
        bsr_threshold = shiny::reactive({ 3.0 }),
        p_threshold = shiny::reactive({ 0.05 }),
        view_mode = shiny::reactive({ "montage" }),
        what = shiny::reactive({ "bsr" })
      )
    ))
  })

  it("sagittal axis can be selected", {
    shiny::testServer(brain_viewer_server, {
      session$setInputs(axis = "1")
      expect_equal(input$axis, "1")
    }, args = list(
      result_rv = shiny::reactive({ make_mock_pls_result() }),
      filters = list(
        lv = shiny::reactive({ 1L }),
        bsr_threshold = shiny::reactive({ 3.0 }),
        p_threshold = shiny::reactive({ 0.05 }),
        view_mode = shiny::reactive({ "montage" }),
        what = shiny::reactive({ "bsr" })
      )
    ))
  })

})
