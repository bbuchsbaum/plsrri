# Test fct_brain_renderer R6 classes
#
# These tests run WITHOUT Shiny context - plain testthat.
# Unit tests for brain renderer abstraction layer.

# Source the functions directly (works both installed + dev mode)
module_file <- system.file("shiny/R/fct_brain_renderer.R", package = "plsrri")
if (module_file == "") {
  module_file <- testthat::test_path("../../inst/shiny/R/fct_brain_renderer.R")
}
source(module_file, local = FALSE)

# Also source fct_surface_mapper.R (SurfwidgetRenderer depends on it)
mapper_file <- system.file("shiny/R/fct_surface_mapper.R", package = "plsrri")
if (mapper_file == "") {
  mapper_file <- testthat::test_path("../../inst/shiny/R/fct_surface_mapper.R")
}
if (file.exists(mapper_file)) {
  source(mapper_file, local = FALSE)
}

describe("BrainRenderer (abstract)", {

  it("can be instantiated", {
    renderer <- BrainRenderer$new()
    expect_s3_class(renderer, "BrainRenderer")
  })

  it("render() throws abstract method error", {
    renderer <- BrainRenderer$new()
    expect_error(
      renderer$render(NULL, 1, "bsr", 3, "montage"),
      "abstract.*implement in subclass"
    )
  })

  it("is_widget() returns FALSE by default", {
    renderer <- BrainRenderer$new()
    expect_false(renderer$is_widget())
  })

})

describe("Neuroim2Renderer", {

  it("inherits from BrainRenderer", {
    renderer <- Neuroim2Renderer$new()
    expect_s3_class(renderer, "BrainRenderer")
    expect_s3_class(renderer, "Neuroim2Renderer")
  })

  it("is_widget() returns FALSE", {
    renderer <- Neuroim2Renderer$new()
    expect_false(renderer$is_widget())
  })

  it("render() calls plot_brain with correct parameters", {
    skip_if_not_installed("mockery")
    skip_if_not_installed("neuroim2")

    renderer <- Neuroim2Renderer$new()

    # Mock plot_brain
    mock_plot <- mockery::mock(ggplot2::ggplot())

    # Temporarily replace plot_brain
    mockery::stub(renderer$render, "plsrri::plot_brain", mock_plot)

    # Call render
    result <- renderer$render(
      result = "mock_result",
      lv = 2,
      what = "bsr",
      threshold = 3.5,
      view = "ortho",
      along = 2
    )

    # Verify plot_brain was called
    mockery::expect_called(mock_plot, 1)

    # Verify arguments
    args <- mockery::mock_args(mock_plot)[[1]]
    expect_equal(args[[1]], "mock_result")
    expect_equal(args$lv, 2)
    expect_equal(args$what, "bsr")
    expect_equal(args$threshold, 3.5)
    expect_equal(args$view, "ortho")
    expect_equal(args$along, 2)
  })

})

describe("MockBrainRenderer", {

  it("inherits from BrainRenderer", {
    renderer <- MockBrainRenderer$new()
    expect_s3_class(renderer, "BrainRenderer")
    expect_s3_class(renderer, "MockBrainRenderer")
  })

  it("initializes with empty render_calls", {
    renderer <- MockBrainRenderer$new()
    expect_type(renderer$render_calls, "list")
    expect_equal(length(renderer$render_calls), 0)
  })

  it("records render calls", {
    renderer <- MockBrainRenderer$new()

    renderer$render("result1", 1, "bsr", 3, "montage")
    expect_equal(length(renderer$render_calls), 1)

    renderer$render("result2", 2, "salience", NULL, "ortho")
    expect_equal(length(renderer$render_calls), 2)
  })

  it("stores call parameters", {
    renderer <- MockBrainRenderer$new()

    renderer$render("my_result", 3, "bsr", 2.5, "ortho", along = 2)

    call_record <- renderer$render_calls[[1]]
    expect_equal(call_record$result, "my_result")
    expect_equal(call_record$lv, 3)
    expect_equal(call_record$what, "bsr")
    expect_equal(call_record$threshold, 2.5)
    expect_equal(call_record$view, "ortho")
    expect_equal(call_record$extra_args$along, 2)
  })

  it("returns ggplot object", {
    renderer <- MockBrainRenderer$new()
    result <- renderer$render("result", 1, "bsr", 3, "montage")
    expect_s3_class(result, "gg")
    expect_s3_class(result, "ggplot")
  })

  it("reset_calls() clears history", {
    renderer <- MockBrainRenderer$new()

    renderer$render("result1", 1, "bsr", 3, "montage")
    renderer$render("result2", 2, "bsr", 3, "montage")
    expect_equal(length(renderer$render_calls), 2)

    renderer$reset_calls()
    expect_equal(length(renderer$render_calls), 0)
  })

  it("reset_calls() returns self for chaining", {
    renderer <- MockBrainRenderer$new()
    result <- renderer$reset_calls()
    expect_identical(result, renderer)
  })

})

describe("RendererRegistry", {

  it("initializes with empty registry", {
    registry <- RendererRegistry$new()
    expect_type(registry$renderers, "list")
  })

  it("registers neuroim2 by default", {
    registry <- RendererRegistry$new()
    available <- registry$list_available()
    expect_true("neuroim2" %in% available)
  })

  it("get() returns registered renderer", {
    registry <- RendererRegistry$new()
    renderer <- registry$get("neuroim2")
    expect_s3_class(renderer, "Neuroim2Renderer")
    expect_s3_class(renderer, "BrainRenderer")
  })

  it("get() errors for unknown renderer", {
    registry <- RendererRegistry$new()
    expect_error(
      registry$get("unknown"),
      "Renderer 'unknown' not found"
    )
  })

  it("register() adds new renderer", {
    registry <- RendererRegistry$new()
    mock_renderer <- MockBrainRenderer$new()

    registry$register("mock", mock_renderer)

    available <- registry$list_available()
    expect_true("mock" %in% available)

    retrieved <- registry$get("mock")
    expect_identical(retrieved, mock_renderer)
  })

  it("register() validates BrainRenderer inheritance", {
    registry <- RendererRegistry$new()

    expect_error(
      registry$register("invalid", "not_a_renderer"),
      "inherits.*BrainRenderer"
    )
  })

  it("list_available() returns names", {
    registry <- RendererRegistry$new()
    available <- registry$list_available()
    expect_type(available, "character")
    expect_true(length(available) > 0)
  })

  it("register() returns self for chaining", {
    registry <- RendererRegistry$new()
    mock_renderer <- MockBrainRenderer$new()

    result <- registry$register("mock", mock_renderer)
    expect_identical(result, registry)
  })

  it("can register multiple renderers", {
    registry <- RendererRegistry$new()

    mock1 <- MockBrainRenderer$new()
    mock2 <- MockBrainRenderer$new()

    registry$register("mock1", mock1)
    registry$register("mock2", mock2)

    available <- registry$list_available()
    expect_true("neuroim2" %in% available)
    expect_true("mock1" %in% available)
    expect_true("mock2" %in% available)
    expect_equal(length(available), 3)
  })

  it("has_surfwidget() checks neurosurf availability", {
    registry <- RendererRegistry$new()

    # Result should be a logical
    result <- registry$has_surfwidget()
    expect_type(result, "logical")
    expect_length(result, 1)
  })

  it("register_surfwidget() conditionally registers", {
    skip_if_not_installed("neurosurf")

    registry <- RendererRegistry$new()
    registry$register_surfwidget()

    # If neurosurf is available, surfwidget should be registered
    available <- registry$list_available()
    expect_true("surfwidget" %in% available)

    # Get it and verify it's a SurfwidgetRenderer
    renderer <- registry$get("surfwidget")
    expect_s3_class(renderer, "SurfwidgetRenderer")
    expect_true(renderer$is_widget())
  })

  it("register_surfwidget() returns self for chaining", {
    registry <- RendererRegistry$new()
    result <- registry$register_surfwidget()
    expect_identical(result, registry)
  })

})

describe("SurfwidgetRenderer", {

  it("inherits from BrainRenderer", {
    renderer <- SurfwidgetRenderer$new()
    expect_s3_class(renderer, "BrainRenderer")
    expect_s3_class(renderer, "SurfwidgetRenderer")
  })

  it("is_widget() returns TRUE", {
    renderer <- SurfwidgetRenderer$new()
    expect_true(renderer$is_widget())
  })

  it("initializes with default geometry", {
    renderer <- SurfwidgetRenderer$new()
    expect_equal(renderer$geometry, "inflated")
    expect_null(renderer$surfaces)
    expect_type(renderer$samplers, "list")
    expect_equal(length(renderer$samplers), 0)
  })

  it("accepts custom geometry parameter", {
    renderer <- SurfwidgetRenderer$new(geometry = "pial")
    expect_equal(renderer$geometry, "pial")
  })

  it("set_geometry() updates geometry and clears surface cache", {
    skip_if_not_installed("neurosurf")

    renderer <- SurfwidgetRenderer$new()

    # Force load surfaces
    renderer$surfaces <- get_fsaverage_surfaces("inflated")
    expect_false(is.null(renderer$surfaces))

    # Change geometry
    renderer$set_geometry("pial")
    expect_equal(renderer$geometry, "pial")
    expect_null(renderer$surfaces)  # Should be cleared
  })

  it("set_geometry() returns self for chaining", {
    renderer <- SurfwidgetRenderer$new()
    result <- renderer$set_geometry("white")
    expect_identical(result, renderer)
  })

})

describe("MockSurfwidgetRenderer", {

  it("inherits from BrainRenderer", {
    renderer <- MockSurfwidgetRenderer$new()
    expect_s3_class(renderer, "BrainRenderer")
    expect_s3_class(renderer, "MockSurfwidgetRenderer")
  })

  it("is_widget() returns TRUE", {
    renderer <- MockSurfwidgetRenderer$new()
    expect_true(renderer$is_widget())
  })

  it("initializes with empty render_calls", {
    renderer <- MockSurfwidgetRenderer$new()
    expect_type(renderer$render_calls, "list")
    expect_equal(length(renderer$render_calls), 0)
  })

  it("records render calls", {
    renderer <- MockSurfwidgetRenderer$new()

    renderer$render("result1", 1, "bsr", 3, "surface")
    expect_equal(length(renderer$render_calls), 1)

    renderer$render("result2", 2, "salience", 2.5, "surface")
    expect_equal(length(renderer$render_calls), 2)
  })

  it("stores call parameters correctly", {
    renderer <- MockSurfwidgetRenderer$new()

    renderer$render("my_result", 3, "bsr", 2.5, "surface", extra = "arg")

    call_record <- renderer$render_calls[[1]]
    expect_equal(call_record$result, "my_result")
    expect_equal(call_record$lv, 3)
    expect_equal(call_record$what, "bsr")
    expect_equal(call_record$threshold, 2.5)
    expect_equal(call_record$view, "surface")
    expect_equal(call_record$extra_args$extra, "arg")
  })

  it("returns htmlwidget-like object", {
    renderer <- MockSurfwidgetRenderer$new()
    result <- renderer$render("result", 1, "bsr", 3, "surface")

    expect_s3_class(result, "htmlwidget")
    expect_s3_class(result, "surfwidget")
  })

  it("reset_calls() clears history", {
    renderer <- MockSurfwidgetRenderer$new()

    renderer$render("result1", 1, "bsr", 3, "surface")
    renderer$render("result2", 2, "bsr", 3, "surface")
    expect_equal(length(renderer$render_calls), 2)

    renderer$reset_calls()
    expect_equal(length(renderer$render_calls), 0)
  })

  it("reset_calls() returns self for chaining", {
    renderer <- MockSurfwidgetRenderer$new()
    result <- renderer$reset_calls()
    expect_identical(result, renderer)
  })

})
