# Architecture Patterns for Testable Shiny Apps

**Project:** plsrri Shiny GUI
**Researched:** 2026-01-21
**Focus:** Testability, Extensibility, Pluggable Visualization Backends

## Executive Summary

The existing plsrri Shiny app has a solid modular foundation with R6 state management and bslib theming. To add testing infrastructure and support pluggable visualization backends (including neurosurf surfwidget), the architecture needs targeted refactoring in three areas:

1. **Extract business logic from modules** to enable unit testing without Shiny reactivity
2. **Create visualization backend abstraction** using R6 strategy pattern for swappable renderers
3. **Restructure state management** to support `testServer()` testing patterns

## Current Architecture Assessment

### Strengths

| Component | Current State | Testability |
|-----------|---------------|-------------|
| AppState R6 class | Centralized state management | Good - can test R6 methods directly |
| Modular structure | Six discrete modules | Good - enables isolated testing |
| Separation of UI/Server | Standard Shiny pattern | Good - `testServer()` compatible |
| Business logic (plsrri package) | Separate from GUI | Excellent - already testable |

### Gaps Requiring Refactoring

| Issue | Impact | Priority |
|-------|--------|----------|
| Brain visualization hardcoded to `plot_brain()` | Cannot swap backends | HIGH |
| `reactiveValues` tightly coupled in modules | Hard to inject test doubles | MEDIUM |
| `updateSelectInput` etc. in observers | Requires `{shinytesters}` or `{shinytest2}` | MEDIUM |
| No visualization abstraction layer | Cannot add surfwidget easily | HIGH |

## Recommended Architecture

### Component Boundaries

```
+------------------+     +------------------+     +------------------+
|   UI Components  |     |  Module Servers  |     |  Business Logic  |
|   (mod_*_ui.R)   |<--->|  (mod_*_server.R)|<--->|  (fct_*, utils_) |
+------------------+     +------------------+     +------------------+
                               |
                               v
                    +----------------------+
                    |   AppState (R6)      |
                    |   - workflow state   |
                    |   - analysis results |
                    +----------------------+
                               |
                               v
                    +----------------------+
                    | Visualization Layer  |
                    | (BrainRenderer R6)   |
                    +----------------------+
                         /          \
                        v            v
              +-----------+    +-----------+
              | neuroim2  |    | surfwidget|
              | backend   |    | backend   |
              +-----------+    +-----------+
```

### Layer Responsibilities

| Layer | Responsibility | Testability Strategy |
|-------|---------------|---------------------|
| **UI Components** | Pure HTML/widget generation | Visual regression tests (optional) |
| **Module Servers** | Reactive coordination | `testServer()` with mocked dependencies |
| **Business Logic** | Non-reactive computation | Standard `testthat` unit tests |
| **AppState** | State container | Direct R6 method tests |
| **Visualization Layer** | Render abstraction | Mock renderer for tests |

## Visualization Backend Abstraction

### Strategy Pattern with R6

Create an abstract `BrainRenderer` interface that concrete implementations inherit from:

```r
# R/fct_brain_renderer.R

#' Abstract Brain Renderer Interface
#' @description Base class defining the visualization contract
BrainRenderer <- R6::R6Class(
  "BrainRenderer",
  public = list(
    #' @description Render brain visualization to Shiny output
    #' @param result pls_result object
    #' @param lv Latent variable index
    #' @param what "bsr" or "salience"
    #' @param threshold Numeric threshold
    #' @param ... Additional renderer-specific options
    render = function(result, lv, what, threshold, ...) {
      stop("BrainRenderer$render() is abstract - implement in subclass")
    },

    #' @description Create UI output element
    #' @param ns Namespace function
    #' @param id Output ID
    output_ui = function(ns, id) {
      stop("BrainRenderer$output_ui() is abstract - implement in subclass")
    },

    #' @description Get renderer capabilities
    capabilities = function() {
      list(
        supports_ortho = FALSE,
        supports_montage = FALSE,
        supports_3d = FALSE,
        supports_interaction = FALSE
      )
    }
  )
)

#' neuroim2 Backend (Current Implementation)
Neuroim2Renderer <- R6::R6Class(
  "Neuroim2Renderer",
  inherit = BrainRenderer,
  public = list(
    render = function(result, lv, what, threshold, view = "montage", ...) {
      plsrri::plot_brain(result, lv = lv, what = what,
                         threshold = threshold, view = view, ...)
    },

    output_ui = function(ns, id) {
      shiny::plotOutput(ns(id), height = "auto")
    },

    capabilities = function() {
      list(
        supports_ortho = TRUE,
        supports_montage = TRUE,
        supports_3d = FALSE,
        supports_interaction = FALSE
      )
    }
  )
)

#' surfwidget Backend (New)
SurfwidgetRenderer <- R6::R6Class(
  "SurfwidgetRenderer",
  inherit = BrainRenderer,
  public = list(
    render = function(result, lv, what, threshold, ...) {
      # Convert volumetric data to surface
      # Use neurosurf::surfwidget()
      values <- if (what == "bsr") plsrri::bsr(result, lv) else plsrri::salience(result, lv)
      # ... surface mapping logic
      neurosurf::surfwidget(surface, data = values, ...)
    },

    output_ui = function(ns, id) {
      # htmlwidget output binding
      neurosurf::surfwidgetOutput(ns(id))
    },

    capabilities = function() {
      list(
        supports_ortho = FALSE,
        supports_montage = FALSE,
        supports_3d = TRUE,
        supports_interaction = TRUE
      )
    }
  )
)

#' threeBrain Backend (Optional/Future)
ThreeBrainRenderer <- R6::R6Class(
  "ThreeBrainRenderer",
  inherit = BrainRenderer,
  public = list(
    render = function(result, lv, what, threshold, ...) {
      # Use threeBrain::three_brain() or threeBrain::renderBrain()
      # Requires FreeSurfer/SUMA surface data
    },

    output_ui = function(ns, id) {
      threeBrain::threejsBrainOutput(ns(id))
    },

    capabilities = function() {
      list(
        supports_ortho = TRUE,
        supports_montage = TRUE,
        supports_3d = TRUE,
        supports_interaction = TRUE
      )
    }
  )
)

#' Mock Renderer for Testing
MockBrainRenderer <- R6::R6Class(
  "MockBrainRenderer",
  inherit = BrainRenderer,
  public = list(
    render_calls = NULL,

    initialize = function() {
      self$render_calls <- list()
    },

    render = function(result, lv, what, threshold, ...) {
      self$render_calls <- c(self$render_calls, list(
        list(lv = lv, what = what, threshold = threshold)
      ))
      # Return a simple ggplot for testing
      ggplot2::ggplot() + ggplot2::ggtitle(paste("Mock LV", lv))
    },

    output_ui = function(ns, id) {
      shiny::plotOutput(ns(id))
    },

    capabilities = function() {
      list(supports_ortho = TRUE, supports_montage = TRUE,
           supports_3d = TRUE, supports_interaction = TRUE)
    }
  )
)
```

### Renderer Registry Pattern

```r
# R/fct_renderer_registry.R

#' Renderer Registry
#' @description Factory for creating visualization backends
RendererRegistry <- R6::R6Class(
  "RendererRegistry",
  public = list(
    renderers = NULL,

    initialize = function() {
      self$renderers <- list()
      # Register default backends
      self$register("neuroim2", Neuroim2Renderer$new())

      # Conditionally register surfwidget if available
      if (requireNamespace("neurosurf", quietly = TRUE)) {
        self$register("surfwidget", SurfwidgetRenderer$new())
      }

      # Register threeBrain if available
      if (requireNamespace("threeBrain", quietly = TRUE)) {
        self$register("threebrain", ThreeBrainRenderer$new())
      }
    },

    register = function(name, renderer) {
      stopifnot(inherits(renderer, "BrainRenderer"))
      self$renderers[[name]] <- renderer
    },

    get = function(name) {
      if (!name %in% names(self$renderers)) {
        stop("Unknown renderer: ", name,
             ". Available: ", paste(names(self$renderers), collapse = ", "))
      }
      self$renderers[[name]]
    },

    list_available = function() {
      names(self$renderers)
    },

    #' For testing - inject mock renderer
    inject_mock = function() {
      self$register("mock", MockBrainRenderer$new())
      "mock"
    }
  )
)

# Global singleton (or pass through AppState)
get_renderer_registry <- function() {
  if (is.null(getOption("plsrri.renderer_registry"))) {
    options(plsrri.renderer_registry = RendererRegistry$new())
  }
  getOption("plsrri.renderer_registry")
}
```

## State Management for Testability

### Enhanced AppState with Dependency Injection

```r
# R/state.R (enhanced)

AppState <- R6::R6Class(
  "AppState",
  public = list(
    # ... existing fields ...

    #' @field renderer_name Active renderer backend name
    renderer_name = "neuroim2",

    #' @field renderer_registry Injected renderer factory
    renderer_registry = NULL,

    initialize = function(renderer_registry = NULL) {
      self$renderer_registry <- renderer_registry %||% get_renderer_registry()
      self$reset()
    },

    #' @description Get active renderer
    get_renderer = function() {
      self$renderer_registry$get(self$renderer_name)
    },

    #' @description Switch renderer backend
    set_renderer = function(name) {
      available <- self$renderer_registry$list_available()
      if (!name %in% available) {
        stop("Renderer '", name, "' not available. Options: ",
             paste(available, collapse = ", "))
      }
      self$renderer_name <- name
    }

    # ... rest of existing methods ...
  )
)
```

### Reactive State Wrapper for Testing

```r
# Pattern for testServer compatibility

create_reactive_state <- function(session = NULL,
                                   renderer_registry = NULL) {
  # Allow injection of mock registry for testing
  state <- AppState$new(renderer_registry = renderer_registry)

  rv <- shiny::reactiveValues(
    # ... existing fields ...
    renderer_name = state$renderer_name
  )

  list(
    rv = rv,
    state = state,

    # Allow test injection of renderer
    set_test_renderer = function(name) {
      state$set_renderer(name)
      rv$renderer_name <- name
    }
  )
}
```

## Module Refactoring for Testability

### Pattern: Extract Business Logic

**Before (hard to test):**
```r
brain_viewer_server <- function(id, result_rv, filters) {
  moduleServer(id, function(input, output, session) {
    output$brain_plot <- renderPlot({
      result <- result_rv()
      if (is.null(result)) { plot.new(); return() }

      # Business logic mixed with rendering
      lv <- filters$lv() %||% 1L
      threshold <- filters$bsr_threshold() %||% 3.0

      plsrri::plot_brain(result, lv = lv, what = "bsr",
                         threshold = threshold)
    })
  })
}
```

**After (testable):**
```r
# R/fct_brain_viewer.R - Pure function, easily testable
prepare_brain_render_params <- function(result, filters, defaults = list()) {
  list(
    result = result,
    lv = filters$lv %||% defaults$lv %||% 1L,
    what = filters$what %||% defaults$what %||% "bsr",
    threshold = filters$threshold %||% defaults$threshold %||% 3.0,
    view = filters$view_mode %||% defaults$view %||% "montage"
  )
}

# R/mod_brain_viewer.R - Thin reactive wrapper
brain_viewer_server <- function(id, result_rv, filters,
                                 renderer = NULL) {
  moduleServer(id, function(input, output, session) {
    # Get renderer (injectable for testing)
    active_renderer <- renderer %||% get_renderer_registry()$get("neuroim2")

    # Dynamic output based on renderer type
    output$brain_plot <- renderPlot({
      result <- result_rv()
      if (is.null(result)) { plot.new(); return() }

      # Extract to testable function
      params <- prepare_brain_render_params(
        result = result,
        filters = list(
          lv = filters$lv(),
          what = filters$what(),
          threshold = filters$bsr_threshold(),
          view_mode = filters$view_mode()
        )
      )

      # Delegate to renderer
      active_renderer$render(
        result = params$result,
        lv = params$lv,
        what = params$what,
        threshold = params$threshold,
        view = params$view
      )
    })
  })
}
```

### Pattern: Module Return Values for Testing

```r
# Modules should return testable values

filter_bar_server <- function(id, result_rv, state_rv) {
  moduleServer(id, function(input, output, session) {
    # ... existing code ...

    # Return structured list for testing
    list(
      lv = reactive({
        if (input$lv == "all") NULL else as.integer(input$lv)
      }),
      bsr_threshold = reactive({ input$bsr_threshold }),
      p_threshold = reactive({ input$p_threshold }),
      view_mode = reactive({ input$view_mode }),
      what = reactive({ input$what }),

      # Test helper: get current filter state as list
      get_filter_state = reactive({
        list(
          lv = input$lv,
          bsr_threshold = input$bsr_threshold,
          p_threshold = input$p_threshold,
          view_mode = input$view_mode,
          what = input$what
        )
      })
    )
  })
}
```

## Testing Strategy

### Test Pyramid

```
                    /\
                   /  \
                  / E2E \        <- shinytest2 (few, slow)
                 /--------\
                /Integration\    <- testServer (medium)
               /--------------\
              /   Unit Tests    \ <- testthat (many, fast)
             /------------------\
```

### Test Categories by Component

| Component | Tool | What to Test |
|-----------|------|--------------|
| `fct_*.R` business logic | testthat | Pure function behavior |
| AppState R6 | testthat | State transitions, validation |
| BrainRenderer | testthat | Render output, capabilities |
| Module servers | testServer | Reactive behavior, returns |
| Update* functions | shinytesters | Input synchronization |
| Full user flows | shinytest2 | End-to-end scenarios |

### Sample Test Structure

```r
# tests/testthat/test-fct_brain_viewer.R

test_that("prepare_brain_render_params uses defaults correctly", {
  params <- prepare_brain_render_params(
    result = mock_result,
    filters = list(lv = NULL, what = NULL),
    defaults = list(lv = 1, what = "bsr", threshold = 3)
  )

  expect_equal(params$lv, 1)
  expect_equal(params$what, "bsr")
  expect_equal(params$threshold, 3)
})

# tests/testthat/test-mod_brain_viewer.R

test_that("brain_viewer_server calls renderer with correct params", {
  mock_renderer <- MockBrainRenderer$new()
  mock_result <- make_mock_pls_result()

  testServer(brain_viewer_server, {
    # Module needs result_rv and filters
  }, args = list(
    result_rv = reactive({ mock_result }),
    filters = list(
      lv = reactive({ 2L }),
      what = reactive({ "bsr" }),
      bsr_threshold = reactive({ 2.5 }),
      view_mode = reactive({ "montage" })
    ),
    renderer = mock_renderer
  ))

  # Verify renderer was called
  expect_length(mock_renderer$render_calls, 1)
  expect_equal(mock_renderer$render_calls[[1]]$lv, 2)
})

# tests/testthat/test-renderer-backends.R

test_that("Neuroim2Renderer produces ggplot output", {
  renderer <- Neuroim2Renderer$new()
  result <- make_mock_pls_result_with_mask()

  output <- renderer$render(result, lv = 1, what = "bsr", threshold = 3)

  expect_s3_class(output, "ggplot")
})

test_that("RendererRegistry registers and retrieves backends", {
  registry <- RendererRegistry$new()

  expect_true("neuroim2" %in% registry$list_available())

  renderer <- registry$get("neuroim2")
  expect_s3_class(renderer, "BrainRenderer")
})
```

### Using shinytesters for Update Functions

```r
# tests/testthat/test-mod_filter_bar.R

test_that("filter_bar updates LV choices when result changes", {
  # Enable shinytesters mocking for update* functions
  shinytesters::use_shiny_testers()

  testServer(filter_bar_server, {
    session$setInputs(lv = "1")

    # Simulate result with 5 LVs
    # This would trigger updateSelectInput in the real module
    # shinytesters handles the mock

    # Check the filter state
    state <- session$getReturned()$get_filter_state()
    expect_equal(state()$lv, "1")
  }, args = list(
    result_rv = reactive({ make_mock_result(n_lv = 5) }),
    state_rv = reactiveValues()
  ))
})
```

## File Organization

### Recommended Structure

```
inst/shiny/
  R/
    # Application logic (mod_*)
    mod_setup.R           # Setup module UI + server
    mod_analyze.R         # Analyze module UI + server
    mod_explore.R         # Explore module UI + server
    mod_brain_viewer.R    # Brain viewer module (uses renderer)
    mod_filter_bar.R      # Filter bar module
    mod_inspector.R       # Inspector module

    # Business logic (fct_*)
    fct_brain_viewer.R    # Brain viewer business logic
    fct_brain_renderer.R  # Renderer abstraction classes
    fct_renderer_registry.R  # Renderer factory/registry
    fct_data_validation.R # Spec validation logic

    # Utilities (utils_*)
    utils_helpers.R       # Small helper functions

    # State management
    state.R               # AppState R6 class

    # UI components
    ui_components.R       # Reusable UI widgets
    theme.R               # bslib theme

  app.R                   # Entry point
  www/
    styles.css

tests/testthat/
  # Unit tests (fast, many)
  test-fct_brain_viewer.R
  test-fct_brain_renderer.R
  test-fct_data_validation.R
  test-state.R

  # Integration tests (medium)
  test-mod_brain_viewer.R
  test-mod_filter_bar.R
  test-mod_setup.R

  # Test helpers
  helper-mock_data.R       # Mock pls_result, pls_spec factories
  helper-mock_renderer.R   # MockBrainRenderer setup
```

## Refactoring Order (Build Phases)

### Phase 1: Foundation (Enable Testing)
**Priority: HIGH | Risk: LOW**

1. Extract business logic from `mod_brain_viewer.R` into `fct_brain_viewer.R`
2. Add return values to modules that don't have them
3. Create test helpers (`helper-mock_data.R`)
4. Write unit tests for existing business logic

### Phase 2: Visualization Abstraction
**Priority: HIGH | Risk: MEDIUM**

1. Create `BrainRenderer` abstract class
2. Implement `Neuroim2Renderer` wrapping current `plot_brain()`
3. Create `RendererRegistry`
4. Refactor `mod_brain_viewer.R` to use renderer
5. Write tests for renderer system

### Phase 3: Add surfwidget Backend
**Priority: HIGH | Risk: MEDIUM**

1. Implement `SurfwidgetRenderer`
2. Add volumetric-to-surface mapping logic
3. Update UI to support htmlwidget output
4. Add renderer selection to UI
5. Test surfwidget integration

### Phase 4: Module Testing Infrastructure
**Priority: MEDIUM | Risk: LOW**

1. Add `{shinytesters}` for update function mocking
2. Write `testServer` tests for all modules
3. Create CI pipeline for tests

### Phase 5: Optional - threeBrain Backend
**Priority: LOW | Risk: MEDIUM**

1. Implement `ThreeBrainRenderer`
2. Handle FreeSurfer surface requirements
3. Add surface data loading to setup module

## Anti-Patterns to Avoid

### 1. Reactive Spaghetti
**Bad:** Modules that directly modify each other's state
**Good:** Modules communicate through returned reactives or shared AppState

### 2. Hardcoded Visualization
**Bad:** `plsrri::plot_brain()` called directly in render functions
**Good:** Delegate to injected `BrainRenderer` instance

### 3. Untestable Observers
**Bad:** Complex logic inside `observeEvent()` blocks
**Good:** Extract logic to pure functions, observers just coordinate

### 4. God AppState
**Bad:** AppState knows about rendering implementation details
**Good:** AppState manages workflow state, delegates rendering to RendererRegistry

### 5. Missing Return Values
**Bad:** Module server returns NULL
**Good:** Module server returns list of reactives for testing and composition

## Sources

- [Mastering Shiny - Chapter 21 Testing](https://mastering-shiny.org/scaling-testing.html)
- [Comprehensive Guide to Testing Shiny Modules with testServer](https://jakubsobolewski.com/blog/anatomy-of-a-shiny-module-test/)
- [Engineering Production-Grade Shiny Apps - Structuring Your Project](https://engineering-shiny.org/structuring-project.html)
- [Appsilon - How to Write Tests with shiny::testServer](https://www.appsilon.com/post/how-to-write-tests-with-shiny-testserver)
- [shinytesters Package - CRAN](https://cran.r-project.org/web/packages/shinytesters/index.html)
- [threeBrain renderBrain Documentation](https://rdrr.io/cran/threeBrain/man/renderBrain.html)
- [neurosurf GitHub Repository](https://github.com/bbuchsbaum/neurosurf)
- [R6 Advanced R - Hadley Wickham](https://adv-r.hadley.nz/r6.html)
- [R6 Interface/Abstract Class Discussion](https://github.com/r-lib/R6/issues/82)
- [distr6 R6 Design Patterns](https://rdrr.io/github/RaphaelS1/distr6/f/vignettes/webs/r6_and_design_patterns.rmd)
