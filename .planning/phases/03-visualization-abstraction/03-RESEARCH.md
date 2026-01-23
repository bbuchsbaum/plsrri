# Phase 3: Visualization Abstraction - Research

**Researched:** 2026-01-22
**Domain:** R6 strategy pattern, abstract classes, renderer abstraction in Shiny
**Confidence:** HIGH

## Summary

This phase implements a pluggable visualization backend architecture using R6's strategy pattern. The standard approach in R is to create an "abstract" base class with methods that throw `stop()` or `rlang::abort()` errors, which concrete implementations override. R6 does not have formal interface support, so duck typing is used—missing methods error naturally when called.

The research confirms that the pattern outlined in ARCHITECTURE.md is the R community's best practice for this problem. The key insight is that R6 strategy patterns work well in Shiny when combined with dependency injection for testing and per-session instantiation for renderer registries.

**Primary recommendation:** Create BrainRenderer base class with `render()` method throwing "abstract method" errors. Neuroim2Renderer wraps existing `plot_brain()` (which returns ggplot objects). Use per-session RendererRegistry instantiated in server function. MockBrainRenderer tracks render calls for testServer() testing.

## Standard Stack

The established libraries/tools for R6 strategy pattern and Shiny visualization:

### Core
| Library | Version | Purpose | Why Standard |
|---------|---------|---------|--------------|
| R6 | 2.5.1 | Encapsulated OOP classes | Industry standard for stateful objects in R |
| shiny | 1.12.1+ | Reactive UI framework | Built-in renderPlot() works with ggplot |
| testthat | 3.3.2+ | Testing framework | R6 mocking via local_mocked_r6_class() (added 3.3.0) |
| rlang | 1.1.0+ | Error handling | `rlang::abort()` for clear error messages |

### Supporting
| Library | Version | Purpose | When to Use |
|---------|---------|---------|-------------|
| neuroim2 | Current | Brain visualization (existing) | Plot backend for Neuroim2Renderer |
| ggplot2 | 3.4.0+ | Graphics system | plot_brain() returns ggplot objects |

### Alternatives Considered
| Instead of | Could Use | Tradeoff |
|------------|-----------|----------|
| R6 | S3 generic + methods | S3 is simpler but lacks reference semantics and state management |
| Strategy pattern | Direct if/else logic | Pattern enables testing, extensibility, and clean module code |
| Duck typing | Explicit capability flags | Duck typing is more R-idiomatic; explicit flags add ceremony |

**Installation:**
```bash
# All dependencies already in plsrri DESCRIPTION
# R6, shiny, ggplot2, testthat already present
install.packages("rlang")  # If not already available
```

## Architecture Patterns

### Recommended Class Structure
```
R/
└── fct_brain_renderer.R       # All renderer classes + registry

inst/shiny/R/
└── mod_brain_viewer.R         # Uses registry to get active renderer
```

### Pattern 1: Abstract Base Class with stop() Methods
**What:** R6 class defining the interface contract; methods throw errors if not overridden
**When to use:** When you need multiple implementations conforming to same interface
**Example:**
```r
# Source: https://jakubsobolewski.com/blog/testable-r6-interfaces/ +
# https://github.com/r-lib/R6/issues/82

BrainRenderer <- R6::R6Class(
  "BrainRenderer",
  public = list(
    #' @description Render brain visualization
    #' @param result pls_result object
    #' @param lv Integer latent variable index
    #' @param what Character "bsr" or "salience"
    #' @param threshold Numeric threshold value
    #' @param view Character "montage" or "ortho"
    #' @param ... Additional renderer-specific parameters
    #' @return Renderer-specific output (ggplot for neuroim2, htmlwidget for surfwidget)
    render = function(result, lv, what, threshold, view = "montage", ...) {
      stop("BrainRenderer$render() is abstract - implement in subclass", call. = FALSE)
    }
  )
)
```

**Rationale:** R6 maintainer confirmed interfaces are out of scope (minimalist philosophy). Community uses `stop()` or `rlang::abort()` pattern. Duck typing is R-idiomatic—no need for capability flags.

### Pattern 2: Concrete Implementation Wrapper
**What:** Subclass that wraps existing function (plot_brain)
**When to use:** Adapting existing code to new interface
**Example:**
```r
# Wraps existing plsrri::plot_brain() function
Neuroim2Renderer <- R6::R6Class(
  "Neuroim2Renderer",
  inherit = BrainRenderer,
  public = list(
    render = function(result, lv, what, threshold, view = "montage", ...) {
      # Delegate to existing function
      plsrri::plot_brain(
        result,
        lv = lv,
        what = what,
        threshold = threshold,
        view = view,
        ...
      )
    }
  )
)
```

**Rationale:** plot_brain() already returns ggplot objects (verified in R/plot_brain.R:179). renderPlot() automatically handles ggplot printing. Wrapper is minimal.

### Pattern 3: Factory Registry with Per-Session Instantiation
**What:** Factory that registers and retrieves renderer instances
**When to use:** Managing multiple backend implementations
**Example:**
```r
# Source: https://mjfrigaard.github.io/shiny-app-pkgs/r6.html
# Avoid package-level instantiation - do inside server()

RendererRegistry <- R6::R6Class(
  "RendererRegistry",
  public = list(
    renderers = NULL,

    initialize = function() {
      self$renderers <- list()
      # Always register neuroim2 (core dependency)
      self$register("neuroim2", Neuroim2Renderer$new())
    },

    register = function(name, renderer) {
      stopifnot(inherits(renderer, "BrainRenderer"))
      self$renderers[[name]] <- renderer
      invisible(self)
    },

    get = function(name) {
      if (!name %in% names(self$renderers)) {
        stop("Unknown renderer: ", name, call. = FALSE)
      }
      self$renderers[[name]]
    },

    list_available = function() {
      names(self$renderers)
    }
  )
)

# Create per-session in server function (NOT at package level)
brain_viewer_server <- function(id, result_rv, filters) {
  moduleServer(id, function(input, output, session) {
    # Per-session instantiation (best practice for Shiny)
    registry <- RendererRegistry$new()
    active_renderer <- registry$get("neuroim2")

    output$brain_plot <- renderPlot({
      # Use renderer...
    })
  })
}
```

**Rationale:** Shiny best practice is per-session R6 instantiation (not package-level). Each user session gets isolated registry. Enables testing via dependency injection.

### Pattern 4: Mock Renderer for Testing
**What:** Test double that records method calls instead of rendering
**When to use:** testServer() tests without visualization dependencies
**Example:**
```r
# Source: https://testthat.r-lib.org/articles/mocking.html
# testthat 3.3.0+ provides local_mocked_r6_class() but dependency injection is cleaner

MockBrainRenderer <- R6::R6Class(
  "MockBrainRenderer",
  inherit = BrainRenderer,
  public = list(
    render_calls = NULL,

    initialize = function() {
      self$render_calls <- list()
    },

    render = function(result, lv, what, threshold, view = "montage", ...) {
      # Record the call
      self$render_calls <- c(self$render_calls, list(list(
        lv = lv,
        what = what,
        threshold = threshold,
        view = view
      )))

      # Return minimal ggplot for renderPlot compatibility
      ggplot2::ggplot() + ggplot2::ggtitle(paste("Mock LV", lv))
    }
  )
)

# In tests: inject mock via dependency injection
testServer(brain_viewer_server, {
  # ...
}, args = list(
  result_rv = reactive(mock_result),
  filters = mock_filters,
  renderer = MockBrainRenderer$new()  # Inject test double
))
```

**Rationale:** Dependency injection is cleaner than local_mocked_r6_class() for this use case. Mock records calls for assertions while still returning valid ggplot objects.

### Anti-Patterns to Avoid
- **Package-level R6 instantiation:** Creates shared state across sessions; use per-session instantiation instead
- **Explicit capability flags:** Duck typing is more R-idiomatic; let missing methods error naturally
- **Tight coupling to plot_brain():** Module should depend on BrainRenderer interface, not concrete implementation
- **Lifecycle methods in interface:** Keep interface minimal (render-only); caller manages lifecycle

## Don't Hand-Roll

Problems that look simple but have existing solutions:

| Problem | Don't Build | Use Instead | Why |
|---------|-------------|-------------|-----|
| R6 abstract classes | Custom interface enforcement | `stop()` in base methods | Community standard pattern; R6 maintainer confirmed no built-in support needed |
| Testing R6 in Shiny | Complex mocking infrastructure | Dependency injection + simple mocks | testthat local_mocked_r6_class() exists but DI is cleaner for strategy pattern |
| Per-session state | Global singletons with session tracking | Per-session R6 instantiation in server() | Shiny best practice; automatic isolation |
| Renderer selection | String-based if/else chains | Registry pattern with lookup | Extensible, testable, follows OOP principles |

**Key insight:** R6 doesn't need formal interface support because duck typing + runtime errors provide the same guarantees. Over-engineering with capability flags or complex validation adds complexity without benefit.

## Common Pitfalls

### Pitfall 1: Global R6 Singleton Causing Session Interference
**What goes wrong:** Instantiating registry at package level causes all sessions to share same renderers
**Why it happens:** Coming from languages with singleton patterns; not understanding Shiny's per-session model
**How to avoid:** Always instantiate R6 objects inside `server()` or module server function
**Warning signs:**
- Registry created with `<<-` or at top level of module file
- Tests fail when run in parallel
- Session A's renderer choice affects Session B

**Prevention:**
```r
# BAD - package level
registry <- RendererRegistry$new()  # All sessions share this!

# GOOD - per-session
brain_viewer_server <- function(id, ...) {
  moduleServer(id, function(input, output, session) {
    registry <- RendererRegistry$new()  # Each session gets own instance
  })
}
```

### Pitfall 2: Forgetting to Override Abstract Methods
**What goes wrong:** Subclass forgets to implement render(), error only occurs at runtime when method is called
**Why it happens:** R6 doesn't enforce interfaces at class definition time
**How to avoid:**
- Write tests that exercise all interface methods
- Use clear error messages in base class (`stop("X is abstract - implement in subclass")`)
**Warning signs:**
- Error message says "is abstract" when rendering
- Tests missing for new renderer implementation

**Detection:**
```r
test_that("NewRenderer implements all BrainRenderer methods", {
  renderer <- NewRenderer$new()

  # Ensure render doesn't throw abstract error
  expect_no_error({
    renderer$render(mock_result, lv = 1, what = "bsr", threshold = 3)
  }, message = "is abstract")
})
```

### Pitfall 3: Returning Wrong Type from render()
**What goes wrong:** Renderer returns htmlwidget but module uses renderPlot() expecting ggplot
**Why it happens:** Different renderers return different output types (ggplot vs htmlwidget)
**How to avoid:**
- Document return type expectations clearly
- Use renderer-specific render functions (renderPlot vs renderUI)
- Consider dynamic UI based on renderer type
**Warning signs:**
- Error: "object of type 'closure' is not subsettable"
- Widget appears blank in Shiny
- Print method called but nothing displays

**Solution Pattern:**
```r
# Module needs to handle different output types
output$brain_plot <- renderPlot({
  # Works for ggplot-returning renderers (Neuroim2)
  active_renderer$render(...)
})

# OR use dynamic output type based on renderer
output$brain_display <- renderUI({
  renderer <- active_renderer
  if (inherits(renderer, "Neuroim2Renderer")) {
    plotOutput(ns("brain_plot"))
  } else if (inherits(renderer, "SurfwidgetRenderer")) {
    surfwidgetOutput(ns("brain_widget"))
  }
})
```

### Pitfall 4: Not Handling Missing Dependencies Gracefully
**What goes wrong:** Trying to register SurfwidgetRenderer when neurosurf package not installed
**Why it happens:** Conditional registration logic missing or incorrect
**How to avoid:** Use `requireNamespace()` checks before registering optional renderers
**Warning signs:**
- "there is no package called 'X'" error in production
- Tests fail in CI but pass locally

**Prevention:**
```r
initialize = function() {
  # Always available
  self$register("neuroim2", Neuroim2Renderer$new())

  # Conditionally register
  if (requireNamespace("neurosurf", quietly = TRUE)) {
    self$register("surfwidget", SurfwidgetRenderer$new())
  }
}
```

## Code Examples

Verified patterns from codebase and official sources:

### Current plot_brain Usage in mod_brain_viewer
```r
# Source: inst/shiny/R/mod_brain_viewer.R:160-169
# Current direct usage - this will be replaced by renderer pattern

output$brain_plot <- renderPlot({
  result <- result_rv()
  if (is.null(result)) {
    plot.new()
    text(0.5, 0.5, "No results to display", cex = 1.2, col = "#6B7280")
    return()
  }

  # Get filter values
  lv <- filters$lv() %||% 1L
  threshold <- filters$bsr_threshold() %||% 3.0
  what <- filters$what() %||% "bsr"
  view <- local_rv$view_mode

  # Direct call to plot_brain
  p <- plsrri::plot_brain(
    result,
    lv = lv,
    what = what,
    threshold = threshold,
    view = view,
    along = as.integer(input$axis)
  )
  print(p)
})
```

### After Refactor: Using Renderer Pattern
```r
# Source: Based on patterns from ARCHITECTURE.md and R6 best practices
# New pattern using dependency injection

brain_viewer_server <- function(id, result_rv, filters, renderer = NULL) {
  moduleServer(id, function(input, output, session) {
    # Create registry per-session (or accept injected renderer for testing)
    registry <- if (is.null(renderer)) {
      RendererRegistry$new()
    } else {
      NULL  # Use injected renderer directly
    }

    active_renderer <- renderer %||% registry$get("neuroim2")

    output$brain_plot <- renderPlot({
      result <- result_rv()
      if (is.null(result)) {
        plot.new()
        text(0.5, 0.5, "No results to display", cex = 1.2, col = "#6B7280")
        return()
      }

      # Extract parameters (same as before)
      lv <- filters$lv() %||% 1L
      threshold <- filters$bsr_threshold() %||% 3.0
      what <- filters$what() %||% "bsr"
      view <- local_rv$view_mode

      # Delegate to renderer
      p <- active_renderer$render(
        result = result,
        lv = lv,
        what = what,
        threshold = threshold,
        view = view,
        along = as.integer(input$axis)
      )
      print(p)
    })
  })
}
```

### Testing with Mock Renderer
```r
# Source: https://testthat.r-lib.org/articles/mocking.html
# Pattern: Dependency injection for clean tests

test_that("brain_viewer calls renderer with correct parameters", {
  mock_renderer <- MockBrainRenderer$new()
  mock_result <- make_test_pls_result()

  testServer(brain_viewer_server, {
    # Renderer should be called when filters update
    session$setInputs(axis = 3)
    session$flushReact()

    # Verify render was called
    expect_length(mock_renderer$render_calls, 1)
    call <- mock_renderer$render_calls[[1]]
    expect_equal(call$lv, 1)
    expect_equal(call$what, "bsr")
    expect_equal(call$threshold, 3.0)
  }, args = list(
    result_rv = reactive(mock_result),
    filters = list(
      lv = reactive(1L),
      bsr_threshold = reactive(3.0),
      what = reactive("bsr")
    ),
    renderer = mock_renderer  # Inject mock via dependency injection
  ))
})
```

### Existing R6 State Pattern in Codebase
```r
# Source: inst/shiny/R/state.R:11-80
# Demonstrates R6 usage already in plsrri

AppState <- R6::R6Class(
  "AppState",
  public = list(
    step = 1L,
    max_step = 1L,
    spec = NULL,
    result = NULL,
    # ... other fields

    initialize = function() {
      self$spec <- NULL
      self$result <- NULL
      self$reset()
    },

    reset = function() {
      self$step <- 1L
      self$max_step <- 1L
      # ... reset other fields
      invisible(self)  # Method chaining pattern
    }

    # ... other methods
  )
)
```

**Pattern note:** AppState already uses R6 with method chaining (returning `invisible(self)`). BrainRenderer follows same conventions.

## State of the Art

| Old Approach | Current Approach | When Changed | Impact |
|--------------|------------------|--------------|--------|
| Direct plot_brain() calls in modules | Renderer abstraction layer | 2025/2026 (this refactor) | Enables surfwidget swap, testability |
| Global R6 singletons | Per-session instantiation in server() | ~2020 (Shiny best practices) | Prevents session interference |
| Manual if/else for backends | Registry pattern with R6 | Ongoing adoption | Cleaner, more extensible |
| Mock with package mocking tools | Dependency injection | testthat 3.3.0 (2024) added local_mocked_r6_class | Both valid; DI simpler for this case |

**Deprecated/outdated:**
- **S3 for stateful objects:** R6 is now standard for objects with mutable state (since ~2014)
- **R.oo, proto packages:** R6 became the de facto standard for OOP in R ecosystem
- **Global option-based configuration:** Per-session state management is preferred in Shiny

**Current (2026) best practices:**
- R6 for encapsulated objects
- Per-session instantiation
- Duck typing over explicit interfaces
- Dependency injection for testing
- Strategy pattern for pluggable backends

## Open Questions

Things that couldn't be fully resolved:

1. **Should registry be per-session or passed down from AppState?**
   - What we know: Per-session in server() is Shiny best practice
   - What's unclear: Whether AppState should own the registry vs each module creating own
   - Recommendation: Start with per-module registry; if renderer choice needs to persist across modules, move to AppState in Phase 4

2. **How to handle dynamic output type (ggplot vs htmlwidget)?**
   - What we know: Different renderers return different types; renderPlot() vs renderUI()
   - What's unclear: Whether to use dynamic UI or separate outputs
   - Recommendation: Phase 3 uses renderPlot() only (Neuroim2 only). Phase 4 (surfwidget) addresses dynamic output.

3. **Should render() return plot object or be side-effect only?**
   - What we know: plot_brain() returns ggplot; renderPlot() auto-prints
   - What's unclear: Whether to formalize return contract or leave flexible
   - Recommendation: Return plot object (ggplot/htmlwidget); matches current plot_brain() behavior

## Sources

### Primary (HIGH confidence)
- **R6 Package Documentation:** https://r6.r-lib.org/articles/Introduction.html
- **Advanced R - R6 Chapter:** https://adv-r.hadley.nz/r6.html (Hadley Wickham)
- **R6 Interface Discussion:** https://github.com/r-lib/R6/issues/82 (maintainer confirmed no built-in support)
- **testthat Mocking Guide:** https://testthat.r-lib.org/articles/mocking.html (official)
- **Shiny R6 Best Practices:** https://mjfrigaard.github.io/shiny-app-pkgs/r6.html (2025 content)
- **plsrri Codebase:** inst/shiny/R/mod_brain_viewer.R, inst/shiny/R/state.R (verified current usage)

### Secondary (MEDIUM confidence)
- **R6 Interfaces Blog:** https://jakubsobolewski.com/blog/testable-r6-interfaces/ (September 2025)
- **R6 Interfaces R-bloggers:** https://www.r-bloggers.com/2025/09/r6-interfaces-for-backend-define-what-not-how/
- **Clean R Tests Blog:** https://jakubsobolewski.com/blog/clean-tests-with-local_mocked_bindings/ (September 2025)
- **Pass Data Between Modules with R6:** https://jiwanheo.rbind.io/pass-around-data-between-shiny-modules-with-r6/
- **plsrri ARCHITECTURE.md:** .planning/research/ARCHITECTURE.md (prior research, verified patterns)

### Tertiary (LOW confidence)
- None - all findings verified with authoritative sources

## Metadata

**Confidence breakdown:**
- Standard stack: HIGH - R6, Shiny, testthat are industry standard; versions verified on CRAN
- Architecture patterns: HIGH - Patterns verified in official docs, codebase, and 2025/2026 community articles
- Pitfalls: MEDIUM-HIGH - Based on community experience and Shiny best practices documentation
- Code examples: HIGH - Extracted from actual codebase and official documentation

**Research date:** 2026-01-22
**Valid until:** 60 days (stable ecosystem; R6 and Shiny patterns mature)

**Key constraints from CONTEXT.md applied:**
- ✓ Render-only interface (no lifecycle methods)
- ✓ Duck typing for capabilities (no explicit flags)
- ✓ User choice for renderer selection
- ✓ Preserve existing plot_brain() functionality
- ✓ Full refactor of mod_brain_viewer in this phase
