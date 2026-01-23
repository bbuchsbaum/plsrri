---
phase: 03-visualization-abstraction
plan: 01
subsystem: visualization
tags: [r6, abstraction, testing, architecture]
date_completed: 2026-01-22
duration_minutes: 1.3

# Dependency graph
requires:
  - 02-01-seed-data-helpers
  - 02-02-filter-bar-extraction
provides:
  - brain-renderer-abstraction
  - renderer-registry-factory
  - mock-renderer-testing
affects:
  - 03-02-integrate-renderer-module
  - phase-04-surfwidget-integration

# Tech tracking
tech_stack:
  added:
    - R6::R6Class (renderer hierarchy)
  patterns:
    - strategy-pattern (pluggable renderers)
    - abstract-base-class (BrainRenderer)
    - factory-pattern (RendererRegistry)

# File tracking
key_files:
  created:
    - inst/shiny/R/fct_brain_renderer.R
    - tests/testthat/test-fct_brain_renderer.R
  modified:
    - tests/testthat/helper-shiny-modules.R
    - inst/shiny/app.R

# Decisions
decisions:
  - id: renderer-abstraction-strategy
    decision: Use R6 abstract base class with concrete implementations
    rationale: Enables pluggable backends (neuroim2 now, surfwidget later) without changing module code
    date: 2026-01-22

  - id: mock-renderer-pattern
    decision: MockBrainRenderer records calls for test assertions
    rationale: Enables unit testing modules without neuroim2 dependency or slow plot_brain()
    date: 2026-01-22

  - id: registry-factory
    decision: RendererRegistry manages renderer instances by name
    rationale: Centralized factory enables runtime renderer selection and validation
    date: 2026-01-22

metrics:
  commits: 3
  tests_added: 36
  tests_passing: 36
  tests_skipped: 1
  files_created: 2
  files_modified: 2
  loc_added: 393
---

# Phase 03 Plan 01: Renderer Abstraction Summary

**One-liner:** R6 abstract BrainRenderer with Neuroim2 implementation, mock for testing, and registry factory

## Objective

Created BrainRenderer R6 class hierarchy with abstract base, Neuroim2 implementation, mock for testing, and registry factory. Establishes pluggable backend architecture for brain visualization, enabling future surfwidget integration and testable module code.

## What Was Built

### 1. BrainRenderer Abstract Base (inst/shiny/R/fct_brain_renderer.R)

**BrainRenderer R6 class:**
- Abstract `render()` method that throws error when called directly
- Signature: `render(result, lv, what, threshold, view, ...)`
- Forces subclasses to implement rendering logic

**Design pattern:**
- Classic abstract base class pattern
- `stop()` with descriptive error message for abstract method
- Documented with roxygen2 @field and @description tags

### 2. Neuroim2Renderer Implementation

**Concrete renderer:**
- Inherits from BrainRenderer
- `render()` delegates directly to `plsrri::plot_brain()`
- Passes through all parameters: `result`, `lv`, `what`, `threshold`, `view`
- Handles extra arguments via `...` (e.g., `along` parameter)
- Returns ggplot object from plot_brain()

**Behavior:**
- Zero logic layer - pure delegation to existing plot_brain()
- Maintains compatibility with existing brain viewer code
- Production renderer for neuroim2 backend

### 3. MockBrainRenderer for Testing

**Test double features:**
- `render_calls` field (list) records all render() invocations
- `initialize()` creates empty render_calls list
- `render()` appends call details (result, lv, what, threshold, view, extra_args)
- Returns minimal ggplot: `ggplot2::ggplot() + ggplot2::ggtitle(paste("Mock LV", lv))`
- `reset_calls()` clears history, returns self for chaining

**Testing benefits:**
- Unit test modules without neuroim2 dependency
- Assert on render() calls without executing slow plot_brain()
- Verify correct parameters passed to renderer

### 4. RendererRegistry Factory

**Factory pattern:**
- `renderers` field (named list) stores registered renderer instances
- `initialize()` automatically registers Neuroim2Renderer as "neuroim2"
- `register(name, renderer)` adds renderer, validates inheritance
- `get(name)` retrieves renderer or stops with helpful error
- `list_available()` returns character vector of registered names

**Validation:**
- `stopifnot(inherits(renderer, "BrainRenderer"))` enforces type safety
- Error message lists available renderers when unknown name requested

### 5. Comprehensive Unit Tests (tests/testthat/test-fct_brain_renderer.R)

**Test coverage (36 tests, 1 skip):**

**BrainRenderer tests (2 tests):**
- Can be instantiated
- render() throws abstract method error with "abstract" in message

**Neuroim2Renderer tests (2 tests):**
- Inherits from BrainRenderer
- render() calls plot_brain with correct parameters (mocked with mockery)

**MockBrainRenderer tests (6 tests):**
- Inherits from BrainRenderer
- Initializes with empty render_calls
- Records render calls (increments list)
- Stores call parameters (result, lv, what, threshold, view, extra_args)
- Returns ggplot object (gg class)
- reset_calls() clears history and returns self

**RendererRegistry tests (10 tests):**
- Initializes with empty registry
- Registers neuroim2 by default
- get() returns registered renderer
- get() errors for unknown renderer with helpful message
- register() adds new renderer
- register() validates BrainRenderer inheritance
- list_available() returns character names
- register() returns self for chaining
- Can register multiple renderers
- All operations work correctly

**Test patterns:**
- describe/it organization (consistent with test-fct_brain_viewer.R)
- System.file() pattern for sourcing in installed vs dev mode
- Mockery used for Neuroim2Renderer delegation test (1 skip if not installed)

### 6. Integration Updates

**helper-shiny-modules.R:**
- Added "fct_brain_renderer.R" to shiny_files vector
- Positioned after fct_seed_data.R, before modules
- Ensures renderer classes available when modules load in tests

**app.R:**
- Added `source(file.path(app_dir, "R", "fct_brain_renderer.R"))`
- Positioned after fct_seed_data.R, before mod_setup.R
- Ensures renderer classes available when app runs

## Deviations from Plan

None - plan executed exactly as written.

## Key Technical Details

### R6 Patterns Used

**Method chaining:**
```r
reset_calls = function() {
  self$render_calls <- list()
  invisible(self)  # Enables: renderer$reset_calls()$render(...)
}
```

**Inheritance:**
```r
Neuroim2Renderer <- R6::R6Class(
  "Neuroim2Renderer",
  inherit = BrainRenderer,  # Establishes inheritance
  ...
)
```

**Abstract method pattern:**
```r
render = function(...) {
  stop("BrainRenderer$render() is abstract - implement in subclass", call. = FALSE)
}
```

### Test Sourcing Pattern

**From test-fct_brain_renderer.R:**
```r
module_file <- system.file("shiny/R/fct_brain_renderer.R", package = "plsrri")
if (module_file == "") {
  module_file <- testthat::test_path("../../inst/shiny/R/fct_brain_renderer.R")
}
source(module_file, local = FALSE)
```

Handles both installed package and development mode.

### MockBrainRenderer Call Recording

**Example:**
```r
mock <- MockBrainRenderer$new()
plot <- mock$render("result", 3, "bsr", 2.5, "ortho", along = 2)

# Assertions:
length(mock$render_calls) == 1
mock$render_calls[[1]]$lv == 3
mock$render_calls[[1]]$extra_args$along == 2
```

## Testing Evidence

**All tests pass:**
```
[ FAIL 0 | WARN 0 | SKIP 1 | PASS 36 ]
```

**Skip:** Mockery package not installed (optional test enhancement)

**Existing tests still pass:**
- test-fct_brain_viewer.R: 101 passing tests
- No regressions from sourcing order changes

## Architecture Benefits

### 1. Pluggable Backends

**Current:**
```r
registry <- RendererRegistry$new()
renderer <- registry$get("neuroim2")  # Uses plot_brain()
```

**Future (Phase 4):**
```r
registry$register("surfwidget", SurfwidgetRenderer$new())
renderer <- registry$get("surfwidget")  # Uses neurosurf/surfwidget
```

Module code unchanged - just swap renderer.

### 2. Testable Modules

**Before (tight coupling):**
```r
# Module directly calls plot_brain() - hard to test without neuroim2
plot <- plsrri::plot_brain(result, lv, what, threshold, view)
```

**After (dependency injection):**
```r
# Module uses renderer - inject mock for testing
plot <- renderer$render(result, lv, what, threshold, view)

# In tests:
mock_renderer <- MockBrainRenderer$new()
# ... run module with mock_renderer ...
expect_equal(length(mock_renderer$render_calls), 1)
```

### 3. Consistent Interface

All renderers implement same signature:
```r
render(result, lv, what, threshold, view, ...)
```

Module code doesn't care which backend - just calls render().

## Next Phase Readiness

### Blockers

None.

### For 03-02 (Integrate into mod_brain_viewer)

**Ready to use:**
- `RendererRegistry$new()` - create registry in module initialize
- `registry$get("neuroim2")` - get default renderer
- `renderer$render(result, lv, what, threshold, view, along = along)` - replace direct plot_brain() call

**Testing pattern:**
- `MockBrainRenderer$new()` - create mock
- Inject into module via initialization parameter
- Assert on render_calls after module operations

### For Phase 4 (surfwidget Integration)

**Extension point ready:**
```r
SurfwidgetRenderer <- R6::R6Class(
  "SurfwidgetRenderer",
  inherit = BrainRenderer,
  public = list(
    render = function(result, lv, what, threshold, view, ...) {
      # Call neurosurf::plot_surface() or surfwidget
      # Return surfwidget HTML widget
    }
  )
)
```

Registry automatically validates inheritance and manages instance.

## Lessons Learned

### 1. R6 Abstract Classes

R doesn't have true abstract classes, but convention works:
- Throw error in abstract method
- Document as abstract in roxygen2
- Subclasses override method

### 2. Factory Pattern in R6

RendererRegistry initialization with defaults works well:
```r
initialize = function() {
  self$renderers <- list()
  self$register("neuroim2", Neuroim2Renderer$new())  # Default available immediately
}
```

### 3. Test Double Recording

Storing full call records (not just counts) enables rich assertions:
```r
call_record <- list(
  result = result,
  lv = lv,
  what = what,
  threshold = threshold,
  view = view,
  extra_args = list(...)
)
```

Can assert on any parameter of any call.

## Files Modified

### Created
- `inst/shiny/R/fct_brain_renderer.R` (173 lines)
- `tests/testthat/test-fct_brain_renderer.R` (220 lines)

### Modified
- `tests/testthat/helper-shiny-modules.R` (+1 line: added fct_brain_renderer.R)
- `inst/shiny/app.R` (+1 line: source fct_brain_renderer.R)

## Commits

1. **f016ae1** - `feat(03-01): create BrainRenderer R6 abstraction`
   - BrainRenderer abstract base class
   - Neuroim2Renderer, MockBrainRenderer implementations
   - RendererRegistry factory

2. **b9d39af** - `test(03-01): add comprehensive unit tests for BrainRenderer`
   - 36 unit tests covering all R6 classes
   - Abstract error, delegation, call recording, registry operations
   - describe/it organization pattern

3. **37406d2** - `feat(03-01): integrate fct_brain_renderer into app sourcing`
   - Updated helper-shiny-modules.R
   - Updated app.R
   - Verified existing tests still pass

## Success Criteria

- [x] fct_brain_renderer.R exists with 4 R6 classes
- [x] BrainRenderer$render() throws abstract error
- [x] Neuroim2Renderer delegates to plot_brain()
- [x] MockBrainRenderer records calls and returns ggplot
- [x] RendererRegistry registers and retrieves renderers
- [x] All unit tests pass (36 passing, 1 skip)
- [x] helper-shiny-modules.R and app.R source the new file
- [x] Existing tests still pass (test-fct_brain_viewer.R: 101 passing)

## Duration

**Total time:** 1.3 minutes

**Breakdown:**
- Task 1 (R6 classes): ~0.5 min
- Task 2 (unit tests): ~0.5 min
- Task 3 (integration): ~0.3 min

**Efficiency factors:**
- Clear R6 pattern from existing state.R
- Straightforward abstract class implementation
- Existing test patterns to follow
- No unexpected issues or blockers
