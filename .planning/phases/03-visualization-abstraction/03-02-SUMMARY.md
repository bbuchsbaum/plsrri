---
phase: "03"
plan: "02"
title: "Renderer Integration"
subsystem: "visualization"
tags: [renderer, dependency-injection, testing, R6, brain-viewer]

dependency_graph:
  requires:
    - "03-01"  # BrainRenderer R6 classes and RendererRegistry
  provides:
    - "mod_brain_viewer.R uses renderer abstraction"
    - "Dependency injection for renderer in module servers"
    - "MockBrainRenderer integration tests"
  affects:
    - "04-*"  # SurfWidget integration (future renderer implementation)

tech_stack:
  added: []  # No new dependencies
  patterns:
    - "Dependency injection via function parameters"
    - "Registry pattern for default renderer selection"
    - "Mock object testing with R6 classes"

key_files:
  created: []
  modified:
    - inst/shiny/R/mod_brain_viewer.R
    - tests/testthat/test-mod_brain_viewer.R

decisions:
  - id: "03-02-01"
    topic: "Renderer injection pattern"
    choice: "Optional renderer parameter with registry fallback"
    rationale: "Production uses registry default; tests inject MockBrainRenderer"
  - id: "03-02-02"
    topic: "Per-session registry"
    choice: "Create RendererRegistry inside moduleServer when no renderer injected"
    rationale: "Each session gets isolated registry; avoids global state"

metrics:
  duration: "2 min 57 sec"
  completed: "2026-01-23"
---

# Phase 03 Plan 02: Renderer Integration Summary

**One-liner:** Refactored brain_viewer and brain_mini modules to use BrainRenderer abstraction via dependency injection, enabling testable visualization with MockBrainRenderer.

## What Was Built

### Module Refactoring (mod_brain_viewer.R)

1. **brain_viewer_server()**
   - Added `renderer = NULL` parameter for dependency injection
   - Creates per-session `RendererRegistry` when no renderer injected
   - Uses `active_renderer$render()` instead of direct `plsrri::plot_brain()` call
   - Passes all filter values (lv, what, threshold, view, along, lag) to renderer

2. **brain_mini_server()**
   - Added `renderer = NULL` parameter for dependency injection
   - Creates per-session `RendererRegistry` when no renderer injected
   - Uses `active_renderer$render()` instead of direct `plsrri::plot_brain()` call
   - Passes fixed values (what="bsr", threshold=3, view="montage", ncol=3)

3. **Preserved functionality**
   - View mode toggle (montage/ortho buttons)
   - Axis selection (axial/coronal/sagittal)
   - Dynamic plot height
   - Colorbar info
   - Click/hover handling

### Test Updates (test-mod_brain_viewer.R)

Added 15 new renderer integration tests:

**brain_viewer_server renderer integration (11 tests):**
- Calls renderer with filter values
- Passes updated filter values to renderer
- Handles NULL result gracefully - renderer not called
- Handles result without mask - renderer not called
- Passes view mode to renderer
- Passes axis along to renderer
- Passes lag to renderer

**brain_mini_server renderer integration (4 tests):**
- Calls renderer with LV value
- Handles NULL result gracefully - renderer not called
- Handles result without mask - renderer not called
- Passes ncol to renderer

### Key Patterns Established

```r
# Dependency injection pattern
brain_viewer_server <- function(id, result_rv, filters, renderer = NULL) {
  moduleServer(id, function(input, output, session) {
    # Per-session registry when no renderer injected
    registry <- if (is.null(renderer)) {
      RendererRegistry$new()
    } else {
      NULL
    }
    active_renderer <- renderer %||% registry$get("neuroim2")

    # Use renderer abstraction
    p <- active_renderer$render(
      result = result,
      lv = lv,
      what = what,
      threshold = threshold,
      view = view,
      ...
    )
  })
}

# Test pattern with MockBrainRenderer
mock_renderer <- MockBrainRenderer$new()
testServer(brain_viewer_server, {
  session$setInputs(axis = "3")
  session$flushReact()

  expect_gte(length(mock_renderer$render_calls), 1)
  last_call <- mock_renderer$render_calls[[length(mock_renderer$render_calls)]]
  expect_equal(last_call$lv, 1L)
}, args = list(
  result_rv = reactive(mock_result),
  filters = list(...),
  renderer = mock_renderer
))
```

## Commits

| Hash | Type | Description |
|------|------|-------------|
| 53614c3 | feat | Refactor brain viewer to use renderer abstraction |
| 8698218 | test | Add renderer integration tests using MockBrainRenderer |

## Test Results

```
[ FAIL 0 | WARN 21 | SKIP 4 | PASS 1662 ]
```

- All existing tests pass
- 40 brain_viewer tests pass (11 skipped when plsrri not fully installed)
- 15 new renderer integration tests added
- Full test suite: 1662 passing

## Deviations from Plan

None - plan executed exactly as written.

## Dependencies Verified

- fct_brain_renderer.R sourced by helper-shiny-modules.R
- MockBrainRenderer available in test environment
- RendererRegistry creates Neuroim2Renderer by default
- No circular dependencies introduced

## Next Steps

Phase 03 visualization abstraction complete. Ready for:
- Phase 04: SurfWidget integration (implement SurfWidgetRenderer as new backend)
- Phase 05: Result management
- Phase 06: Enhanced interactions
