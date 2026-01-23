# Phase 4: surfwidget Integration - Research

**Researched:** 2026-01-23
**Domain:** neurosurf surfwidget, vol_to_surf mapping, Shiny htmlwidget integration
**Confidence:** HIGH

## Summary

This research investigates the integration of neurosurf's surfwidget into the plsrri Shiny GUI for cortical surface visualization of PLS results. The neurosurf package provides a complete solution with three key components: (1) surfwidget htmlwidget for WebGL-based brain surface rendering, (2) vol_to_surf() for volume-to-surface mapping, and (3) pre-loaded fsaverage surfaces.

The existing BrainRenderer abstraction from Phase 3 provides the architectural foundation for adding a SurfwidgetRenderer. The key challenge is adapting the interface since surfwidget is an htmlwidget (not ggplot), requiring different render patterns. surfwidget has built-in JS-side threshold controls that enable fast threshold updates without re-rendering from R.

**Primary recommendation:** Implement SurfwidgetRenderer as a new renderer type that returns surfwidget htmlwidgets, add surface mapping infrastructure using vol_to_surf() with pre-computed samplers, and integrate into a new Surface tab in the Explore view using tabsetPanel.

## Standard Stack

The established libraries/tools for this domain:

### Core
| Library | Version | Purpose | Why Standard |
|---------|---------|---------|--------------|
| neurosurf | current | Surface visualization and vol_to_surf mapping | Package provides surfwidget, vol_to_surf, and fsaverage surfaces in one solution |
| htmlwidgets | 1.6.x | Widget infrastructure | R standard for interactive widgets in Shiny |
| shiny | 1.8.x | Web application framework | Already in use |

### Supporting
| Library | Version | Purpose | When to Use |
|---------|---------|---------|-------------|
| FNN | current | Fast nearest-neighbor for vol_to_surf | Internal to neurosurf, used in mapping |
| neuroim2 | current | NeuroVol for BSR/salience volumes | Already used in plsrri |

### Alternatives Considered
| Instead of | Could Use | Tradeoff |
|------------|-----------|----------|
| surfwidget | rgl | rgl requires X11/WebGL server-side; surfwidget is pure htmlwidget |
| pre-computed sampler | on-demand vol_to_surf | Sampler is 10-100x faster for threshold changes |

**Installation:**
```r
# neurosurf already suggested in plsrri DESCRIPTION
# No new dependencies required
```

## Architecture Patterns

### Recommended Project Structure
```
inst/shiny/R/
├── fct_brain_renderer.R       # Add SurfwidgetRenderer class
├── fct_surface_mapper.R       # NEW: vol_to_surf wrapper functions
├── mod_surface_viewer.R       # NEW: Surface tab module
├── mod_brain_viewer.R         # Existing volume viewer
└── mod_explore.R              # Add tabsetPanel for Volume/Surface tabs
```

### Pattern 1: SurfwidgetRenderer as BrainRenderer Subclass
**What:** R6 class extending BrainRenderer that produces surfwidget htmlwidgets
**When to use:** Rendering PLS results on cortical surfaces
**Example:**
```r
# Source: neurosurf/R/surfwidget.R confirmed API
SurfwidgetRenderer <- R6::R6Class(
  "SurfwidgetRenderer",
  inherit = BrainRenderer,
  public = list(
    surfaces = NULL,  # Cached fsaverage surfaces
    sampler_lh = NULL,
    sampler_rh = NULL,

    initialize = function() {
      # Load fsaverage surfaces once
      self$surfaces <- neurosurf::load_fsaverage_std8("inflated")
    },

    # Returns surfwidget, not ggplot
    render = function(result, lv, what, threshold, ...) {
      # Get BSR or salience volume
      vol <- if (what == "bsr") {
        plsrri::bsr(result, lv = lv, as_neurovol = TRUE)
      } else {
        plsrri::salience(result, lv = lv, as_neurovol = TRUE)
      }

      # Map to surface (both hemispheres)
      surf_lh <- neurosurf::vol_to_surf(
        self$surfaces$lh, self$surfaces$lh,
        vol, mask = result$mask, fun = "avg"
      )

      # Create surfwidget with threshold
      neurosurf::surfwidget(
        surf_lh,
        thresh = c(-threshold, threshold),
        cmap = diverging_colormap()
      )
    }
  )
)
```

### Pattern 2: Lazy Tab Updating
**What:** Only update the visible tab; defer other tab updates until tab switch
**When to use:** Performance optimization for multi-tab views
**Example:**
```r
# Track active tab
active_tab <- reactiveVal("volume")

# Only render surface when tab is active
output$surface_plot <- renderSurfwidget({
  req(active_tab() == "surface")
  # ... expensive surface rendering
})

# Invalidate on tab switch
observeEvent(input$view_tabs, {
  active_tab(input$view_tabs)
})
```

### Pattern 3: Surface Sampler Caching
**What:** Pre-compute vol_to_surf sampler for reuse across LV/threshold changes
**When to use:** Repeated mapping of same volume space to same surface
**Example:**
```r
# Source: neurosurf/R/vol_to_surf.R confirmed API
# Build sampler once per result (same mask/space)
sampler <- neurosurf::surface_sampler(
  surf_wm = surfaces$lh,
  surf_pial = surfaces$lh,  # Use same for inflated
  vol_template = result$mask,
  mask = result$mask,
  knn = 6,
  dthresh = 16
)

# Apply sampler repeatedly for different volumes/LVs
mapped <- neurosurf::apply_surface_sampler(sampler, bsr_vol, fun = "avg")
```

### Anti-Patterns to Avoid
- **Calling vol_to_surf per threshold change:** JS-side threshold updates are much faster
- **Global surface cache:** Use per-session caching to avoid cross-session leaks
- **Blocking initial render:** Load fsaverage asynchronously or show loading spinner

## Don't Hand-Roll

Problems that look simple but have existing solutions:

| Problem | Don't Build | Use Instead | Why |
|---------|-------------|-------------|-----|
| Volume-to-surface mapping | Custom interpolation | neurosurf::vol_to_surf() | Handles coordinate transforms, nearest-neighbor, Gaussian weighting |
| fsaverage surfaces | Download/bundle FreeSurfer | neurosurf::load_fsaverage_std8() | Bundled in package, std.8 decimation for performance |
| WebGL surface rendering | Custom Three.js | neurosurf::surfwidget | Complete solution with colorbar, threshold, controls |
| Threshold slider | Custom numericInput | surfwidget JS controls | JS-side updates without R roundtrip |
| WebGL disposal | Manual cleanup | surfwidget$destroy() | surfwidget.js handles full cleanup chain |

**Key insight:** neurosurf provides a complete, tested surface visualization stack. The main work is adapting the interface to fit the existing BrainRenderer abstraction and integrating into the Shiny UI structure.

## Common Pitfalls

### Pitfall 1: Coordinate Space Mismatch
**What goes wrong:** vol_to_surf maps voxels in volume space to surface vertices in surface space; mismatch produces garbage
**Why it happens:** The plsrri mask/results use a specific NeuroSpace; fsaverage surfaces are in FreeSurfer space
**How to avoid:** vol_to_surf() handles coordinate transformation via surf_to_world() transform; ensure mask is passed correctly
**Warning signs:** All-zero or NaN values on surface; values don't match expected anatomical patterns

### Pitfall 2: WebGL Context Limits
**What goes wrong:** Browser has limited WebGL contexts (typically 8-16); creating too many causes "WebGL context lost"
**Why it happens:** Each surfwidget creates a WebGL context; tab switching or repeated renders accumulate contexts
**How to avoid:** Call surfwidget's destroy() method when switching away; use single surfwidget instance per view
**Warning signs:** Black widget, console "WebGL context lost" errors, browser performance degradation

### Pitfall 3: Slow Initial Render
**What goes wrong:** First surface view takes 2-5 seconds, poor UX
**Why it happens:** Loading fsaverage surfaces and computing vol_to_surf sampler is expensive
**How to avoid:** Show loading spinner; consider pre-computing sampler when result loads (not on tab switch)
**Warning signs:** UI freezes when switching to Surface tab

### Pitfall 4: Threshold Update Latency
**What goes wrong:** Threshold slider feels laggy; surface doesn't update smoothly
**Why it happens:** Each threshold change triggers full R re-render + vol_to_surf recomputation
**How to avoid:** Use surfwidget's JS-side setThreshold(); don't re-render from R
**Warning signs:** Slider drags with delay; network traffic on threshold changes

### Pitfall 5: Memory Leaks from Widget Accumulation
**What goes wrong:** Memory usage grows unbounded over time
**Why it happens:** htmlwidgets aren't automatically disposed on Shiny output overwrite
**How to avoid:** Explicit destroy() call before creating new widget; monitor with browser dev tools
**Warning signs:** Browser memory grows, eventual slowdown or crash

## Code Examples

Verified patterns from official sources:

### surfwidget Creation with Shiny Bindings
```r
# Source: neurosurf/R/surfwidget.R lines 381-390
# UI
surfwidgetOutput(ns("surface_view"), width = "100%", height = "400px")

# Server
output$surface_view <- renderSurfwidget({
  surf <- vol_to_surf(...)
  surfwidget(surf,
             cmap = colorRampPalette(c("blue", "white", "red"))(256),
             irange = c(-5, 5),
             thresh = c(-3, 3),
             colorbar = TRUE,
             colorbar_label = "BSR")
})
```

### vol_to_surf with All Parameters
```r
# Source: neurosurf/R/vol_to_surf.R lines 186-193
mapped_surf <- vol_to_surf(
  surf_wm = surfaces$lh,       # White matter surface
  surf_pial = surfaces$lh,     # Pial surface (or same as wm for inflated)
  vol = bsr_vol,               # NeuroVol from bsr(result, as_neurovol=TRUE)
  mask = result$mask,          # PLS mask
  fun = "avg",                 # Averaging function: "avg", "nn", "mode"
  knn = 6,                     # Number of neighbors
  sigma = 8,                   # Gaussian kernel bandwidth
  dthresh = 16,                # Distance threshold
  fill = 0                     # Value for missing data
)
```

### JS-Side Threshold Update via Shiny
```r
# Source: neurosurf/R/surfwidget.R lines 408-411
# Update threshold without re-rendering entire widget
updateSurfwidgetConfig(session, ns("surface_view"), list(
  threshold = c(-new_threshold, new_threshold)
))
```

### Widget Disposal on Tab Switch
```js
// Source: neurosurf/inst/htmlwidgets/surfwidget.js line 444-452
// Called automatically by htmlwidgets on output clear
destroy: function() {
  if (viewer && viewer.dispose) {
    viewer.dispose();  // Full WebGL cleanup
  }
  if (viewerContainer) {
    viewerContainer.innerHTML = '';
  }
  viewer = null;
}
```

### Loading fsaverage Surfaces
```r
# Source: neurosurf/R/fetch_surfaces.R lines 21-37
# Load once per session, cache in renderer
surfaces <- neurosurf::load_fsaverage_std8("inflated")
# Returns list(lh = SurfaceGeometry, rh = SurfaceGeometry)
```

### Surface Sampler for Repeated Mapping
```r
# Source: neurosurf/R/vol_to_surf.R lines 343-430
# Build sampler once
sampler <- surface_sampler(
  surf_wm = surfaces$lh,
  surf_pial = surfaces$lh,
  vol_template = result$mask,
  mask = result$mask,
  sampling = "midpoint",
  knn = 6,
  dthresh = 16
)

# Apply to different volumes efficiently
surf_bsr <- apply_surface_sampler(sampler, bsr_vol, fun = "avg")
surf_sal <- apply_surface_sampler(sampler, salience_vol, fun = "avg")
```

## State of the Art

| Old Approach | Current Approach | When Changed | Impact |
|--------------|------------------|--------------|--------|
| rgl for R surface viz | htmlwidgets (surfwidget) | 2023+ | Works in browser without X11 |
| Manual FreeSurfer setup | Bundled fsaverage | neurosurf 2.x | No user setup required |
| Full re-render on threshold | JS-side threshold update | surfviewjs | 10-100x faster threshold changes |

**Deprecated/outdated:**
- Direct rgl:: calls: Use neurosurf::surfwidget or neurosurf::show_surface_plot
- Manual surface loading: Use load_fsaverage_std8()

## surfwidget JS API Reference

The surfwidget htmlwidget exposes methods for client-side updates:

| Method | Purpose | Parameters |
|--------|---------|------------|
| setThreshold(min, max) | Update display threshold | Two floats |
| setIRange(min, max) | Update intensity range | Two floats |
| setColorMap(colors) | Update colormap | Array of hex colors |
| updateConfig(config) | Update multiple settings | Config object |
| destroy() | Dispose WebGL resources | None |

These are invoked via:
```r
# From R via session$sendCustomMessage
updateSurfwidgetConfig(session, "widget_id", config)
```

Or via htmlwidget method chaining in R:
```r
surfwidget(surf) %>%
  updateThreshold(-3, 3) %>%
  updateIRange(-5, 5)
```

## View Synchronization Strategy

Based on CONTEXT.md decisions:

1. **Active tab only**: Render only the visible tab; invalidate other tab on data change
2. **Shared threshold**: Single BSR threshold input controls both views
3. **Lazy update**: Surface view updates when tab becomes active, not on every threshold change
4. **Loading state**: Show spinner while surface mapping computes

```r
# Pseudo-code for synchronization
observe({
  # When threshold changes and surface tab is visible
  if (active_tab() == "surface") {
    # Use JS-side threshold update (fast)
    updateSurfwidgetConfig(session, "surface", list(
      threshold = c(-filters$bsr_threshold(), filters$bsr_threshold())
    ))
  } else {
    # Mark surface as stale; will re-render on tab switch
    surface_stale(TRUE)
  }
})

observeEvent(input$view_tabs, {
  if (input$view_tabs == "surface" && surface_stale()) {
    # Re-render surface with current threshold
    surface_stale(FALSE)
    # ... trigger renderSurfwidget
  }
})
```

## BrainRenderer Interface Considerations

The existing BrainRenderer interface returns ggplot objects. SurfwidgetRenderer returns htmlwidgets. Two options:

### Option A: Separate render method
Add a `render_widget()` method for htmlwidget renderers:
```r
BrainRenderer <- R6::R6Class(
  "BrainRenderer",
  public = list(
    render = function(...) stop("abstract"),
    render_widget = function(...) NULL,  # Optional
    is_widget = function() FALSE
  )
)

SurfwidgetRenderer <- R6::R6Class(
  inherit = BrainRenderer,
  public = list(
    is_widget = function() TRUE,
    render_widget = function(result, lv, what, threshold, ...) {
      # Return surfwidget
    }
  )
)
```

### Option B: Polymorphic render (Recommended)
Let render() return different types based on renderer:
```r
# Volume renderer returns ggplot
output$brain_plot <- renderPlot({
  if (renderer$is_widget()) return(NULL)
  renderer$render(...)
})

# Surface renderer returns surfwidget
output$surface_plot <- renderSurfwidget({
  if (!renderer$is_widget()) return(NULL)
  renderer$render(...)
})
```

**Recommendation:** Option B is cleaner. The module should know which output type to use based on the renderer, not require different method calls.

## Open Questions

Things that couldn't be fully resolved:

1. **Dual hemisphere layout**
   - What we know: surfwidget renders single surface; need two side-by-side for both hemispheres
   - What's unclear: Best layout approach (two widgets vs. combined view)
   - Recommendation: Use two surfwidgetOutput in a flexbox div; simpler than modifying surfwidget internals

2. **Preset camera views (lateral/medial/etc.)**
   - What we know: NeuroSurfaceViewer has viewpoints object with predefined matrices
   - What's unclear: How to trigger from R (no setViewpoint() exposed in R bindings)
   - Recommendation: Add setViewpoint to surfwidget.js methods; invoke via custom message

3. **Surface geometry selector (pial/white/inflated/flat)**
   - What we know: load_fsaverage_std8() accepts surf parameter
   - What's unclear: Whether switching geometry requires full re-render or can update in place
   - Recommendation: Full re-render on geometry change (infrequent operation)

## Sources

### Primary (HIGH confidence)
- neurosurf/R/surfwidget.R - surfwidget API, Shiny bindings
- neurosurf/R/vol_to_surf.R - vol_to_surf() function, surface_sampler
- neurosurf/R/fetch_surfaces.R - load_fsaverage_std8()
- neurosurf/inst/htmlwidgets/surfwidget.js - JS widget implementation
- neurosurf/inst/htmlwidgets/neurosurface/src/NeuroSurfaceViewer.js - Three.js viewer, dispose()
- plsrri/inst/shiny/R/fct_brain_renderer.R - BrainRenderer abstraction

### Secondary (MEDIUM confidence)
- plsrri/R/accessors.R - bsr(), salience() with as_neurovol option
- neurosurf/tests/testthat/test_vol_to_surf.R - vol_to_surf test patterns

### Tertiary (LOW confidence)
- None - all findings verified with source code

## Metadata

**Confidence breakdown:**
- Standard stack: HIGH - Direct source code inspection of neurosurf
- Architecture: HIGH - Builds on verified Phase 3 BrainRenderer abstraction
- Pitfalls: HIGH - Based on WebGL/htmlwidget patterns and source code analysis
- vol_to_surf API: HIGH - Verified from neurosurf/R/vol_to_surf.R
- JS threshold updates: HIGH - Verified from surfwidget.js and NeuroSurfaceViewer.js

**Research date:** 2026-01-23
**Valid until:** 60 days (neurosurf is stable; local package)
