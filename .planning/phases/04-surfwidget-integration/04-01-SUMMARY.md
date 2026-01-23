# Phase 04 Plan 01: Surface Mapping Infrastructure Summary

## One-liner
Surface mapping infrastructure with vol_to_surf wrappers, sampler caching, and SurfwidgetRenderer R6 class for htmlwidget output.

---

## Metadata

| Field | Value |
|-------|-------|
| Phase | 04-surfwidget-integration |
| Plan | 01 |
| Subsystem | visualization |
| Started | 2026-01-23T15:22:42Z |
| Completed | 2026-01-23T15:27:19Z |
| Duration | 4.6 min |

---

## What Was Done

### Task 1: Surface mapping infrastructure
Created `inst/shiny/R/fct_surface_mapper.R` with pure functions for volume-to-surface mapping:

1. **get_fsaverage_surfaces(geometry)** - Loads fsaverage surfaces from neurosurf with environment-based caching per geometry type. Validates geometry parameter (pial, white, inflated, smoothwm, sphere).

2. **create_surface_sampler(surfaces, mask, hemisphere)** - Creates reusable vol_to_surf sampler for fast repeated mapping. Defaults: knn=6, dthresh=16, sampling="midpoint".

3. **map_volume_to_surface(sampler, volume, fun)** - Applies pre-computed sampler to volume data. Much faster than vol_to_surf() for repeated mapping.

4. **map_result_to_surfaces(result, lv, what, surfaces, samplers)** - High-level function that extracts BSR/salience volume and maps to both hemispheres.

5. **clear_surface_cache()** - Utility to clear cached surfaces for memory management.

Test file `tests/testthat/test-fct_surface_mapper.R` created with 26 tests covering validation, caching, and function delegation.

### Task 2: SurfwidgetRenderer
Extended `inst/shiny/R/fct_brain_renderer.R` with:

1. **BrainRenderer.is_widget()** - Added to base class, returns FALSE by default. Enables polymorphic output type detection.

2. **SurfwidgetRenderer R6 class** - New renderer producing htmlwidgets:
   - Lazy-loads fsaverage surfaces on first render
   - Caches samplers by mask hash for fast re-mapping across LV changes
   - render() returns surfwidget htmlwidget with threshold
   - set_geometry() allows switching surface type

3. **MockSurfwidgetRenderer** - Testing mock that returns htmlwidget-like objects and records calls.

4. **RendererRegistry enhancements**:
   - has_surfwidget() - Checks if neurosurf is available
   - register_surfwidget() - Conditionally registers SurfwidgetRenderer

Test file extended with 38 new tests (74 total) covering is_widget polymorphism and new classes.

### Task 3: Source ordering
- `app.R`: Added fct_surface_mapper.R sourcing after fct_brain_renderer.R
- `helper-shiny-modules.R`: Added fct_surface_mapper.R to shiny_files list

---

## Key Files

### Created
| File | Purpose |
|------|---------|
| inst/shiny/R/fct_surface_mapper.R | Vol_to_surf wrapper functions with sampler caching |
| tests/testthat/test-fct_surface_mapper.R | 26 unit tests for surface mapping |

### Modified
| File | Changes |
|------|---------|
| inst/shiny/R/fct_brain_renderer.R | Added is_widget(), SurfwidgetRenderer, MockSurfwidgetRenderer, registry methods |
| tests/testthat/test-fct_brain_renderer.R | Added 38 new tests (74 total) |
| inst/shiny/app.R | Added fct_surface_mapper.R source line |
| tests/testthat/helper-shiny-modules.R | Added fct_surface_mapper.R to shiny_files |

---

## Decisions Made

| Decision | Rationale |
|----------|-----------|
| Valid geometries: pial, white, inflated, smoothwm, sphere | Matches neurosurf::load_fsaverage_std8() API (not "flat") |
| Environment-based cache for surfaces | Avoids repeated loading; faster than disk-based cache |
| Mask hash for sampler caching | Fast hash (dims + sum) sufficient to distinguish masks |
| is_widget() method on base class | Enables polymorphic output detection without instanceof checks |
| MockSurfwidgetRenderer returns htmlwidget-like object | Enables testing surface view module without neurosurf dependency |

---

## Deviations from Plan

### Auto-fixed Issues

**1. [Rule 1 - Bug] Fixed invalid "flat" geometry**
- **Found during:** Task 1 test execution
- **Issue:** Plan specified "flat" as valid geometry, but neurosurf only supports smoothwm/sphere/pial/white/inflated
- **Fix:** Changed valid_geometries to match neurosurf API
- **Files modified:** fct_surface_mapper.R, test-fct_surface_mapper.R
- **Commit:** a8d976c

---

## Test Results

| Test File | Pass | Skip | Fail |
|-----------|------|------|------|
| test-fct_surface_mapper.R | 22 | 11 | 0 |
| test-fct_brain_renderer.R | 74 | 1 | 0 |
| All fct_* tests | 236 | 12 | 0 |

Skips are expected when mockery not installed or for inverse-condition tests.

---

## Commits

| Hash | Type | Description |
|------|------|-------------|
| a8d976c | feat | Add surface mapping infrastructure |
| 1118600 | feat | Add SurfwidgetRenderer to brain renderer abstraction |
| 835b4aa | chore | Add fct_surface_mapper.R to app and test sourcing |

---

## Dependencies

### Provides
- Surface mapping pipeline for PLS results
- SurfwidgetRenderer for htmlwidget output
- MockSurfwidgetRenderer for testing
- is_widget() polymorphism for renderer type detection

### Requires
- neurosurf package (optional - checked at runtime)
- BrainRenderer abstraction (Phase 3)
- plsrri::bsr() and plsrri::salience() with as_neurovol option

### Affects
- Phase 04-02: Surface view module (will use SurfwidgetRenderer)
- Phase 04-03: UI integration (will need tab switching)

---

## Next Phase Readiness

Ready for 04-02: mod_surface_viewer.R implementation
- SurfwidgetRenderer is fully functional
- Surface mapping pipeline tested
- Mock renderer available for testing
