# Project State

## Project Reference

See: .planning/PROJECT.md (updated 2026-01-21)

**Core value:** The GUI must be reliable and correct before users touch it
**Current focus:** Phase 4 - surfwidget Integration (In progress)

## Current Position

Phase: 4 of 6 (surfwidget Integration)
Plan: 2 of 3 in current phase
Status: In progress
Last activity: 2026-01-23 - Completed 04-02-PLAN.md

Progress: [██████▓---] 62%

## Performance Metrics

**Velocity:**
- Total plans completed: 10
- Average duration: 3.8 min
- Total execution time: 39.3 min

**By Phase:**

| Phase | Plans | Total | Avg/Plan |
|-------|-------|-------|----------|
| 01-testing-foundation | 4 | 18.5 min | 4.6 min |
| 02-business-logic-extraction | 2 | 8.5 min | 4.25 min |
| 03-visualization-abstraction | 2 | 4.3 min | 2.15 min |
| 04-surfwidget-integration | 2 | 8.0 min | 4.0 min |

**Recent Trend:**
- Last 5 plans: 02-02 (4 min), 03-01 (1.3 min), 03-02 (3 min), 04-01 (4.6 min), 04-02 (3.4 min)
- Trend: Consistent execution maintained

*Updated after each plan completion*

## Accumulated Context

### Decisions

Decisions are logged in PROJECT.md Key Decisions table.
Recent decisions affecting current work:

| Date | Phase | Decision | Rationale |
|------|-------|----------|-----------|
| 2026-01-21 | 01-01 | Factory functions use internal set.seed() | Ensures reproducibility when creating test fixtures |
| 2026-01-21 | 01-01 | Default fixtures: 500 voxels, 3 LVs, 30 obs | Matches typical PLS analysis dimensions |
| 2026-01-21 | 01-01 | neuroim2 fallback for masks | Tests run without full neuroim2 dependency |
| 2026-01-21 | 01-02 | data-test naming: module-element pattern | Consistent, predictable selector naming |
| 2026-01-21 | 01-02 | Wrapper div for inputs without direct attribute support | Some Shiny inputs don't accept extra attributes |
| 2026-01-21 | 01-02 | SELECTORS constant for programmatic access | Enables structured access to data-test values |
| 2026-01-21 | 01-03 | Source modules directly in tests | Shiny modules not exported from package |
| 2026-01-21 | 01-03 | Mock results instead of running analysis | Tests use fixtures, avoid slow PLS execution |
| 2026-01-21 | 01-04 | Direct local state modification for parent module tests | Avoids triggering child module observer errors |
| 2026-01-21 | 01-04 | Mock filters list pattern for brain_viewer testing | Enables testing without fully initialized filter_bar |
| 2026-01-21 | 02-01 | Pure functions sourced globally by app.R | Avoids path resolution issues when sourcing inside moduleServer |
| 2026-01-21 | 02-01 | Test helper sources fct_* before modules | Ensures pure functions available when modules are sourced |
| 2026-01-21 | 02-01 | NULL handling before NA checks in validation | Shiny inputs may be NULL when not yet initialized |
| 2026-01-21 | 02-02 | fct_* files sourced by app.R before modules | Functions available when modules load |
| 2026-01-21 | 02-02 | Pure functions take/return plain R objects | Enables unit testing without Shiny context |
| 2026-01-21 | 02-02 | Reactive wrapper pattern for module delegation | Extract values from reactives, call pure functions |
| 2026-01-22 | 03-01 | R6 abstract base class with concrete implementations | Enables pluggable backends (neuroim2 now, surfwidget later) without changing module code |
| 2026-01-22 | 03-01 | MockBrainRenderer records calls for test assertions | Enables unit testing modules without neuroim2 dependency or slow plot_brain() |
| 2026-01-22 | 03-01 | RendererRegistry manages renderer instances by name | Centralized factory enables runtime renderer selection and validation |
| 2026-01-23 | 03-02 | Optional renderer parameter with registry fallback | Production uses registry default; tests inject MockBrainRenderer |
| 2026-01-23 | 03-02 | Per-session registry in moduleServer | Each session gets isolated registry; avoids global state |
| 2026-01-23 | 04-01 | Valid geometries: pial, white, inflated, smoothwm, sphere | Matches neurosurf::load_fsaverage_std8() API |
| 2026-01-23 | 04-01 | Environment-based cache for surfaces | Avoids repeated loading; faster than disk-based cache |
| 2026-01-23 | 04-01 | Mask hash for sampler caching | Fast hash (dims + sum) sufficient to distinguish masks |
| 2026-01-23 | 04-01 | is_widget() method on base class | Enables polymorphic output detection without instanceof checks |
| 2026-01-23 | 04-02 | MockSurfwidgetRenderer for testing | Avoids neurosurf dependency in unit tests while testing renderer integration |
| 2026-01-23 | 04-02 | Fallback plot output for non-widget renderers | Enables testing with MockSurfwidgetRenderer |
| 2026-01-23 | 04-02 | WebGL disposal via parent module dispose() call | Parent knows when tabs switch; child can clean up resources on demand |

### Pending Todos

None.

### Blockers/Concerns

- Note: bsicons package missing from app dependencies (pre-existing, not blocking tests)

## Session Continuity

Last session: 2026-01-23
Stopped at: Completed 04-02-PLAN.md
Resume file: None

Related artifacts:
- .planning/phases/04-surfwidget-integration/04-02-SUMMARY.md
- .planning/phases/04-surfwidget-integration/04-01-SUMMARY.md
- .planning/phases/04-surfwidget-integration/04-RESEARCH.md
- .planning/phases/04-surfwidget-integration/04-CONTEXT.md
- inst/shiny/R/mod_surface_viewer.R (surface_viewer_ui, surface_viewer_server)
- tests/testthat/test-mod_surface_viewer.R (24 tests)
- inst/shiny/R/fct_surface_mapper.R (5 functions)
- inst/shiny/R/fct_brain_renderer.R (6 R6 classes)
