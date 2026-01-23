# Project State

## Project Reference

See: .planning/PROJECT.md (updated 2026-01-21)

**Core value:** The GUI must be reliable and correct before users touch it
**Current focus:** Phase 3 - Visualization Abstraction (Complete)

## Current Position

Phase: 3 of 6 (Visualization Abstraction)
Plan: 2 of 2 in current phase
Status: Phase complete
Last activity: 2026-01-23 - Completed 03-02-PLAN.md

Progress: [█████-----] 44%

## Performance Metrics

**Velocity:**
- Total plans completed: 8
- Average duration: 3.8 min
- Total execution time: 31.3 min

**By Phase:**

| Phase | Plans | Total | Avg/Plan |
|-------|-------|-------|----------|
| 01-testing-foundation | 4 | 18.5 min | 4.6 min |
| 02-business-logic-extraction | 2 | 8.5 min | 4.25 min |
| 03-visualization-abstraction | 2 | 4.3 min | 2.15 min |

**Recent Trend:**
- Last 5 plans: 01-04 (5.1 min), 02-01 (4.1 min), 02-02 (4 min), 03-01 (1.3 min), 03-02 (3 min)
- Trend: Efficient execution maintained

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

### Pending Todos

None.

### Blockers/Concerns

- Phase 4 (surfwidget Integration): Needs research on neurosurf API, WebGL disposal
- Note: bsicons package missing from app dependencies (pre-existing, not blocking tests)

## Session Continuity

Last session: 2026-01-23
Stopped at: Completed 03-02-PLAN.md (Phase 3 complete)
Resume file: None

Related artifacts:
- .planning/phases/03-visualization-abstraction/03-01-SUMMARY.md
- .planning/phases/03-visualization-abstraction/03-02-SUMMARY.md
- inst/shiny/R/fct_brain_renderer.R (4 R6 classes)
- inst/shiny/R/mod_brain_viewer.R (refactored with renderer injection)
- tests/testthat/test-fct_brain_renderer.R (36 tests)
- tests/testthat/test-mod_brain_viewer.R (55 tests total, 15 new renderer tests)
