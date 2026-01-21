# plsrri Shiny GUI Hardening

## What This Is

A quality and extensibility milestone for the plsrri Shiny GUI. The GUI exists but is untested — this work makes it production-ready through comprehensive testing, design refinement, and adding surface-based visualization via neurosurf integration.

## Core Value

The GUI must be reliable and correct before users touch it. Programmatic tests catch bugs; design consistency builds trust; extensible architecture enables iteration.

## Requirements

### Validated

- ✓ Three-step workflow (Setup → Analyze → Explore) — existing
- ✓ R6 AppState for centralized state management — existing
- ✓ Modular architecture (mod_setup, mod_analyze, mod_explore, mod_brain_viewer, mod_filter_bar, mod_inspector) — existing
- ✓ bslib theming with Bootstrap 5 — existing
- ✓ Volume-based brain visualization via plot_brain() — existing
- ✓ Filter bar controls (LV, BSR threshold, p-value) — existing
- ✓ Export functionality (NIfTI, CSV, PDF, HTML report) — existing
- ✓ Package launcher launch_pls_gui() with dependency checking — existing

### Active

- [ ] **TEST-01**: Module-level tests for all Shiny modules using testServer()
- [ ] **TEST-02**: End-to-end tests using shinytest2 for critical user flows
- [ ] **TEST-03**: Test fixtures — mock pls_result objects with realistic structure
- [ ] **TEST-04**: Integration tests verifying plsrri API calls are correct
- [ ] **DESIGN-01**: CSS audit against design-principles (4px grid, semantic color, status dots)
- [ ] **DESIGN-02**: UI component audit (stepper, cards, filter bar, inspector)
- [ ] **DESIGN-03**: Typography and spacing consistency
- [ ] **DESIGN-04**: Error states, empty states, disabled states per design spec
- [ ] **UX-01**: Setup flow review — progressive disclosure, validation feedback
- [ ] **UX-02**: Explore flow review — information hierarchy, interaction patterns
- [ ] **UX-03**: Keyboard navigation and accessibility basics
- [ ] **EXT-01**: Brain viewer abstraction layer for pluggable backends
- [ ] **EXT-02**: neurosurf integration — vol_to_surf() mapping for results
- [ ] **EXT-03**: Surface tab in Explore view using surfwidget htmlwidget
- [ ] **EXT-04**: Synchronized controls between volume and surface views

### Out of Scope

- New analysis methods — this is GUI quality, not plsrri core features
- Mobile responsiveness — desktop-first for neuroimaging workflows
- User authentication — single-user local app
- Internationalization — English only for v1
- Real-time collaboration — not applicable

## Context

**Brownfield:** Building on existing Shiny GUI implementation in `inst/shiny/`. The core plsrri package (builder pattern, analysis engine, plotting) is mature and tested. This milestone focuses on the GUI layer.

**Codebase mapped:** `.planning/codebase/` contains architecture, stack, conventions, and testing documentation from prior analysis.

**Key integration points:**
- `R/gui.R` — package-level launcher
- `inst/shiny/app.R` — Shiny entry point
- `inst/shiny/R/mod_*.R` — modular UI/server components
- `inst/shiny/R/state.R` — R6 AppState class
- `inst/shiny/www/styles.css` — custom CSS
- `~/code/neurosurf` — surface visualization package for integration

**Design reference:** design-principles skill defines the visual language (4px grid, semantic colors, status dots, filter bar pattern, inspector pattern).

## Constraints

- **Stack**: Shiny + bslib + shinyjs — already chosen, no framework changes
- **Testing**: testthat ecosystem (testServer, shinytest2) — R standard
- **Surface viz**: neurosurf package — requires as suggested dependency
- **Compatibility**: Must work with existing plsrri API — no breaking changes to core

## Key Decisions

| Decision | Rationale | Outcome |
|----------|-----------|---------|
| testServer() for module tests | Tests reactive logic without browser overhead | — Pending |
| shinytest2 for E2E tests | Industry standard for Shiny app testing | — Pending |
| neurosurf htmlwidget (not rgl) | Web-native, works in Shiny without X11 | — Pending |
| Surface as additional tab, not replacement | Users need both volume slices and surface views | — Pending |
| Design-principles skill as spec | Consistent visual language across GUI | — Pending |

---
*Last updated: 2026-01-21 after initialization*
