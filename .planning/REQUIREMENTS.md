# Requirements: plsrri Shiny GUI Hardening

**Defined:** 2026-01-21
**Core Value:** The GUI must be reliable and correct before users touch it

## v1 Requirements

Requirements for this milestone. Each maps to roadmap phases.

### Testing

- [ ] **TEST-01**: All 6 Shiny modules have testServer() tests covering reactive logic
- [ ] **TEST-02**: Test fixtures exist with mock pls_result objects (with/without bootstrap, with/without permutation)
- [ ] **TEST-03**: shinytest2 E2E tests cover critical path: Setup → Analyze → Explore

### Architecture

- [ ] **ARCH-01**: Business logic extracted from modules to fct_* files for unit testability
- [ ] **ARCH-02**: BrainRenderer R6 abstract class with Neuroim2Renderer implementation
- [ ] **ARCH-03**: RendererRegistry factory for visualization backend selection

### Surface Visualization

- [ ] **SURF-01**: neurosurf surfwidget integrated as Shiny htmlwidget output
- [ ] **SURF-02**: vol_to_surf() mapping applies to BSR and salience results
- [ ] **SURF-03**: Surface tab added to Explore view for cortical surface display
- [ ] **SURF-04**: Filter bar controls (LV, threshold) synchronized between volume and surface views

### UX / Design

- [ ] **UX-01**: Setup inputs have inline validation with status indicators
- [ ] **UX-02**: Long operations (analysis, export) show loading indicators with progress
- [ ] **UX-03**: CSS and components audited against design-principles (4px grid, semantic colors, status dots)
- [ ] **UX-04**: Errors display user-friendly messages with recovery actions

## v2 Requirements

Deferred to future release. Tracked but not in current roadmap.

### Testing

- **TEST-04**: CI integration for automated test runs on commit
- **TEST-05**: Code coverage reporting via covr with minimum threshold

### UX

- **UX-05**: Keyboard navigation for power users
- **UX-06**: Session state persistence for interrupted analysis

### Visualization

- **SURF-05**: Interactive plot brushing (click brain region to filter scores)

## Out of Scope

Explicitly excluded. Documented to prevent scope creep.

| Feature | Reason |
|---------|--------|
| Visual regression tests (screenshots) | Platform-dependent, brittle, high maintenance |
| Mobile responsiveness | Desktop-first for neuroimaging workflows |
| User authentication | Single-user local app |
| Internationalization | English only for scientific tool |
| Batch analysis mode | Feature expansion, not quality milestone |
| Cluster table with atlas lookup | Feature expansion, defer to v2+ |
| threeBrain integration | surfwidget sufficient for v1, threeBrain adds complexity |

## Traceability

Which phases cover which requirements. Updated during roadmap creation.

| Requirement | Phase | Status |
|-------------|-------|--------|
| TEST-01 | TBD | Pending |
| TEST-02 | TBD | Pending |
| TEST-03 | TBD | Pending |
| ARCH-01 | TBD | Pending |
| ARCH-02 | TBD | Pending |
| ARCH-03 | TBD | Pending |
| SURF-01 | TBD | Pending |
| SURF-02 | TBD | Pending |
| SURF-03 | TBD | Pending |
| SURF-04 | TBD | Pending |
| UX-01 | TBD | Pending |
| UX-02 | TBD | Pending |
| UX-03 | TBD | Pending |
| UX-04 | TBD | Pending |

**Coverage:**
- v1 requirements: 14 total
- Mapped to phases: 0 (pending roadmap)
- Unmapped: 14 ⚠️

---
*Requirements defined: 2026-01-21*
*Last updated: 2026-01-21 after initial definition*
