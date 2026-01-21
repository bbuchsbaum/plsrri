# Phase 1: Testing Foundation - Context

**Gathered:** 2026-01-21
**Status:** Ready for planning

<domain>
## Phase Boundary

Establish module testing infrastructure and realistic fixtures for the plsrri Shiny GUI's 6 modules. This includes testServer() tests, mock pls_result objects, and data-test attribute conventions. E2E browser testing belongs in Phase 5.

</domain>

<decisions>
## Implementation Decisions

### Fixture realism
- Structurally correct fixtures — right shape/class/slots, but not necessarily from real analyses
- Include edge cases: single LV, many LVs, missing bootstrap, missing permutation, sparse data
- Storage approach: both .rds files in tests/fixtures/ for complex objects AND helper functions for variations
- Include small but real NeuroVol objects (e.g., 10x10x10 voxels) — not stubs

### Developer workflow
- Run tests via `devtools::test()` only — standard R package testing
- No CI integration for now — local testing only (CI can be added later)
- Standard testthat output on failure — no custom verbosity
- Track test coverage using covr

### Claude's Discretion
- Test file organization (per-module vs per-behavior)
- Specific data-test attribute naming conventions
- Number of test cases per module
- Helper function API design

</decisions>

<specifics>
## Specific Ideas

- Fixtures should have variants: with/without bootstrap, with/without permutation (per roadmap success criteria)
- The 6 modules to test are the existing Shiny modules in the plsrri GUI

</specifics>

<deferred>
## Deferred Ideas

None — discussion stayed within phase scope

</deferred>

---

*Phase: 01-testing-foundation*
*Context gathered: 2026-01-21*
