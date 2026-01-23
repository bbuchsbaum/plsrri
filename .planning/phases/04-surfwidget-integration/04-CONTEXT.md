# Phase 4: surfwidget Integration - Context

**Gathered:** 2026-01-23
**Status:** Ready for planning

<domain>
## Phase Boundary

Users can view PLS results on cortical surfaces via neurosurf surfwidget. This phase implements SurfwidgetRenderer using the existing BrainRenderer abstraction, adds a Surface tab to the Explore view, and ensures proper synchronization with filter bar controls. Custom surface upload and advanced atlas options are out of scope.

</domain>

<decisions>
## Implementation Decisions

### Surface presentation
- Surface view appears as a **separate tab** within Explore (alongside existing volume view)
- **Both hemispheres side-by-side** with ability to hide one hemisphere to view medial surface
- **Multiple surface geometries** available: pial, white, inflated, flat (via selector)
- **Camera controls**: Standard surfwidget mouse rotation/zoom PLUS preset view buttons (lateral/medial/dorsal/ventral)

### Mapping behavior
- **Mapping approach**: Analyze neurosurf vol_to_surf functionality, expose parameters cleanly, choose sensible defaults
- **Missing data (outside mask)**: Render as transparent — show underlying surface only
- **Threshold updates**: Use surfwidget's built-in JS-side threshold controls (fast, no re-mapping)
- **Colormap**: Match volume view colormap — synchronized between views

### View synchronization
- **Update timing**: Active tab only — other tab updates when switched to (performance optimization)
- **Threshold**: Shared single threshold between volume and surface views
- **Cross-view linking**: When switching tabs, vertex/voxel positions should correspond (no real-time cross-linking needed since tabs are separate)
- **Loading state**: Show spinner while surface initializes or updates
- **Research note**: Deep dive into neurosurf surfwidget and underlying JS code needed for proper implementation

### Surface selection
- **Default template**: fsaverage only for now, but architecture should allow swapping templates later
- **Custom surfaces**: Not supported — templates only
- **Selector location**: Dropdown in the surface view tab header (not in filter bar)

### Claude's Discretion
- Exact mapping parameters and defaults (based on neurosurf analysis)
- Error handling approach for vol_to_surf failures
- Loading skeleton/spinner implementation details
- Geometry selector UI design
- Hemisphere toggle mechanism

</decisions>

<specifics>
## Specific Ideas

- surfwidget has built-in threshold controls that update on the JS side — leverage this for performance
- Architecture should anticipate future template swapping even though only fsaverage supported initially
- Research phase needs to investigate neurosurf internals thoroughly

</specifics>

<deferred>
## Deferred Ideas

- Custom surface upload — future enhancement
- Multiple atlas options (fsaverage5, fsaverage6, etc.) — future enhancement
- Real-time cross-view linking between volume and surface — separate feature

</deferred>

---

*Phase: 04-surfwidget-integration*
*Context gathered: 2026-01-23*
