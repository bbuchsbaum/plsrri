# plsrri: Partial Least Squares for Neuroimaging

A modern R implementation of the McIntosh Lab PLS package for
neuroimaging analysis. Supports task PLS, behavior PLS, and multiblock
PLS methods with permutation testing, bootstrap confidence intervals,
and split-half validation.

A modern R implementation of the McIntosh Lab PLS package for
neuroimaging analysis. Supports task PLS, behavior PLS, and multiblock
PLS methods with permutation testing, bootstrap confidence intervals,
and split-half validation. Integrates with neuroim2 for brain imaging
data structures and bidser for BIDS directory access.

## PLS Methods

The package supports six PLS analysis methods:

- Method 1: Mean-Centering Task PLS:

  Identifies patterns of brain activity that optimally distinguish
  experimental conditions. The most common approach for task-based fMRI.

- Method 2: Non-Rotated Task PLS:

  Tests specific hypotheses about condition differences using predefined
  contrasts. Use when you have specific predictions.

- Method 3: Regular Behavior PLS:

  Identifies brain patterns that maximally correlate with behavioral
  measures. For brain-behavior relationship analysis.

- Method 4: Regular Multiblock PLS:

  Combines task effects and behavior-brain correlations in a single
  analysis. Examines both task and behavior simultaneously.

- Method 5: Non-Rotated Behavior PLS:

  Tests specific hypotheses about behavior-brain relationships using
  predefined contrasts.

- Method 6: Non-Rotated Multiblock PLS:

  Tests combined hypotheses about task effects and behavior-brain
  relationships.

## Builder API

The recommended way to run analyses is using the builder API:

    result <- pls_spec(mask = brain_mask) |>
      add_subjects(bids_dir, groups = c("control", "patient")) |>
      add_conditions(events_df) |>
      configure(method = "task", nperm = 1000, nboot = 500) |>
      run()

## Key Functions

- Analysis:

  [`pls_analysis`](https://bbuchsbaum.github.io/plsrri/reference/pls_analysis.md):
  Core analysis function
  [`pls_spec`](https://bbuchsbaum.github.io/plsrri/reference/pls_spec.md):
  Create analysis specification

- Builder API:

  [`add_subjects`](https://bbuchsbaum.github.io/plsrri/reference/add_subjects.md):
  Add subject data
  [`add_conditions`](https://bbuchsbaum.github.io/plsrri/reference/add_conditions.md):
  Specify conditions
  [`add_behavior`](https://bbuchsbaum.github.io/plsrri/reference/add_behavior.md):
  Add behavioral measures
  [`add_design`](https://bbuchsbaum.github.io/plsrri/reference/add_design.md):
  Add design contrasts
  [`configure`](https://bbuchsbaum.github.io/plsrri/reference/configure.md):
  Set analysis parameters
  [`run`](https://bbuchsbaum.github.io/plsrri/reference/run.md): Execute
  analysis

- Results:

  [`salience`](https://bbuchsbaum.github.io/plsrri/reference/salience.md):
  Extract brain loadings
  [`bsr`](https://bbuchsbaum.github.io/plsrri/reference/bsr.md): Extract
  bootstrap ratios
  [`scores`](https://bbuchsbaum.github.io/plsrri/reference/scores.md):
  Extract brain/design scores
  [`significance`](https://bbuchsbaum.github.io/plsrri/reference/significance.md):
  Get permutation p-values
  [`confidence`](https://bbuchsbaum.github.io/plsrri/reference/confidence.md):
  Get bootstrap CIs

- Visualization:

  [`plot_brain`](https://bbuchsbaum.github.io/plsrri/reference/plot_brain.md):
  Brain slice plots
  [`plot_scores`](https://bbuchsbaum.github.io/plsrri/reference/plot_scores.md):
  Score plots
  [`plot_loadings`](https://bbuchsbaum.github.io/plsrri/reference/plot_loadings.md):
  Loading bar plots
  [`render_report`](https://bbuchsbaum.github.io/plsrri/reference/render_report.md):
  Generate Quarto report

## Integration

The package integrates with:

- neuroim2: Imaging data structures (NeuroVol, NeuroVec)

- bidser: BIDS directory access and fMRIPrep derivatives

- neurosurf: Surface visualization (planned)

## Performance

For large-scale analyses (500+ subjects, 10k permutations), the package
provides Rcpp-accelerated versions of key functions. These are used
automatically when available.

## References

McIntosh AR, Lobaugh NJ (2004). Partial least squares analysis of
neuroimaging data: applications and advances. NeuroImage 23: S250-S263.

McIntosh AR, Bookstein FL, Haxby JV, Grady CL (1996). Spatial pattern
analysis of functional brain images using partial least squares.
NeuroImage 3: 143-157.

## Author

**Maintainer**: Brad Buchsbaum <brad.buchsbaum@gmail.com>

Other contributors:

- McIntosh Lab (Original MATLAB implementation) \[copyright holder\]
